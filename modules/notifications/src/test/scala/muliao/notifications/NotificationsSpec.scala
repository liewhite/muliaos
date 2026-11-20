package muliao.notifications

import muliao.contract.*
import muliao.notifications.push.*
import muliao.notifications.store.Repo
import muliao.platform.{AppError, Retry}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.platform.server.{Auth, HttpServer, ServerSettings}
import muliao.sessions.*
import muliao.sessions.TestEnv.*
import zio.*
import zio.json.*
import zio.test.*

import java.nio.charset.StandardCharsets

/** 通知收件箱、推送投递与重试、会话事件转通知、notify_user、HTTP 接口：真的数据库与会话领域，假的模型与推送服务。 */
object NotificationsSpec extends ZIOSpecDefault:

  // 一台合法设备的密钥（RFC 8291 的例子）
  private val p256dh = "BCVxsr7N_eNgVRqvHtD0zTZsEc6-VV-JvLexhqUzORcxaOzi6-AYWXvTBHm4bjyPjs7Vd8pZGH6SRpkNtoIAiw4"
  private val auth   = "BTBZMqHH6r4Tts7J_aSIgg"

  private def sub(name: String) = PushSubscriptionIn(s"https://push.example.net/$name", PushKeys(p256dh, auth))

  /** 假推送服务：按 endpoint 依次给出脚本里的回应（用完后重复最后一个，没脚本就成功），记下每次尝试。 */
  final class FakeGateway(scripts: Ref[Map[String, List[Either[PushFailure, Unit]]]], val tries: Ref[Chunk[(String, String)]]) extends PushGateway:
    def send(device: Device, payload: Array[Byte]): IO[PushFailure, Unit] =
      val ep = device.endpoint.toString
      tries.update(_ :+ (ep -> String(payload, StandardCharsets.UTF_8))) *>
        scripts
          .modify { m =>
            m.getOrElse(ep, Nil) match
              case Nil          => (Right(()), m)
              case last :: Nil  => (last, m)
              case next :: rest => (next, m.updated(ep, rest))
          }
          .flatMap(ZIO.fromEither(_))

    def script(name: String, rs: Either[PushFailure, Unit]*): UIO[Unit] = scripts.update(_.updated(sub(name).endpoint, rs.toList))
    def triesOf(name: String): UIO[Chunk[String]] = tries.get.map(_.collect { case (e, p) if e == sub(name).endpoint => p })

  private val quick = Retry.Policy(10.millis, 40.millis, 400.millis)

  private val transient = Left(PushFailure.Transient("断网"))
  private val ok        = Right(())

  type Env = TestEnv.Env & Notifications

  /** 起一套会话 + 通知环境，接好通知并启动会话领域。 */
  private def fresh[A](script: muliao.models.llm.ModelRequest => Script)(body: (FakeGateway, FakeModels) => ZIO[Env & Scope, AppError, A]): ZIO[Scope, AppError, A] =
    for
      d   <- dirs
      m   <- fakeModels(script)
      gw  <- Ref.make(Map.empty[String, List[Either[PushFailure, Unit]]]).zip(Ref.make(Chunk.empty[(String, String)])).map(FakeGateway(_, _))
      env <- layer(d, m, more = List(muliao.notifications.Module.migrations), moreTools = SessionLink.specs).build
      n   <- Notifications.make(env.get[Database], env.get[ChangeBus], gw, "PUBLIC", quick)
      all  = env.add(n)
      p   <- probe
      a <- (for
             c <- SessionLink.contribution
             u <- SessionLink.unread
             _ <- start(p, List(c), unread = u)
             r <- body(gw, m)
           yield r).provideSomeEnvironment[Scope](_ ++ all)
    yield a

  private def notifications = ZIO.service[Notifications]
  private def inbox         = notifications.flatMap(_.inbox(100))

  private def payloads(gw: FakeGateway, name: String): UIO[List[Notifications.Payload]] =
    gw.triesOf(name).map(_.toList.flatMap(_.fromJson[Notifications.Payload].toOption))

  def spec = suite("通知与推送")(
    test("事务里发通知：回滚则不发；提交后推到每台设备，载荷带点开地址与未读角标") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("嗯"))))((gw, _) =>
        for
          n       <- notifications
          _       <- n.subscribe(sub("a")) *> n.subscribe(sub("b"))
          rolled  <- Database.write(n.notifyIn(Notice("回滚", "x", Target.Inbox)) *> ZIO.fail(AppError.conflict("不要了"))).either
          _       <- Clock.sleep(100.millis)
          before  <- gw.tries.get
          _       <- Database.write(n.notifyIn(Notice("请审核 #7", "做完了", Target.Page("/issues/7"))))
          a       <- eventually("推到 a")(payloads(gw, "a"))(_.nonEmpty)
          b       <- eventually("推到 b")(payloads(gw, "b"))(_.nonEmpty)
          box     <- inbox
        yield assertTrue(
          rolled.isLeft,
          before.isEmpty,
          a == List(Notifications.Payload("请审核 #7", "做完了", "/issues/7", 1)),
          b == a,
          box.items.map(_.title) == List("请审核 #7"),
          box.unread == 1L
        )
      ))
    },
    test("每台设备独立投递：暂时失败退避后送达；410 删订阅；拒收不重试不删；一直失败到期放弃") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("嗯"))))((gw, _) =>
        for
          n     <- notifications
          _     <- ZIO.foreachDiscard(List("flaky", "gone", "bad", "down"))(x => n.subscribe(sub(x)))
          _     <- gw.script("flaky", transient, transient, ok)
          _     <- gw.script("gone", Left(PushFailure.Gone(410)))
          _     <- gw.script("bad", Left(PushFailure.Rejected("JWT 不对")))
          _     <- gw.script("down", transient)
          _     <- n.sendTest
          flaky <- eventually("flaky 送达")(gw.triesOf("flaky"))(_.size == 3)
          subs  <- eventually("gone 被删")(Database.read(Repo.endpoints))(!_.contains(sub("gone").endpoint))
          _     <- Clock.sleep(800.millis) // 超过放弃时限
          down1 <- gw.triesOf("down")
          _     <- Clock.sleep(200.millis)
          down2 <- gw.triesOf("down")
          bad   <- gw.triesOf("bad")
          test  <- payloads(gw, "flaky")
        yield assertTrue(
          flaky.size == 3,
          subs.toSet == Set("flaky", "bad", "down").map(sub(_).endpoint),
          bad.size == 1,
          down1.size > 2,
          down2.size == down1.size,
          test.head == Notifications.Payload("Muliao", "推送通知已开启。", "/notifications", 1)
        )
      ))
    },
    test("已读：通知页标到某条为止、打开页面即已读、会话未读；会话删了通知随之删除；重新订阅同一设备只算一台") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("嗯"))))((gw, _) =>
        val (s1, s2) = (SessionId.wrap("s1"), SessionId.wrap("s2"))
        for
          n    <- notifications
          a    <- n.notify(Notice("a", "", Target.Session(s1)))
          _    <- n.notify(Notice("b", "", Target.Session(s2)))
          _    <- n.notify(Notice("c", "", Target.Page("/issues/3")))
          u0   <- n.unreadSessions(List(s1, s2, SessionId.wrap("s3")))
          _    <- n.readThrough(a)
          u1   <- n.unreadSessions(List(s1, s2))
          _    <- n.readPage("/sessions/s2")
          u2   <- n.unreadSessions(List(s1, s2))
          box1 <- inbox
          _    <- n.readPage("/issues/3")
          box2 <- inbox
          _    <- Database.write(n.forgetSessionIn(s1))
          box3 <- inbox
          _    <- n.subscribe(sub("x")) *> n.subscribe(sub("x"))
          eps  <- Database.read(Repo.endpoints)
        yield assertTrue(
          u0 == Set(s1, s2),
          u1 == Set(s2),
          u2.isEmpty,
          box1.unread == 1L,
          box2.unread == 0L,
          box3.items.map(_.title) == List("c", "b"),
          box3.items.find(_.title == "b").flatMap(_.url).contains("/sessions/s2"),
          eps == List(sub("x").endpoint)
        )
      ))
    },
    test("会话事件转通知：用户在等的回答、等批准（触发器会话也通知）、最终失败、notify_user；没人在等的回答不通知；打开会话即已读") {
      ZIO.scoped(fresh { req =>
        val last = lastUserText(req)
        if last.contains("要批准") then Script.Say(List(call("a1", "approve_me", "{}")))
        else if last.contains("提醒我") then Script.Say(List(call("n1", "notify_user", """{"title":"到点了","body":"该开会了"}""")))
        else if last.contains("已通知用户") then Script.Say(List(text("提醒好了")))
        else if last.contains("搞坏") then Script.Fail(AppError.External("坏请求"))
        else Script.Say(List(text(s"答：$last")))
      }((_, _) =>
        for
          alice  <- agent("alice")
          s      <- ZIO.service[Sessions]
          user   <- userSession(alice, "你好")
          _      <- idle(user)
          trig   <- s.deliver(Address.Fresh(alice, Origin.Trigger(TriggerName.wrap("daily")), "日报"), Letter(Source.Trigger(TriggerName.wrap("daily")), "跑一下")).map(_.get.session)
          _      <- idle(trig)
          appr   <- s.deliver(Address.Fresh(alice, Origin.Trigger(TriggerName.wrap("deploy")), "部署"), Letter(Source.Trigger(TriggerName.wrap("deploy")), "要批准")).map(_.get.session)
          _      <- eventually("等批准")(stateOf(appr))(_ == WorkState.Waiting)
          remind <- userSession(alice, "提醒我")
          _      <- eventually("提醒完")(timeline(remind))(_.exists(_.parts.exists(_.text.contains("提醒好了"))))
          broken <- userSession(alice, "搞坏它")
          box    <- eventually("五条通知")(inbox)(_.items.size >= 5)
          con    <- ZIO.service[SessionConsole]
          page1  <- con.list(None, None, None, 50)
          _      <- con.markRead(user)
          page2  <- con.list(None, None, None, 50)
        yield
          val bySession = box.items.groupBy(_.session)
          val unread1   = page1.items.filter(_.unread).map(_.id).toSet
          assertTrue(
            box.items.size == 5,
            bySession.get(Some(user)).toList.flatten.map(n => (n.title, n.body)) == List("alice · 你好" -> "答：你好"),
            bySession.get(Some(trig)).isEmpty,
            bySession.get(Some(appr)).toList.flatten.map(n => (n.title, n.body)) == List("alice · 部署" -> "等你批准：跑测试脚本"),
            bySession.get(Some(remind)).toList.flatten.map(_.title).toSet == Set("到点了", "alice · 提醒我"),
            bySession.get(Some(broken)).toList.flatten.map(_.title) == List("alice · 搞坏它 运行失败"),
            bySession.get(Some(broken)).toList.flatten.head.body.contains("坏请求"),
            box.items.forall(n => n.url == n.session.map(id => s"/sessions/${id.value}")),
            unread1 == Set(user, appr, remind, broken),
            !page2.items.find(_.id == user).exists(_.unread)
          )
      ))
    },
    test("会话删了，指向它的通知随之删除") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("嗯"))))((_, _) =>
        for
          alice <- agent("alice")
          id    <- userSession(alice, "你好")
          _     <- eventually("有通知")(inbox)(_.items.nonEmpty)
          _     <- ZIO.serviceWithZIO[SessionConsole](_.delete(id))
          box   <- eventually("通知删了")(inbox)(_.items.isEmpty)
        yield assertTrue(box.unread == 0L)
      ))
    },
    test("HTTP：通知页、标已读、公钥、登记订阅（坏的拒绝）、测试推送") {
      ZIO.scoped(fresh(_ => Script.Say(List(text("嗯"))))((gw, _) =>
        for
          n     <- notifications
          bus   <- ZIO.service[ChangeBus]
          http   = NotificationsHttp(n)
          set    = ServerSettings(port = 0, password = "pw", cookieSecret = "s" * 32)
          authz <- Auth.layer.build.map(_.get[Auth]).provideSomeLayer[Scope](ZLayer.succeed(set))
          port  <- HttpServer.serve(set, authz, bus, http.routes).orDie
          c      = muliao.sessions.SessionsHttpSpec.Client(s"http://127.0.0.1:$port")
          call   = (m: String, p: String, b: Option[String]) => c.call(m, p, b).orDie
          _     <- call("POST", "/api/auth/login", Some("""{"password":"pw"}"""))
          key   <- call("GET", "/api/push/key", None)
          bad   <- call("POST", "/api/push/subscriptions", Some(PushSubscriptionIn("http://x/y", PushKeys(p256dh, auth)).toJson))
          good  <- call("POST", "/api/push/subscriptions", Some("""{"endpoint":"https://push.example.net/web","expirationTime":null,"keys":{"p256dh":"""" + p256dh + """","auth":"""" + auth + """"}}"""))
          test  <- call("POST", "/api/push/test", None)
          sent  <- eventually("推送了")(gw.triesOf("web"))(_.nonEmpty)
          _     <- n.notify(Notice("会话", "", Target.Session(SessionId.wrap("s9"))))
          list1 <- call("GET", "/api/notifications?limit=10", None)
          box1   = list1.body.fromJson[Inbox].toOption.get
          page  <- call("POST", "/api/notifications/read-page", Some(ReadPage("/sessions/s9").toJson))
          box2  <- inbox
          thru  <- call("POST", "/api/notifications/read", Some(ReadThrough(box1.items.last.id).toJson))
          box3  <- inbox
        yield assertTrue(
          key.body.fromJson[PushKey].toOption.contains(PushKey("PUBLIC")),
          bad.status == 400,
          good.status == 204,
          test.status == 204,
          sent.size == 1,
          box1.items.map(_.title) == List("会话", "Muliao"),
          box1.unread == 2L,
          page.status == 204,
          box2.unread == 1L,
          thru.status == 204,
          box3.unread == 0L
        )
      ))
    }
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(60.seconds) @@ TestAspect.sequential
