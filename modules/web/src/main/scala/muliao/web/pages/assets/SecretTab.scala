package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** Secret：值加密保存，只能设不能看。passkey 加密的在浏览器里加密，每次 agent 要用都在这页上方等你授权（#915）；签名钥匙只能在浏览器里签名（#938）。 */
object SecretTab:
  import scala.concurrent.Future

  /** 加密方式：服务端主密钥、passkey（用时解开交出明文）、签名钥匙（只能在浏览器里签名，#938）。 */
  enum Lock(val label: String) extends Shown.Self:
    case Server  extends Lock("服务端")
    case Passkey extends Lock("passkey")
    case Signing extends Lock("签名钥匙 ed25519")

  object Lock:
    def of(v: SecretLockView): Lock = v match
      case SecretLockView.Server        => Server
      case SecretLockView.Passkey       => Passkey
      case SecretLockView.Signing(_)    => Signing

  /** `lock`、`allowShell` 没动过时：已有的保持原样、新建的取默认（只改值时不把加密方式和限制悄悄改掉）。 */
  final case class SecretForm(key: String, value: String, description: String, allowShell: Option[Boolean], owner: String, lock: Option[Lock])

  private def emptyForm(owner: String) = SecretForm("", "", "", None, owner, None)

  /** 新值：passkey 与签名钥匙在浏览器里用已用过的那把 passkey（没有就新建）加密，签名钥匙先核对是认得的私钥（ed25519：不带口令的 OpenSSH 私钥文件）。
    * `creds` 是页面上已拉到的，点击后不再发请求（Safari 要求 WebAuthn 由点击直接触发）。
    */
  private def valueOf(key: String, text: String, lock: Lock, creds: List[PasskeyCredential]): Future[SecretValue] = lock match
    case Lock.Server  => Future.successful(SecretValue.Plain(text))
    case Lock.Passkey => Passkey.seal(PasskeyAad.plain(key), text, creds).map(SecretValue.Passkey(_))
    case Lock.Signing => Passkey.sealSigning(key, SigningAlgorithm.Ed25519, text, creds).map(SecretValue.Signing(SigningAlgorithm.Ed25519, _))

  def view(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]]): HtmlElement =
    val vault = Remote(VaultApi.vaultState)(())
    val creds = Var(List.empty[PasskeyCredential])
    div(
      vault.map(_.data.map(_.credentials)) --> (_.foreach(creds.set)),
      cls := "flex flex-col gap-3",
      p(
        cls := "text-sm text-muted",
        "agent 通过 key 引用：MCP 配置里写 ",
        code(cls := "font-mono", "${vault:KEY}"),
        "，shell 里用 ",
        code(cls := "font-mono", "run_shell(secrets=[KEY])"),
        " 注入同名环境变量；不允许 shell 的只能用 ",
        code(cls := "font-mono", "skill_run(secrets=[KEY])"),
        " 经 skill 里的脚本注入。值只能设，不能看。passkey 加密的只有你的 passkey 能解开，agent 每次要用都等你授权（一次或一段时间）：在它的会话里批准，触发器这类没有会话的在下面批准。"
      ),
      Grants.view(vault, agents),
      Ui.collapsibleCard("新建或改值")(form(list, agents, creds)),
      AssetsPage.listOf(list, "没有 secret")(item(_, creds))
    )

  private def form(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]], creds: Var[List[PasskeyCredential]]): Modifier[HtmlElement] =
    val v        = Var(emptyForm(Owner.Platform))
    val act      = new Action
    val existing = v.signal.map(_.key.trim).combineWith(list).map((k, l) => l.find(_.name == k))
    def text(get: SecretForm => String, set: (SecretForm, String) => SecretForm) = v.zoom(get)(set)
    val lockOn  = v.signal.combineWith(existing).map((f, e) => f.lock.orElse(e.flatMap(_.lock).map(Lock.of)).getOrElse(Lock.Server))
    val lockNow = Var[Lock](Lock.Server)
    def save(): Unit =
      val f   = v.now()
      val key = f.key.trim
      act
        .run(
          valueOf(key, f.value, lockNow.now(), creds.now()).flatMap(value =>
            Http.call(ResourcesApi.putSecret)((key, SecretWrite(Some(value), Option(f.description.trim).filter(_.nonEmpty), f.allowShell, Some(f.owner))))
          )
        )
        .foreach(ok => if ok then { Toast.success(s"已保存 $key"); v.set(emptyForm(f.owner)) })
    Seq(
      lockOn --> lockNow,
      Fields.text("Key", text(_.key, (f, s) => f.copy(key = s)), hint = Some("已有的同名 secret 改值与说明，归属不变"), placeholderText = "SLACK_TOKEN", mono = true),
      Fields.text("值", text(_.value, (f, s) => f.copy(value = s)), inputType = "password"),
      Fields.text("说明", text(_.description, (f, s) => f.copy(description = s))),
      div(
        cls := "flex flex-row items-center gap-2 text-sm",
        // 没动过开关：已有的保持原值、新建的允许（只改值时不把不允许的悄悄放开）
        Fields.switch(
          "允许 shell 使用",
          v.signal.combineWith(existing).map((f, e) => f.allowShell.orElse(e.flatMap(_.allowShell)).getOrElse(true)),
          on => v.update(_.copy(allowShell = Some(on))),
          showLabel = true
        ),
        span(cls := "text-xs text-muted", "关掉后只能经 skill 脚本（skill_run）和 MCP 配置用")
      ),
      div(
        cls := "flex flex-col gap-1 text-sm",
        if Passkey.available then Fields.segmented("加密方式", lockOn, l => v.update(_.copy(lock = Some(l))), Lock.values.toList, showLabel = true)
        else span(cls := "text-xs text-muted", "passkey 加密与签名钥匙要在 HTTPS 的域名地址（如 https://muliao.leeliewhite.com）打开才能用"),
        span(
          cls := "text-xs text-muted",
          child.text <-- lockOn.map {
            case Lock.Server  => "服务端主密钥加密，agent 用时直接解开"
            case Lock.Passkey => "只有你的 passkey 能解开，agent 每次用都要你授权；服务端只存密文"
            case Lock.Signing => "私钥（ed25519：不带口令的 OpenSSH 私钥文件）只在你的浏览器里签名，取不出明文；agent 每次要签都要你批准"
          }
        )
      ),
      child.maybe <-- existing.map(_.isEmpty).distinct.map(isNew => Option.when(isNew)(AssetUi.ownerSelect(v.signal.map(_.owner), o => v.update(_.copy(owner = o)), agents))),
      act.errorView(),
      div(Ui.btn(pending = act.busy.signal, disabledIf = v.signal.map(f => f.key.trim.isEmpty || f.value.isEmpty))("保存", onClick --> (_ => save())))
    )

  private def item(s: StrictSignal[AssetItem], creds: Var[List[PasskeyCredential]]): HtmlElement =
    val act      = new Action
    val setting  = Var(false)
    val newValue = Var("")
    val key      = s.now().name
    def put(w: => Future[SecretWrite], done: String): Unit =
      act.run(w.flatMap(x => Http.call(ResourcesApi.putSecret)((key, x)))).foreach(ok => if ok then { Toast.success(done); setting.set(false); newValue.set("") })
    Ui.card(
      div(
        cls := "card__content flex flex-col gap-2",
        div(
          cls := "flex flex-row items-center gap-3",
          div(
            cls := "min-w-0 flex-1",
            div(
              cls := "flex flex-row items-center gap-2",
              span(cls := "font-mono font-medium", key),
              child.maybe <-- s.map(_.lock.collect {
                case SecretLockView.Passkey       => Ui.chip("passkey", Tone.Accent)()
                case SecretLockView.Signing(a)    => Ui.chip(s"签名钥匙 ${a.label}", Tone.Accent)()
              }),
              child.maybe <-- s.map(i => Option.when(i.allowShell.contains(false))(Ui.chip("仅 skill 脚本")()))
            ),
            div(cls := "truncate text-xs text-muted", child.text <-- s.map(_.description.filter(_.nonEmpty).getOrElse("无说明")))
          ),
          Fields.switch(
            s"允许 shell 使用 $key",
            s.map(_.allowShell.getOrElse(true)),
            on => put(Future.successful(SecretWrite(allowShell = Some(on))), if on then "已允许 shell 使用" else "已改为仅 skill 脚本可用"),
            disabledIf = act.busy.signal
          ),
          Ui.iconBtn("KeyRound", s"改 $key 的值")(onClick --> (_ => setting.update(!_))),
          AssetsPage.deleteButton(s, "引用它的 MCP 与命令会失败。", act)
        ),
        AssetUi.access(s, AgentsData.list),
        child.maybe <-- setting.signal.map(on =>
          Option.when(on)(
            div(
              cls := "flex items-end gap-2",
              div(cls := "flex-1", Fields.text("新的值", newValue, inputType = "password", autoFocusOn = true)),
              Ui.btn(pending = act.busy.signal, disabledIf = newValue.signal.map(_.isEmpty))(
                "保存",
                // 沿用原来的锁法：passkey 加密的仍用 passkey 加密，签名钥匙仍是签名钥匙
                onClick --> (_ => s.now().lock.map(Lock.of).foreach(l => put(valueOf(key, newValue.now(), l, creds.now()).map(v => SecretWrite(value = Some(v))), "已改值")))
              )
            )
          )
        ),
        act.errorView()
      )
    )

  /** 等授权的请求（全部会话的，含触发器这类没有会话的）与生效中的授权。 */
  object Grants:
    def view(state: Signal[Load[VaultState]], agents: Signal[List[AgentSummary]]): HtmlElement =
      div(
        cls := "flex flex-col gap-2",
        Ui.loading(state) { st =>
          div(
            cls := "flex flex-col gap-2",
            SecretAsks.list(st.map(_.asks), agents),
            children <-- st.map(_.grants).splitSeq(g => (g.agent, g.key, g.until))(s => grant(s.now(), agents))
          )
        }
      )

    private def grant(g: SecretGrant, agents: Signal[List[AgentSummary]]): HtmlElement =
      val act = new Action
      Ui.card(
        div(
          cls := "card__content flex flex-row items-center gap-3 text-sm",
          Icon("KeyRound", "size-4 text-muted"),
          div(
            cls := "min-w-0 flex-1",
            child.text <-- agents.map(as => s"${AgentsData.label(as, g.agent)} 可以用 "),
            code(cls := "font-mono", g.key),
            s"，到 ${new scala.scalajs.js.Date(g.until.toDouble).toLocaleString()}"
          ),
          Ui.btn(Variant.Ghost, Size.Sm, pending = act.busy.signal)("收回", onClick --> (_ => act.fire(Http.call(VaultApi.revokeGrant)(GrantRevoke(g.agent, g.key)))))
        ),
        act.errorView()
      )
