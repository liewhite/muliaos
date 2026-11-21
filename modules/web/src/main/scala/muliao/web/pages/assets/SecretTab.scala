package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import Http.ec

/** Secret：三个维度各选一项（#1069）：怎么保存（服务端加密 / 你的 passkey 加密 / 明文）、内容（口令 / 密钥对）、
  * 能否取出明文（任意命令 / 只给脚本 / 只能签名）。加密的值只能设不能看；passkey 加密的在浏览器里加密，每次 agent 要用都在这页上方等你授权（#915）；
  * 明文保存的只能是口令，在这页上直接显示（#1093）。
  */
object SecretTab:
  import scala.concurrent.Future

  /** 怎么保存。 */
  enum Lock(val label: String) extends Shown.Self:
    case Server  extends Lock("服务端加密")
    case Passkey extends Lock("passkey 加密")
    case Plain   extends Lock("明文")

  object Lock:
    def of(v: SecretView): Lock = v match
      case SecretView.Server(_)   => Server
      case SecretView.Passkey(_)  => Passkey
      case SecretView.Plain(_, _) => Plain

    /** passkey 加密要在 HTTPS 的域名地址上才能选。 */
    def options: List[Lock] = if muliao.web.core.Passkey.available then values.toList else List(Server, Plain)

  /** 内容。 */
  enum Kind(val label: String) extends Shown.Self:
    case Password extends Kind("口令")
    case Ed25519  extends Kind("ed25519 密钥")

  object Kind:
    /** 明文保存的只能是口令。 */
    def options(l: Lock): List[Kind] = l match
      case Lock.Plain                => List(Password)
      case Lock.Server | Lock.Passkey => values.toList

    def fit(l: Lock, k: Kind): Kind = if options(l).contains(k) then k else Password

  /** 能否取出明文。`Sign` 只给密钥对。 */
  enum Usage(val label: String) extends Shown.Self:
    case Shell   extends Usage("任意命令")
    case Scripts extends Usage("只给脚本")
    case Sign    extends Usage("只能签名")

  object Usage:
    def of(s: RevealScope): Usage = s match
      case RevealScope.Shell   => Shell
      case RevealScope.Scripts => Scripts

    def options(k: Kind): List[Usage] = k match
      case Kind.Password => List(Shell, Scripts)
      case Kind.Ed25519  => values.toList

    /** 口令的明文范围（「只能签名」按只给脚本算，同 [[fit]]）。 */
    def passwordScope(u: Usage): RevealScope = u match
      case Shell          => RevealScope.Shell
      case Scripts | Sign => RevealScope.Scripts

    /** 新建时的默认：口令任意命令可用；密钥对多半是 CA、签名私钥，默认只能签名。 */
    def default(k: Kind): Usage = k match
      case Kind.Password => Shell
      case Kind.Ed25519  => Sign

    /** 选的用法在这种内容下不可选（只能签名的切回口令）：落到口令里最严的一档，不放宽成任意命令。 */
    def fit(k: Kind, u: Usage): Usage = if options(k).contains(u) then u else Scripts

  def kindOf(c: SecretContent): Kind = c match
    case SecretContent.Opaque(_)     => Kind.Password
    case SecretContent.KeyPair(_, _) => Kind.Ed25519

  def usageOf(c: SecretContent): Usage = c.revealScope.fold(Usage.Sign)(Usage.of)

  /** 选的内容与用法合成一个 secret 内容（用法先经 [[Usage.fit]]，口令配「只能签名」按只给脚本算）。 */
  def contentOf(k: Kind, u: Usage): SecretContent = (k, u) match
    case (Kind.Password, _)             => SecretContent.Opaque(Usage.passwordScope(u))
    case (Kind.Ed25519, Usage.Sign)     => SecretContent.KeyPair(SigningAlgorithm.Ed25519, KeyExposure.SignOnly)
    case (Kind.Ed25519, Usage.Scripts)  => SecretContent.KeyPair(SigningAlgorithm.Ed25519, KeyExposure.Reveal(RevealScope.Scripts))
    case (Kind.Ed25519, Usage.Shell)    => SecretContent.KeyPair(SigningAlgorithm.Ed25519, KeyExposure.Reveal(RevealScope.Shell))

  /** 三个选项没动过时：已有的保持原样、新建的取默认（只改值时不把加密方式和限制悄悄改掉）。 */
  final case class SecretForm(key: String, value: String, description: String, owner: String, lock: Option[Lock], kind: Option[Kind], usage: Option[Usage])

  private def emptyForm(owner: String) = SecretForm("", "", "", owner, None, None, None)

  /** 新值按什么存（不含值）：加密的带内容，明文的只是口令、带明文范围。 */
  enum Target:
    case Server(content: SecretContent)
    case Passkey(content: SecretContent)
    case Plain(scope: RevealScope)

  object Target:
    /** 沿用已有的。 */
    def of(v: SecretView): Target = v match
      case SecretView.Server(c)   => Server(c)
      case SecretView.Passkey(c)  => Passkey(c)
      case SecretView.Plain(_, s) => Plain(s)

    /** 表单里选的：明文的内容固定为口令（表单只给它口令可选）。 */
    def of(l: Lock, k: Kind, u: Usage): Target = l match
      case Lock.Server  => Server(contentOf(k, u))
      case Lock.Passkey => Passkey(contentOf(k, u))
      case Lock.Plain   => Plain(Usage.passwordScope(u))

  extension (t: Target)
    def content: SecretContent = t match
      case Target.Server(c)  => c
      case Target.Passkey(c) => c
      case Target.Plain(s)   => SecretContent.Opaque(s)

  /** 新值：passkey 的在浏览器里用已用过的那把 passkey（没有就新建）加密，密钥对先核对是认得的私钥（ed25519：不带口令的 OpenSSH 私钥文件）。
    * `creds` 是页面上已拉到的，点击后不再发请求（Safari 要求 WebAuthn 由点击直接触发）。
    */
  private def valueOf(key: String, text: String, target: Target, creds: List[PasskeyCredential]): Future[SecretValue] = target match
    case Target.Server(c)  => Future.successful(SecretValue.Server(text, c))
    case Target.Passkey(c) => Passkey.sealFor(key, c, text, creds).map(SecretValue.Passkey(_, c))
    case Target.Plain(s)   => Future.successful(SecretValue.Plain(text, s))

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
        " 注入同名环境变量；只给脚本的只能用 ",
        code(cls := "font-mono", "skill_run(secrets=[KEY])"),
        " 经 skill 里的脚本注入；只能签名的密钥对取不出明文，只能请平台签（如 ",
        code(cls := "font-mono", "ssh_certificate"),
        "）。加密的值只能设，不能看；明文保存的直接显示在下面。passkey 加密的只有你的 passkey 能解开，agent 每次要用都等你授权（一次或一段时间）：在它的会话里批准，触发器这类没有会话的在下面批准。"
      ),
      Grants.view(vault, agents),
      Ui.collapsibleCard("新建或改值")(form(list, agents, creds)),
      AssetsPage.listOf(list, "没有 secret")(item(_, creds))
    )

  private def form(list: Signal[List[AssetItem]], agents: Signal[List[AgentSummary]], creds: Var[List[PasskeyCredential]]): Modifier[HtmlElement] =
    val v        = Var(emptyForm(Owner.Platform))
    val act      = new Action
    val existing = v.signal.map(_.key.trim).combineWith(list).map((k, l) => l.find(_.name == k).flatMap(_.secret))
    def text(get: SecretForm => String, set: (SecretForm, String) => SecretForm) = v.zoom(get)(set)
    val lockOn  = v.signal.combineWith(existing).map((f, e) => f.lock.orElse(e.map(Lock.of)).getOrElse(Lock.Server))
    val kindOn = v.signal.combineWith(existing, lockOn).map((f, e, l) => Kind.fit(l, f.kind.orElse(e.map(s => kindOf(s.content))).getOrElse(Kind.Password)))
    val usageOn  = v.signal.combineWith(existing, kindOn).map((f, e, k) => Usage.fit(k, f.usage.orElse(e.map(s => usageOf(s.content))).getOrElse(Usage.default(k))))
    val targetOn = lockOn.combineWith(kindOn, usageOn).map((l, k, u) => Target.of(l, k, u))
    val targetNow = Var[Target](Target.Server(SecretContent.Default))
    def save(): Unit =
      val f   = v.now()
      val key = f.key.trim
      act
        .run(
          valueOf(key, f.value, targetNow.now(), creds.now()).flatMap(value =>
            Http.call(ResourcesApi.putSecret)((key, SecretWrite.Value(value, Option(f.description.trim).filter(_.nonEmpty), Some(f.owner))))
          )
        )
        .foreach(ok => if ok then { Toast.success(s"已保存 $key"); v.set(emptyForm(f.owner)) })
    Seq(
      targetOn --> targetNow,
      Fields.text("Key", text(_.key, (f, s) => f.copy(key = s)), hint = Some("已有的同名 secret 改值与说明，归属不变"), placeholderText = "SLACK_TOKEN", mono = true),
      // 明文保存的值不遮住
      child <-- lockOn.map(_ == Lock.Plain).distinct.map(plain =>
        Fields.text("值", text(_.value, (f, s) => f.copy(value = s)), inputType = if plain then "text" else "password", mono = plain)
      ),
      Fields.text("说明", text(_.description, (f, s) => f.copy(description = s))),
      div(
        cls := "flex flex-col gap-1 text-sm",
        Fields.segmented("怎么保存", lockOn, l => v.update(_.copy(lock = Some(l))), Lock.options, showLabel = true),
        span(
          cls := "text-xs text-muted",
          child.text <-- targetOn.map {
            case Target.Server(c) if c.revealScope.isEmpty  => "服务端主密钥加密，平台在服务端签名、不用你批准；agent 的工具取不出明文（防误用：能读主密钥的程序仍能解开，要真正隔离选 passkey）"
            case Target.Server(_)                           => "服务端主密钥加密，agent 用时直接解开；值只能设、不能看"
            case Target.Passkey(c) if c.revealScope.isEmpty => "只有你的 passkey 能解开：私钥只在你的浏览器里签名，agent 每次要签都要你批准"
            case Target.Passkey(_)                          => "只有你的 passkey 能解开，agent 每次用都要你授权；服务端只存密文"
            case Target.Plain(_)                            => "不加密：值照原样存在文件里，这页上直接显示，agent 读得到文件也就看得到值；只放不保密的配置（地址、账户名等），只能是口令"
          }
        ),
        Option.unless(muliao.web.core.Passkey.available)(
          span(cls := "text-xs text-muted", "passkey 加密要在 HTTPS 的域名地址（如 https://muliao.leeliewhite.com）打开才能用")
        )
      ),
      div(
        cls := "flex flex-col gap-1 text-sm",
        child <-- lockOn.map(l => Fields.segmented("内容", kindOn, k => v.update(_.copy(kind = Some(k))), Kind.options(l), showLabel = true)),
        span(
          cls := "text-xs text-muted",
          child.text <-- kindOn.map {
            case Kind.Password => "任意文本：token、口令、私钥都行，平台不解析"
            case Kind.Ed25519  => "ed25519 私钥（不带口令的 OpenSSH 私钥文件），平台能用它签名（如 SSH 证书）"
          }
        )
      ),
      div(
        cls := "flex flex-col gap-1 text-sm",
        child <-- kindOn.map(k => Fields.segmented("能否取出明文", usageOn, u => v.update(_.copy(usage = Some(u))), Usage.options(k), showLabel = true)),
        span(
          cls := "text-xs text-muted",
          child.text <-- targetOn.map(_.content.revealScope).map {
            case Some(RevealScope.Shell)   => "能注入任意命令（run_shell 等）"
            case Some(RevealScope.Scripts) => "只能经 skill 脚本（skill_run）和 MCP 配置用，不能注入自写命令"
            case None                      => "取不出明文：只能请平台签名"
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
              children <-- s.map(_.secret.toList.flatMap { sv =>
                (sv match
                  case SecretView.Passkey(_)  => List(Ui.chip("passkey", Tone.Accent)())
                  case SecretView.Plain(_, _) => List(Ui.chip(Lock.Plain.label)())
                  case SecretView.Server(_)   => Nil) ++
                  Option.when(kindOf(sv.content) == Kind.Ed25519)(Ui.chip(Kind.Ed25519.label, Tone.Accent)()).toList ++
                  (usageOf(sv.content) match
                    case Usage.Shell => Nil
                    case u           => List(Ui.chip(u.label)()))
              })
            ),
            div(cls := "truncate text-xs text-muted", child.text <-- s.map(_.description.filter(_.nonEmpty).getOrElse("无说明"))),
            // 明文保存的直接显示值
            child.maybe <-- s.map(_.secret.collect { case SecretView.Plain(value, _) => value }).distinct.map(_.map(value =>
              div(cls := "mt-1 whitespace-pre-wrap break-all font-mono text-sm select-all", value)
            ))
          ),
          // 只能签名的没有明文范围可切
          child.maybe <-- s.map(_.secret.flatMap(_.content.revealScope)).distinct.map(_.map(_ =>
            Fields.switch(
              s"允许 shell 使用 $key",
              s.map(_.secret.flatMap(_.content.revealScope).contains(RevealScope.Shell)),
              on =>
                put(
                  Future.successful(SecretWrite.Settings(scope = Some(if on then RevealScope.Shell else RevealScope.Scripts))),
                  if on then "已允许 shell 使用" else "已改为只给脚本"
                ),
              disabledIf = act.busy.signal
            )
          )),
          // 明文保存的改值时预填当前值
          Ui.iconBtn("KeyRound", s"改 $key 的值")(onClick --> (_ =>
            newValue.set(s.now().secret.collect { case SecretView.Plain(value, _) => value }.getOrElse(""))
            setting.update(!_)
          )),
          AssetsPage.deleteButton(s, "引用它的 MCP 与命令会失败。", act)
        ),
        AssetUi.access(s, AgentsData.list),
        child.maybe <-- setting.signal.map(on =>
          Option.when(on)(
            div(
              cls := "flex items-end gap-2",
              div(
                cls := "flex-1",
                s.now().secret match
                  case Some(SecretView.Plain(_, _)) => Fields.text("新的值", newValue, mono = true, autoFocusOn = true)
                  case _                            => Fields.text("新的值", newValue, inputType = "password", autoFocusOn = true)
              ),
              Ui.btn(pending = act.busy.signal, disabledIf = newValue.signal.map(_.isEmpty))(
                "保存",
                // 沿用原来的保存方式、内容与能否取明文
                onClick --> (_ =>
                  s.now().secret.foreach(sv =>
                    put(valueOf(key, newValue.now(), Target.of(sv), creds.now()).map(v => SecretWrite.Value(v)), "已改值")
                  )
                )
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
