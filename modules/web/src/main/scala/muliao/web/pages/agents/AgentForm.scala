package muliao.web.pages.agents

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*


/** agent 表单的值（新建与编辑共用）。`group` 空串＝不分组；`model` 是 `<服务商>:<模型名>`。 */
final case class AgentForm(
    name: String,
    description: String,
    group: String,
    model: String,
    thinking: String,
    roles: List[String],
    allow: List[String],
    deny: List[String]
):
  def permissions: PermissionsInput.Value = PermissionsInput.Value(roles, allow, deny)
  def withPermissions(p: PermissionsInput.Value): AgentForm = copy(roles = p.roles, allow = p.allow, deny = p.deny)

object AgentForm:
  def of(d: AgentDetail): AgentForm =
    AgentForm(
      d.summary.name.value,
      d.summary.description.getOrElse(""),
      d.summary.group.getOrElse(""),
      d.summary.model.getOrElse(""),
      d.thinking.getOrElse("default"),
      d.roles,
      d.allow,
      d.deny
    )

  def blank(defaults: AgentDefaults): AgentForm = AgentForm("", "", "", defaults.model, "default", Nil, Nil, Nil)

  def create(f: AgentForm): AgentCreate =
    AgentCreate(
      name = f.name.trim,
      description = f.description.trim,
      model = Option(f.model).filter(_.nonEmpty),
      thinking = Some(f.thinking),
      group = Option(f.group.trim).filter(_.nonEmpty),
      roles = f.roles,
      allow = f.allow,
      deny = f.deny
    )

  /** 只含改过的字段。分组清空时发空串（接口约定：去掉分组）。 */
  def patch(d: Draft[AgentForm]): AgentUpdate =
    AgentUpdate(
      name = d.changed(_.name.trim),
      description = d.changed(_.description.trim),
      group = d.changed(_.group.trim),
      model = d.changed(_.model),
      thinking = d.changed(_.thinking),
      roles = d.changed(_.roles),
      allow = d.changed(_.allow),
      deny = d.changed(_.deny)
    )

  /** 思考档位的显示名：「默认」什么都不传（按模型与服务端的默认）。 */
  def thinkingLabel(t: String): String = t match
    case "default" => "默认"
    case "off"     => "关闭"
    case other     => other

  /** 各项配置。`agent` 为空是还没建的（预览权限时按它算「自己的资源」）。 */
  def fields(agent: Option[AgentId], form: Var[AgentForm], thinkings: List[String]): Modifier[HtmlElement] =
    def field(get: AgentForm => String, set: (AgentForm, String) => AgentForm): Var[String] =
      form.zoom(get)((f, v) => set(f, v))
    Seq(
      Fields.text("名称", field(_.name, (f, v) => f.copy(name = v)), hint = Some("只用于显示，可随时修改，不能与其他 agent 重名")),
      Fields.text(
        "职责描述",
        field(_.description, (f, v) => f.copy(description = v)),
        hint = Some("它的角色：agent 自己每次运行都会看到，其他 agent 委派任务时据此选择")
      ),
      Fields.text("分组", field(_.group, (f, v) => f.copy(group = v)), hint = Some("可不填；同一类的 agent（如一批接单的）归到一组，列表里按组折叠")),
      ModelInput.view(form.signal.map(_.model), m => form.update(_.copy(model = m)), form.signal.map(_.thinking), t => form.update(_.copy(thinking = t)), thinkings),
      PermissionsInput.view(agent, form.signal.map(_.permissions), f => form.update(x => x.withPermissions(f(x.permissions))))
    )

/** agent 的模型：服务商来自服务端配置，模型名向该服务商实时查询；旁边是它的思考档位。 */
object ModelInput:
  def split(model: String): (String, String) = model.split(":", 2) match
    case Array(p, m) => (p, m)
    case Array(p)    => (p, "")
    case _           => ("", "")

  def view(model: Signal[String], onModel: String => Unit, thinking: Signal[String], onThinking: String => Unit, thinkings: List[String]): HtmlElement =
    val overview = Remote.once(Http.call(ModelsApi.overview)(()))
    val provider = model.map(split(_)._1).distinct
    val listed: Signal[Load[List[String]]] = provider.flatMapSwitch {
      case "" => Val(Load.Ready(Nil))
      case p  => Remote.once(Http.call(ModelsApi.listModels)(p))
    }
    val names = listed.map(_.data.getOrElse(Nil))
    // 选模型名时要拼上当前的服务商
    val current = Var("")
    div(
      cls := "flex flex-col gap-2",
      model --> current,
      div(
        cls := "grid grid-cols-1 gap-2 sm:grid-cols-3",
        Fields.select[String](
          "服务商",
          provider.map(p => Option(p).filter(_.nonEmpty)),
          // 换服务商后模型要重新选；没选就保存会被服务端拒绝
          p => onModel(s"$p:"),
          overview.map(_.data.fold(Nil)(_.providers.map(p => Opt(p, p)))),
          hint = Some("来自服务端配置")
        ),
        Fields.select[String](
          "模型",
          model.map(m => Option(split(m)._2).filter(_.nonEmpty)),
          m => onModel(s"${split(current.now())._1}:$m"),
          // 当前的不在列表里也列出来，才显示得出
          names.combineWith(model).map((ns, m) => (ns ++ Option(split(m)._2).filter(n => n.nonEmpty && !ns.contains(n))).map(n => Opt(n, n))),
          hint = Some(child.text <-- listed.combineWith(model).map {
            case (Load.Pending, _)                                                   => "查询中…"
            case (l, m) if split(m)._2.nonEmpty && l.data.exists(!_.contains(split(m)._2)) => s"当前为 ${split(m)._2}，不在服务商返回的列表中"
            case _                                                                   => "向服务商实时查询"
          }),
          placeholderText = "请选择",
          disabledIf = provider.map(_.isEmpty)
        ),
        Fields.select[String](
          "思考",
          thinking.map(Some(_)),
          onThinking,
          Val(thinkings.map(t => Opt(t, AgentForm.thinkingLabel(t)))),
          hint = Some("默认＝不传，按模型自己的默认")
        )
      ),
      child.maybe <-- overview.map { case Load.Failed(m) => Some(Ui.errorAlert("读取服务商失败", m)); case _ => None },
      child.maybe <-- listed.map { case Load.Failed(m) => Some(Ui.errorAlert("查询模型失败", m)); case _ => None }
    )
