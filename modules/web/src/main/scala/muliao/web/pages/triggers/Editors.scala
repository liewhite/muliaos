package muliao.web.pages.triggers

import com.raquo.laminar.api.L.*
import muliao.contract.FireWhen
import muliao.web.ui.*

/** 配置与前置检查的编辑器：各输入框自己的 Var 只在挂上时从草稿初始化，之后把编辑中的内容写回草稿（草稿可能不合法，由提交方校验）。 */
object Editors:
  /** 按种类选配置编辑器。种类变了由调用方换一个新的编辑器（读换上的草稿）。 */
  def config(draft: Var[ConfigDraft]): HtmlElement = draft.now() match
    case ConfigDraft.Schedule(c) =>
      val cron = Var(c)
      div(
        cls := "contents",
        cron.signal.updates --> (v => draft.set(ConfigDraft.Schedule(v))),
        Fields.text("cron 表达式", cron, hint = Some("5 段式 cron 表达式，按服务端配置文件中的 timezone 解释，如 */5 * * * *"), mono = true)
      )
    case ConfigDraft.Once(a) =>
      val at = Var(a)
      div(
        cls := "contents",
        at.signal.updates --> (v => draft.set(ConfigDraft.Once(v))),
        Fields.text(
          "触发时间",
          at,
          hint = Some("按服务端配置文件中的 timezone 解释；必须是将来的时间。触发一次之后它就从列表里消失，在「已结束」里还能看到结果"),
          inputType = "datetime-local"
        )
      )
    case ConfigDraft.Stream(c, d, s) =>
      val command  = Var(c)
      val debounce = Var(d)
      val secrets  = Var(s)
      div(
        cls := "contents",
        command.signal.combineWith(debounce.signal, secrets.signal).updates --> ((c, d, s) => draft.set(ConfigDraft.Stream(c, d, s))),
        Fields.area(
          "常驻命令（bash，在 agent 的工作目录里运行）",
          command,
          hint = Some("stdout 每一行是一个事件；退出后按退避自动重启，停用或删除时被杀掉。stderr 写到工作目录的 trigger-logs/<名称>.stderr.log"),
          rowsN = 3,
          mono = true
        ),
        Fields.text(
          "合并窗口（秒）",
          debounce,
          hint = Some("第一行到达后再等这么久，期间的行合并成一次触发；上次的会话还在忙时事件排队，它结束后一起投递。留空用默认值"),
          mono = true
        ),
        Fields.text("secret（可选）", secrets, hint = Some("vault 里的 key，逗号分隔，作为同名环境变量传给命令"), mono = true)
      )

  /** 前置检查：先跑一条命令，只有真有事才调用 agent。 */
  def precheck(draft: Var[PrecheckDraft]): HtmlElement =
    val init     = draft.now()
    val on       = Var(init.on)
    val command  = Var(init.command)
    val fireWhen = Var(init.fireWhen)
    val secrets  = Var(init.secrets)
    div(
      cls := "flex flex-col gap-2 rounded-xl border border-separator p-3",
      on.signal.combineWith(command.signal, fireWhen.signal, secrets.signal).updates --> ((o, c, f, s) => draft.set(PrecheckDraft(o, c, f, s))),
      div(
        cls := "flex items-center gap-2 text-sm",
        Fields.switch("前置检查", on.signal, on.set),
        span(cls := "cursor-pointer", onClick --> (_ => on.update(!_)), "前置检查：先跑一条命令，有事才调用 agent")
      ),
      child.maybe <-- on.signal.map(o =>
        Option.when(o)(
          div(
            cls := "flex flex-col gap-2",
            Fields.area(
              "命令（bash，在 agent 的工作目录里执行）",
              command,
              hint = Some("stdin 是事件内容（流式命令输出的行）。退出码 0：按下面的条件决定是否触发，输出作为事件交给 agent；非 0 视为检查出错，不触发并告警一次"),
              rowsN = 3,
              mono = true
            ),
            Fields.select[FireWhen](
              "触发条件",
              fireWhen.signal.map(Some(_)),
              fireWhen.set,
              Val(FireWhen.values.toList.map(f => Opt(f, f.title, Some(f.description))))
            ),
            Fields.text("secret（可选）", secrets, hint = Some("vault 里的 key，逗号分隔，作为同名环境变量传给命令"), mono = true)
          )
        )
      )
    )
