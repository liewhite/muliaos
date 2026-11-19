package muliao.web.pages.assets

import com.raquo.laminar.api.L.*
import muliao.contract.*
import muliao.web.core.*
import muliao.web.ui.*

import scala.concurrent.Future

import Http.ec

/** /ai-assets/skill?name=&owner=（见 [[Page.Skill]]）：编辑 SKILL.md（保存带指纹，期间被改过就提示重来）、浏览附件；不带 name 是新建。 */
object SkillEditor:
  private val SkillsUrl = Route.assets(AssetTab.Skill).url

  /** 已有 skill 的编辑页（名字来自服务端或刚保存成功）。 */
  def url(name: String, owner: String): String = Route.skill(Some(SkillRef(SkillName.wrap(name), Some(owner)))).url

  def mainPath(owner: String, name: String): String = s"$owner/skills/$name/SKILL.md"

  /** 新建时的 SKILL.md 模板。 */
  def template(name: String): String =
    s"""---
       |name: $name
       |description: 何时使用这个 skill（会出现在 agent 的 skill 索引里）
       |---
       |
       |# $name
       |
       |""".stripMargin

  /** @param target 地址里的 skill；没有是新建 */
  def view(target: Signal[Option[SkillRef]]): HtmlElement =
    div(
      cls := "h-full",
      child <-- target.distinct.map {
        case Some(r) => existing(r.name.value, r.owner)
        case None    => create()
      }
    )

  private def create(): HtmlElement =
    val agents  = AgentsData.list
    val owner   = Var(Owner.Platform)
    val name    = Var("")
    val content = Var(template("名字"))
    // 正文没动过时跟着名字更新模板
    val touched = Var(false)
    val act     = new Action
    def save(): Unit =
      val (o, n) = (owner.now(), name.now().trim)
      act
        .fire(
          Http.call(ResourcesApi.writeAssetFile)(AssetFileWrite(mainPath(o, n), content.now(), None)).map { _ =>
            Toast.success("skill 已保存")
            Router.update(Route.skill(Some(SkillRef(SkillName.wrap(n), Some(o)))))
          }
        )
    Ui.page("新建 skill", back = Some(Route.assets()))(
      Ui.card(
        div(
          cls := "card__content flex flex-col gap-4",
          name.signal.updates.filter(_ => !touched.now()) --> (n => content.set(template(if n.trim.isEmpty then "名字" else n.trim))),
          AssetUi.ownerSelect(owner.signal, owner.set, agents),
          Fields.text("名称", name, hint = Some("小写字母、数字、- 或 _；就是 skill 目录名"), mono = true),
          div(
            onInput --> (_ => touched.set(true)),
            Fields.area("SKILL.md", content, hint = Some("开头 --- 包起来的 frontmatter 里 description 必填：何时使用这个 skill"), rowsN = 18, mono = true)
          ),
          act.errorView(),
          div(Ui.btn(pending = act.busy.signal, disabledIf = name.signal.map(_.trim.isEmpty))("保存", onClick --> (_ => save())))
        )
      )
    )

  private def existing(name: String, ownerParam: Option[String]): HtmlElement =
    // 没给归属时按资源列表找（同名 skill 只有一份生效）
    val index = Remote(ResourcesApi.listAssets)(())
    val owner: Signal[Load[Option[String]]] = ownerParam match
      case Some(o) => Val(Load.Loaded(Some(o)))
      case None    => index.map(_.map(_.items.find(x => x.kind == AssetItemKind.Skill && x.name == name).map(_.owner)))
    Ui.page(span(cls := "font-mono", name), back = Some(Route.assets()))(
      Ui.loading(owner)(o =>
        child <-- o.distinct.map {
          case None    => Ui.emptyHint(s"没有叫 $name 的 skill")
          case Some(o) => editor(name, o)
        }
      )
    )

  private def editor(name: String, owner: String): HtmlElement =
    val act = new Action
    def remove(): Unit =
      Confirm
        .ask(s"删除 skill「$name」？", Some("整个 skill 目录（含附件）都会删除；用它的 agent 从下一次运行起就没有它了。"), "删除", danger = true)
        .foreach(ok => if ok then act.fire(Http.call(ResourcesApi.deleteAsset)((AssetKind.Skill, name)), leave = Some(() => Router.go(SkillsUrl))))
    div(
      cls := "flex flex-col gap-4",
      Ui.card(
        div(
          cls := "card__content flex flex-col gap-4",
          div(cls := "text-sm text-muted", s"归属：${Owner.label(owner)}"),
          FileEditor.view(
            mainPath(owner, name),
            "SKILL.md",
            rows = 18,
            hint = _ => "开头 --- 包起来的 frontmatter 里 description 必填：何时使用这个 skill，会出现在 agent 的 skill 索引里",
            extra = Ui.btn(Variant.DangerSoft, pending = act.busy.signal)(Icon("Trash2"), "删除", onClick --> (_ => remove()))
          ),
          act.errorView()
        )
      ),
      sectionTag(
        cls := "flex flex-col gap-2",
        h2(cls := "text-base font-semibold", "文件"),
        FileBrowser.view(files(name), "skill 目录", "skill 目录是空的")
      )
    )

  /** skill 目录：接口一次给出全部文件的相对路径，这里按层拆成目录（取不到大小与修改时间）。 */
  def files(name: String): DirSource = new DirSource:
    private var cached: Option[Future[List[String]]] = None
    // 读根目录（打开、刷新）时重新取一次全部文件，子目录用这一份
    private def fetch(path: String): Future[List[String]] = cached match
      case Some(f) if path.nonEmpty => f
      case _ =>
        val f = Http.call(ResourcesApi.skillFiles)(SkillName.wrap(name)).map(_.files)
        cached = Some(f)
        f
    def list(path: String): Future[DirContent] =
      fetch(path).map(fs => DirContent(level(fs, path), truncated = false))
    def read(path: String): Future[Array[Byte]] = Http.call(WorkspaceApi.skillFile)((SkillName.wrap(name), path)).map(_._3)
    def url(path: String): String               = Http.href(WorkspaceApi.skillFile)((SkillName.wrap(name), path))

  /** 全部文件的相对路径 → 某一层目录里的条目（目录在前，各自按名字排）。 */
  def level(files: List[String], dir: String): List[FileEntry] =
    val prefix = if dir.isEmpty then "" else s"$dir/"
    val below  = files.filter(_.startsWith(prefix)).map(_.drop(prefix.length)).filter(_.nonEmpty)
    val dirs   = below.filter(_.contains('/')).map(_.takeWhile(_ != '/')).distinct.sorted.map(FileEntry(_, dir = true, None, None))
    val plain  = below.filterNot(_.contains('/')).distinct.sorted.map(FileEntry(_, dir = false, None, None))
    dirs ++ plain
