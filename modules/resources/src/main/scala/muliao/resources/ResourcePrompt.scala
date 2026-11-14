package muliao.resources

import muliao.resources.tree.Formats

/** 给模型看的资源说明段落。固定部分（记忆、skill 索引）在资源不变时逐字节不变，以命中提示缓存；
  * 每次运行的提醒（没加载的文件、超长的记忆、规则文件写坏）单独给出，由会话领域放进本次新增的第一条请求。
  */
object ResourcePrompt:

  private def attr(s: String): String = s.replace("&", "&amp;").replace("\"", "&quot;").replace("<", "&lt;")

  /** 记忆块全文。 */
  def memories(r: RunResources): String =
    if r.memories.isEmpty then ""
    else
      val blocks = r.memories.map { m =>
        val scope = m.kind match
          case MemoryKind.Global  => "全局"
          case MemoryKind.Shared  => "共享"
          case MemoryKind.Private => "你私有"
        val access = if m.editable then s"可改，$scope" else s"只读，$scope"
        s"""<memory name="${attr(m.name)}" access="$access" file="${attr(m.file.toString)}">
           |${m.content.stripSuffix("\n")}
           |</memory>""".stripMargin
      }
      (s"## 记忆\n你用到的记忆块（每块上限 ${r.memoryLimit} 字）；标「可改」的，改它的文件即可，下次运行生效。" :: blocks).mkString("\n")

  /** skill 索引：名字、是否你的、什么时候用、SKILL.md 在哪。 */
  def skillIndex(r: RunResources): String =
    if r.skills.isEmpty then ""
    else
      val lines = r.skills.map { s =>
        val mine = if s.owned then "（你的）" else ""
        s"- `${s.doc.name}`$mine：${s.doc.description.linesIterator.map(_.trim).mkString(" ")}（${s.doc.main}）"
      }
      ("## Skill 索引\n做和某个 skill 描述相符的事之前，先读它的 SKILL.md。" :: lines).mkString("\n")

  /** 本次运行的资源提醒；没有就是空串。`admins` 用于「找谁」。 */
  def notices(r: RunResources, admins: List[String]): String =
    val who = if admins.isEmpty then "用户" else s"admin（${admins.mkString("、")}）"
    val broken = r.accessBroken.map(why => s"[platform/access.toml 写坏了，现在沿用上一份有效的规则，平台拒绝再改它直到修好：$why]")
    val files = r.problems.map { p =>
      val mine = p.owner.exists(_.label == r.agent.name.value)
      val fix  = if mine then "这是你目录里的文件，照原因改好它" else s"不是你能改的，告诉 $who"
      s"[资源文件没加载：${p.path}：${p.reason}。$fix。]"
    }
    val long = r.memories.filter(_.editable).flatMap { m =>
      val n = Formats.memoryLength(m.content)
      Option.when(n > r.memoryLimit)(s"[记忆 ${m.name} 现在 $n 字，超过上限 ${r.memoryLimit}：请压缩（${m.file}）。]")
    }
    (broken.toList ++ files ++ long).mkString("\n")
