package muliao.workspace.scripts

import muliao.contract.{AgentId, ScriptGrantView}
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.resources.agents.Agents
import muliao.resources.tree.{AssetStore, SkillDoc}
import muliao.workspace.store.{GrantRow, Repo}
import zio.*
import zio.json.*

import java.nio.file.{Files, Path}
import java.security.MessageDigest

/** 「始终允许」的键：同一 agent 以同样的参数和 secret 运行同一个 skill 脚本，且 skill 里除 SKILL.md 外的文件都没变。 */
final case class GrantKey(agent: AgentId, skill: String, script: String, args: List[String], secrets: List[String], filesHash: String):
  private[scripts] def argsJson: String    = args.toJson
  private[scripts] def secretsJson: String = secrets.distinct.sorted.toJson

/** skill 脚本的「始终允许」：查、记、列、撤销。 */
trait ScriptGrants:
  def allowed(key: GrantKey): IO[AppError, Boolean]
  def grant(key: GrantKey): IO[AppError, Unit]

  /** 全部记录；`current` 表示 skill 的文件和记下时一样（改过之后这条不再生效）。 */
  def list: IO[AppError, List[ScriptGrantView]]
  def revoke(id: Long): IO[AppError, Unit]
  def cleanup(agent: AgentId): IO[AppError, Unit]

object ScriptGrants:
  val layer: URLayer[Database & AssetStore & Agents, ScriptGrants] = ZLayer.derive[Live]

  /** skill 目录里除 SKILL.md 外全部文件（相对路径与内容）的摘要：脚本或它用到的附件改了，以前的「始终允许」就不再生效。 */
  def filesHash(dir: Path): IO[AppError, String] =
    ZIO
      .attemptBlocking {
        val md = MessageDigest.getInstance("SHA-256")
        val files = scala.util.Using.resource(Files.walk(dir)) { s =>
          s.filter(Files.isRegularFile(_)).toArray.toList.map(_.asInstanceOf[Path])
        }
        files
          .map(p => dir.relativize(p).toString -> p)
          .filterNot(_._1 == SkillDoc.Main)
          .sortBy(_._1)
          .foreach { (rel, p) =>
            md.update(rel.getBytes("UTF-8"))
            md.update(0.toByte)
            md.update(Files.readAllBytes(p))
            md.update(0.toByte)
          }
        md.digest().map(b => f"${b & 0xff}%02x").mkString
      }
      .mapError(e => AppError.internal(s"读 skill 目录 $dir 失败", e))

  final case class Live(db: Database, store: AssetStore, agents: Agents) extends ScriptGrants:
    private val now = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def allowed(k: GrantKey): IO[AppError, Boolean] =
      db.read(Repo.findGrant(k.agent, k.skill, k.script, k.argsJson, k.secretsJson, k.filesHash))

    def grant(k: GrantKey): IO[AppError, Unit] =
      now.flatMap(t => db.write(Repo.insertGrant(GrantRow(0L, k.agent, k.skill, k.script, k.argsJson, k.secretsJson, k.filesHash, t))))

    def list: IO[AppError, List[ScriptGrantView]] =
      for
        rows   <- db.read(Repo.allGrants)
        snap   <- store.snapshot
        names  <- agents.list.map(_.map(a => a.id -> a.name.value).toMap)
        hashes <- ZIO.foreach(rows.map(_.skill).distinct)(s => ZIO.foreach(snap.skill(s))(d => filesHash(d.dir)).map(s -> _)).map(_.toMap)
      yield rows.map { r =>
        ScriptGrantView(
          r.id,
          r.agentId,
          names.getOrElse(r.agentId, r.agentId.value),
          r.skill,
          r.script,
          r.args.fromJson[List[String]].getOrElse(Nil),
          r.secrets.fromJson[List[String]].getOrElse(Nil),
          hashes.get(r.skill).flatten.contains(r.filesHash),
          r.createdAt
        )
      }

    def revoke(id: Long): IO[AppError, Unit] =
      db.write(Repo.deleteGrant(id)).flatMap(n => ZIO.when(n == 0)(ZIO.fail(AppError.notFound(s"没有这条始终允许：$id"))).unit)

    def cleanup(agent: AgentId): IO[AppError, Unit] = db.write(Repo.deleteGrantsOf(agent))
