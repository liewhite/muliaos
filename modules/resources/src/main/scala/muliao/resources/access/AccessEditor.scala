package muliao.resources.access

import muliao.contract.{AgentId, AgentName, AssetKind}
import muliao.platform.AppError
import muliao.platform.db.Database
import muliao.resources.ToolCatalog
import muliao.resources.agents.AgentRepo
import muliao.resources.tree.{AssetPath, AssetStore, Snapshot}
import zio.*

/** 规则改动的校验：只拦这次新出现的问题，已有的问题不挡路（否则一处旧问题会让整个文件改不动）。 */
object AccessValidation:

  /** 规则能匹配到的现有对象：工具或已加载的资源。 */
  def matchesSomething(rule: Rule, snapshot: Snapshot, tools: List[String]): Boolean =
    rule.selector match
      case Selector.Path(_) => true
      case Selector.Named(kinds, glob) =>
        (kinds(AssetKind.Tool) && tools.exists(glob.matches)) ||
        snapshot.named.exists(a => kinds(a.kind) && glob.matches(a.name))

  /** `before` 为空表示原来没有这份文件或原来就是坏的。 */
  def check(before: Option[AccessConfig], after: AccessConfig, live: Set[AgentId], snapshot: Snapshot, tools: List[String]): Either[String, Unit] =
    val oldProblems = before.map(_.problems(live)).getOrElse(Nil).toSet
    val newProblems = after.problems(live).filterNot(oldProblems)
    val oldRules    = before.map(_.allRules).getOrElse(Set.empty)
    val rules = (after.roles.values.flatMap(r => r.allow ++ r.deny) ++ after.agents.values.flatMap(g => g.allow ++ g.deny)).toList
    val unmatched = rules.filterNot(r => oldRules(r.text)).filterNot(matchesSomething(_, snapshot, tools)).map(_.text).distinct
    (newProblems, unmatched) match
      case (Nil, Nil) => Right(())
      case _ =>
        val ps = newProblems ++ unmatched.map(t => s"规则 `$t` 匹配不上任何现有的工具或资源（名字写错了？）")
        Left(ps.mkString("；"))

/** 平台自己改 access.toml（界面编辑角色、建 agent 时配角色等）的唯一入口：串行读改写，校验后按固定格式写回。
  * 文件现在是坏的（手改写坏了）就拒绝，免得在旧规则上改动覆盖掉手写的内容；先把文件修好。
  */
trait AccessEditor:
  def modify(f: AccessConfig => Either[String, AccessConfig]): IO[AppError, AccessConfig]

object AccessEditor:
  val layer: ZLayer[AssetStore & Database & ToolCatalog, Nothing, AccessEditor] =
    ZLayer {
      for
        store <- ZIO.service[AssetStore]
        db    <- ZIO.service[Database]
        tools <- ZIO.service[ToolCatalog]
        lock  <- Semaphore.make(1)
      yield Live(store, db, tools, lock)
    }

  private final case class Live(store: AssetStore, db: Database, tools: ToolCatalog, lock: Semaphore) extends AccessEditor:
    def modify(f: AccessConfig => Either[String, AccessConfig]): IO[AppError, AccessConfig] =
      lock.withPermit {
        for
          state <- store.access
          _ <- ZIO.foreachDiscard(state.broken)(r =>
                 ZIO.fail(AppError.conflict(s"platform/access.toml 现在写坏了（$r），平台不在它上面改；先把文件修好"))
               )
          next     <- ZIO.fromEither(f(state.config)).mapError(AppError.invalid)
          agents   <- db.read(AgentRepo.grantable)
          snapshot <- store.snapshot
          _ <- ZIO
                 .fromEither(AccessValidation.check(Some(state.config), next, agents.map(_.id).toSet, snapshot, tools.tools.map(_.name)))
                 .mapError(AppError.invalid)
          names = agents.map(a => a.id -> a.name).toMap[AgentId, AgentName]
          _    <- store.write(AssetPath.AccessFile, AccessConfig.render(next, names.get))
        yield next
      }
