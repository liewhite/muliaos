package muliao.resources

import muliao.contract.{AgentId, AgentName, AssetKind}
import muliao.platform.AppError
import muliao.resources.access.AccessValidation
import muliao.contract.AgentState
import muliao.resources.agents.{AgentRecord, AgentRepo}
import muliao.resources.tree.{AssetStore, Owner}
import muliao.resources.vault.{Fernet, Vault}
import muliao.toolkit.Access
import zio.*

import java.nio.file.{Files, Path, Paths}

/** 验收用：把现有资源目录的一份副本按 Scala 版加载一遍，报出加载结果（不打印任何 secret 的值）。
  * {{{
  * RealAssetsCheck <资源目录副本> <agent 列表 TSV：id\t名字\t是否归档> <存 vault 主密钥的文件>
  * }}}
  */
object RealAssetsCheck extends ZIOAppDefault:

  /** 需求文档第 4 节列出的内置工具。 */
  val toolNames: List[(String, Access)] =
    List(
      "read_file", "write_file", "edit_file", "share_file", "run_shell", "job_status", "job_kill", "skill_run", "ask_user",
      "notify_user", "delegate_task", "ask_agent", "list_threads", "stop_thread", "wake_me", "list_agents", "get_agent",
      "vault_set", "issue_list", "issue_show", "issue_create", "issue_update", "issue_comment", "issue_stop", "trigger_kinds",
      "trigger_list", "trigger_create", "trigger_update", "trigger_run", "trigger_delete", "history_append", "history_search",
      "activity_overview", "session_read", "web_search", "web_fetch"
    ).map(_ -> Access.Write) ++ List("create_agent" -> Access.Admin, "delete_agent" -> Access.Admin)

  def run =
    for
      args   <- getArgs
      root    = Paths.get(args(0))
      rows    = Files.readAllLines(Paths.get(args(1))).toArray.toList.map(_.toString.split('\t').toList)
      key    <- ZIO.attempt(Files.readString(Paths.get(args(2))).trim).orDie
      base    = Files.createTempDirectory("muliao-real")
      _      <- check(root, rows, key, base).provide(
                  ZLayer.succeed(ResourcePaths(root, base.resolve("workspaces"), base.resolve("state"), Nil)),
                  ZLayer.fromZIO(ZIO.service[ResourcePaths]).flatMap(p => AssetStore.layer(AssetStore.Settings(p.get.assets, p.get.lastGoodAccess))),
                  muliao.platform.db.Database.layer(muliao.platform.db.Database.Settings(base.resolve("t.db")), List(Module.migrations)),
                  muliao.platform.events.ChangeBus.layer,
                  ZLayer.succeed(ResourcesSettings()),
                  ZLayer.succeed(VaultSettings(key)),
                  ZLayer.succeed(ToolCatalog.fixed(toolNames.map(ToolInfo(_, _)))),
                  ZLayer.succeed(new ModelCheck { val defaultModel = "x:y"; def check(m: String) = None }),
                  AgentHooks.layer,
                  access.AccessEditor.layer,
                  agents.Agents.layer,
                  Resources.layer,
                  Vault.layer
                )
    yield ()

  private def check(root: Path, rows: List[List[String]], key: String, base: Path) =
    for
      db    <- ZIO.service[muliao.platform.db.Database]
      store <- ZIO.service[AssetStore]
      res   <- ZIO.service[Resources]
      _     <- ZIO.service[Vault] // 起得来说明主密钥对得上现有 secret
      _ <- ZIO.foreachDiscard(rows) { case List(id, name, archived) =>
             db.write(
               AgentRepo.insert(
                 AgentRecord(AgentId.wrap(id), AgentName.wrap(name), AgentRecord.keyOf(name), if archived == "1" then AgentState.Archived else AgentState.Active, false, None, 0, 0)
               )
             )
           case other => ZIO.fail(AppError.invalid(s"坏行 $other"))
           }
      snap   <- store.snapshot
      access <- store.access
      live    = rows.map(r => AgentId.wrap(r.head)).toSet
      _ <- Console.printLine(s"资源目录副本：$root")
      _ <- Console.printLine(
             s"加载：agent.toml ${snap.profiles.size}/${snap.agentDirs.size}，记忆 ${snap.memories.size}，skill ${snap.skills.size}，MCP ${snap.mcps.size}，secret ${snap.secrets.size}"
           )
      _ <- Console.printLine(s"没加载的文件 ${snap.problems.size} 个：")
      _ <- ZIO.foreachDiscard(snap.problems)(p => Console.printLine(s"  - ${root.relativize(p.path)}：${p.reason}"))
      _ <- Console.printLine(
             s"access.toml：${access.broken.fold("正常")(r => s"写坏了：$r")}，角色 ${access.config.roles.size}，agent 段 ${access.config.agents.size}"
           )
      _ <- ZIO.foreachDiscard(access.config.problems(live))(p => Console.printLine(s"  引用问题：$p"))
      names = toolNames.map(_._1)
      rules = (access.config.roles.values.flatMap(r => r.allow ++ r.deny) ++ access.config.agents.values.flatMap(g => g.allow ++ g.deny)).toList.distinct
      unmatched = rules.filterNot(AccessValidation.matchesSomething(_, snap, names))
      _ <- Console.printLine(s"规则 ${rules.size} 条，匹配不上现有工具或资源的 ${unmatched.size} 条：${unmatched.map(_.text).mkString("、")}")
      fernet <- ZIO.fromEither(Fernet.fromKey(key)).mapError(AppError.invalid)
      bad     = snap.secrets.filter(s => fernet.decrypt(s.content.ciphertext).isLeft).map(_.key)
      _ <- Console.printLine(s"secret 解密：${snap.secrets.size - bad.size}/${snap.secrets.size} 成功${if bad.isEmpty then "" else s"，失败：${bad.mkString("、")}"}")
      runs <- ZIO.foreach(rows.filter(_(2) == "0")) { r =>
                res.forRun(AgentId.wrap(r.head)).either.map(r(1) -> _)
              }
      _ <- Console.printLine(s"运行资源快照：${runs.count(_._2.isRight)}/${runs.size} 个 agent 成功")
      _ <- ZIO.foreachDiscard(runs.collect { case (n, Left(e)) => n -> e.message })((n, m) => Console.printLine(s"  - $n：$m"))
      muliao = rows.find(_(1) == "muliao").map(r => AgentId.wrap(r.head))
      _ <- ZIO.foreachDiscard(muliao) { id =>
             for
               rr <- res.forRun(id)
               sc <- res.writableScope(id)
               _ <- Console.printLine(
                      s"muliao：admin=${rr.permissions.isAdmin}，工具 ${rr.tools.size}，记忆 ${rr.memories.map(_.name).mkString(",")}，skill ${rr.skills.size}，" +
                        s"可写 ${sc.writable.size} 处，只读 ${sc.readOnly.size} 处，资源提醒 ${ResourcePrompt.notices(rr, Nil).linesIterator.size} 行"
                    )
             yield ()
           }
      sample = rows.find(r => r(1) == "slack-hub").map(r => AgentId.wrap(r.head))
      _ <- ZIO.foreachDiscard(sample) { id =>
             res.forRun(id).flatMap(rr => Console.printLine(s"slack-hub：工具 ${rr.tools.toList.sorted.mkString(",")}；skill ${rr.skills.map(_.doc.name).mkString(",")}"))
           }
      perOwner = snap.named.groupBy(_._3).view.mapValues(_.size).toMap
      _ <- Console.printLine(s"平台名下资源 ${perOwner.getOrElse(Owner.Platform, 0)} 项；按种类：" + AssetKind.values.map(k => s"${k.label} ${snap.named.count(_._1 == k)}").mkString("，"))
    yield ()
