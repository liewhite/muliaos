package muliao.contract

import sttp.tapir.*
import sttp.tapir.json.zio.*
import zio.json.*

/** 工作历史的一条。
  * @param day        做这件事的那天（YYYY-MM-DD）
  * @param importance 1 日常、2 值得注意、3 重大
  * @param session    在哪个会话里记的
  */
final case class HistoryEntryView(
    id: Long,
    agent: AgentId,
    agentName: String,
    day: String,
    title: String,
    summary: String,
    refs: List[String],
    importance: Int,
    session: Option[SessionId],
    createdAt: Long
) derives JsonCodec, Schema

object HistoryApi:

  val list: Endpoint[Option[String], (Option[AgentId], Option[String], Option[String], Option[String], Option[Int], Option[Int]), ApiError, List[HistoryEntryView], Any] =
    Api.secure.get
      .in("history")
      .in(
        query[Option[AgentId]]("agent")
          .and(query[Option[String]]("q"))
          .and(query[Option[String]]("since"))
          .and(query[Option[String]]("until"))
          .and(query[Option[Int]]("minImportance"))
          .and(query[Option[Int]]("limit"))
      )
      .out(jsonBody[List[HistoryEntryView]])
      .name("listHistory")
      .refreshedBy(Topic.History)
      .description(
        "工作历史（agent 详情页）：`agent` 不给是全部；`q` 空白分开的关键字，每个都要出现，按相关度排，不给按日期倒序；`since` / `until` 是含两端的日期（YYYY-MM-DD）；" +
          "`minImportance` 1–3；`limit` 默认 50，最多 200。"
      )

  val all = List(list)
