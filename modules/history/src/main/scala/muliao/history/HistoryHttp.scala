package muliao.history

import muliao.contract.*
import muliao.platform.AppError
import muliao.platform.server.{Endpoints, Route}
import zio.*

import java.time.LocalDate

/** 本领域的 HTTP 接口：agent 详情页的工作历史。 */
final case class HistoryHttp(history: History):
  import Endpoints.*

  private def day(field: String, s: String): IO[AppError, LocalDate] =
    ZIO.attempt(LocalDate.parse(s.trim)).orElseFail(AppError.invalid(s"$field「$s」不是 YYYY-MM-DD 的日期"))

  private def list(agent: Option[AgentId], q: Option[String], since: Option[String], until: Option[String], min: Option[Int], limit: Option[Int]) =
    for
      s     <- ZIO.foreach(since)(day("since", _))
      u     <- ZIO.foreach(until)(day("until", _))
      imp   <- ZIO.fromEither(Importance.of(min.getOrElse(1))).mapError(AppError.invalid)
      found <- history.search(HistoryQuery(agent, HistoryQuery.words(q), s, u, imp, limit.getOrElse(50)))
      ns    <- history.names(found.map(_.agent))
    yield found.map(e =>
      HistoryEntryView(e.id, e.agent, ns(e.agent), e.day.toString, e.title, e.summary, e.refs, e.importance.level, e.session, e.createdAt)
    )

  def routes: List[Route] =
    List(HistoryApi.list.secured(list))
