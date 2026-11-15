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

  private def list(in: HistorySearch) =
    for
      s     <- ZIO.foreach(in.since)(day("since", _))
      u     <- ZIO.foreach(in.until)(day("until", _))
      imp   <- ZIO.fromEither(Importance.of(in.minImportance.getOrElse(1))).mapError(AppError.invalid)
      found <- history.search(HistoryQuery(in.agent, HistoryQuery.words(in.q), s, u, imp, in.limit.getOrElse(50)))
      ns    <- history.names(found.map(_.agent))
    yield found.map(e =>
      HistoryEntryView(e.id, e.agent, ns(e.agent), e.day.toString, e.title, e.summary, e.refs, e.importance.level, e.session, e.createdAt)
    )

  def routes: List[Route] =
    List(HistoryApi.list.secured(list))
