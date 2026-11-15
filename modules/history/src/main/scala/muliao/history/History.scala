package muliao.history

import muliao.board.Board
import muliao.contract.{AgentId, SessionId, Topic}
import muliao.history.store.Repo
import muliao.platform.{AppError, GeneralSettings}
import muliao.platform.db.Database
import muliao.platform.events.ChangeBus
import muliao.resources.agents.Agents
import muliao.sessions.Sessions
import zio.*

import java.time.{Instant, LocalDate, ZoneId}

/** 工作历史与活动回顾。
  *
  * 工作历史只能追加（库里拒绝改、删，删 agent 也保留）；检索按子串匹配（中文不用分词），按相关度排（[[Search]]）。
  * 活动回顾从会话领域（[[Sessions.activity]]）、看板（[[Board.activity]]）和本领域取某天的活动；读会话复用会话领域的时间线投影。
  * 日期都按配置时区。
  */
trait History:
  /** 配置时区的今天。 */
  def today: UIO[LocalDate]

  /** 记一批（同一事务，全部记下或都不记）。返回编号。 */
  def append(agent: AgentId, session: Option[SessionId], entries: List[NewEntry]): IO[AppError, List[Long]]

  def search(q: HistoryQuery): IO[AppError, List[HistoryEntry]]

  /** 某天（`agent` 不给是全部 agent）的活动。 */
  def overview(agent: Option[AgentId], day: LocalDate): IO[AppError, DayOverview]

  /** 按编号或唯一前缀读一个会话：会话信息与按时间排的条目。 */
  def read(session: String): IO[AppError, SessionRead]

  /** agent 编号到显示名（删除了的也有名字；查不到用编号）。 */
  def names(ids: Iterable[AgentId]): UIO[Map[AgentId, String]]

  def zone: ZoneId

object History:
  val layer: URLayer[Database & ChangeBus & Sessions & Board & Agents & GeneralSettings, History] =
    ZLayer {
      for
        db <- ZIO.service[Database]
        bus <- ZIO.service[ChangeBus]
        s   <- ZIO.service[Sessions]
        b   <- ZIO.service[Board]
        a   <- ZIO.service[Agents]
        g   <- ZIO.service[GeneralSettings]
      yield Live(db, bus, s, b, a, g.timezone)
    }

  private final case class Live(db: Database, bus: ChangeBus, sessions: Sessions, board: Board, agents: Agents, zone: ZoneId) extends History:

    private val now: UIO[Long] = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)

    def today: UIO[LocalDate] = now.map(t => Instant.ofEpochMilli(t).atZone(zone).toLocalDate)

    def append(agent: AgentId, session: Option[SessionId], entries: List[NewEntry]): IO[AppError, List[Long]] =
      for
        _   <- ZIO.fail(AppError.invalid("至少要有一条")).when(entries.isEmpty)
        t   <- now
        ids <- db.write(Repo.append(agent, session, entries, t) <* bus.publishAfterCommit(Topic.History))
      yield ids

    def search(q: HistoryQuery): IO[AppError, List[HistoryEntry]] =
      val limited = q.copy(limit = q.limit.max(1).min(200))
      limited.words.maxByOption(_.length) match
        case None          => db.read(Repo.recent(limited))
        case Some(longest) => db.read(Repo.containing(limited, longest)).map(Search.rank(_, limited.words, limited.limit))

    private def bounds(day: LocalDate): (Long, Long) =
      (day.atStartOfDay(zone).toInstant.toEpochMilli, day.plusDays(1).atStartOfDay(zone).toInstant.toEpochMilli)

    def overview(agent: Option[AgentId], day: LocalDate): IO[AppError, DayOverview] =
      val (from, to) = bounds(day)
      for
        acts   <- sessions.activity(agent, from, to)
        issues <- board.activity(agent, from, to)
        hist   <- db.read(Repo.recent(HistoryQuery(agent = agent, since = Some(day), until = Some(day), limit = Int.MaxValue)))
      yield DayOverview(agent, day, zone, acts, issues, hist.reverse)

    def read(session: String): IO[AppError, SessionRead] =
      for
        info <- sessions.find(session)
        tl   <- sessions.timeline(info.id)
      yield SessionRead(info, Reading.items(tl))

    def names(ids: Iterable[AgentId]): UIO[Map[AgentId, String]] = agents.names(ids)
