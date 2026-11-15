package muliao.board

import muliao.contract.*
import zio.test.*

/** 看板规则（纯函数）：谁在处理、投给谁、停滞、改动成动态、交给用户、打回给谁、审核条件、依赖成环。 */
object RulesSpec extends ZIOSpecDefault:
  private val bob   = AgentId.wrap("bob")
  private val carol = AgentId.wrap("carol")
  private val one   = IssueId.wrap(1L)
  private val two   = IssueId.wrap(2L)

  private val base = Issue(one, "t", "", IssueStatus.InProgress, Priority.Unset, Some(carol), Some(Assignee.Agent(bob)), Nil, Actor.User, 0L, 0L)

  private def ev(actor: Actor, a: Activity) = IssueEvent(0L, one, 0L, actor, a)

  def spec = suite("看板规则")(
    test("投给谁：在做且不等上游投给指派的 agent；它在自己的 issue 会话里做的不投回；在别的会话里做的照投") {
      val inOwn   = By(Actor.Agent(bob, None), Some(one))
      val inOther = By(Actor.Agent(bob, None), Some(two))
      assertTrue(
        Rules.recipients(base, Nil, By.user, List(Activity.Commented("x"))) == List(bob),
        Rules.recipients(base, Nil, inOwn, List(Activity.Commented("x"))).isEmpty,
        Rules.recipients(base, Nil, inOther, List(Activity.Commented("x"))) == List(bob),
        Rules.recipients(base, Nil, By(Actor.Agent(carol, None)), List(Activity.StatusChanged(IssueStatus.Backlog, IssueStatus.InProgress))) == List(bob)
      )
    },
    test("没人处理（不在做、等上游、指派给用户）：只有用户的评论投给 owner") {
      val backlog = base.copy(status = IssueStatus.Backlog)
      val toUser  = base.copy(assignee = Some(Assignee.User))
      assertTrue(
        Rules.recipients(backlog, Nil, By.user, List(Activity.Commented("x"))) == List(carol),
        Rules.recipients(base, List(two), By.user, List(Activity.Commented("x"))) == List(carol),
        Rules.recipients(toUser, Nil, By.user, List(Activity.Commented("x"))) == List(carol),
        Rules.recipients(backlog, Nil, By.user, List(Activity.PriorityChanged(Priority.Unset, Priority.High))).isEmpty,
        Rules.recipients(backlog, Nil, By(Actor.Agent(bob, None)), List(Activity.Commented("x"))).isEmpty
      )
    },
    test("停滞：该有人处理、会话空闲或还没有") {
      assertTrue(
        Rules.stalled(base, Nil, None),
        Rules.stalled(base, Nil, Some(WorkState.Idle)),
        !Rules.stalled(base, Nil, Some(WorkState.Waiting)),
        !Rules.stalled(base, Nil, Some(WorkState.Running)),
        !Rules.stalled(base, List(two), None),
        !Rules.stalled(base.copy(status = IssueStatus.Blocked), Nil, None),
        !Rules.stalled(base.copy(assignee = Some(Assignee.User)), Nil, None)
      )
    },
    test("改动：每个变了的字段一条动态，没变的不记；依赖按集合比较") {
      val (after, acts) = Rules.diff(
        base.copy(dependsOn = List(two)),
        Patch(title = Some("t"), status = Some(IssueStatus.InReview), assignee = Some(Some(Assignee.User)), dependsOn = Some(List(two, two)))
      )
      val (_, none) = Rules.diff(base, Patch(assignee = Some(base.assignee), priority = Some(Priority.Unset)))
      val (unassigned, un) = Rules.diff(base, Patch(assignee = Some(None)))
      assertTrue(
        acts == List(Activity.StatusChanged(IssueStatus.InProgress, IssueStatus.InReview), Activity.AssigneeChanged(Some(Assignee.Agent(bob)), Some(Assignee.User))),
        after.dependsOn == List(two),
        none.isEmpty,
        unassigned.assignee.isEmpty,
        un == List(Activity.AssigneeChanged(Some(Assignee.Agent(bob)), None))
      )
    },
    test("交给用户：agent 把指派或状态改到用户名下才算；用户自己改的、已结束的不算") {
      val review = base.copy(status = IssueStatus.InReview, assignee = Some(Assignee.User))
      val agent  = By(Actor.Agent(bob, None))
      assertTrue(
        Rules.handedToUser(Some(base), review, agent),
        Rules.handedToUser(None, review, agent),
        Rules.handedToUser(Some(review.copy(status = IssueStatus.InProgress)), review.copy(status = IssueStatus.Blocked), agent),
        !Rules.handedToUser(Some(review), review.copy(priority = Priority.High), agent),
        !Rules.handedToUser(Some(base), review, By.user),
        !Rules.handedToUser(Some(base), review.copy(status = IssueStatus.Done), agent)
      )
    },
    test("审核：只对指派给用户、没结束的有效；打回给最近把它交给用户的 agent，没有就给 owner") {
      val review = base.copy(status = IssueStatus.InReview, assignee = Some(Assignee.User))
      val events = List(
        ev(Actor.Agent(carol, None), Activity.AssigneeChanged(None, Some(Assignee.User))),
        ev(Actor.User, Activity.AssigneeChanged(Some(Assignee.User), Some(Assignee.Agent(bob)))),
        ev(Actor.Agent(bob, None), Activity.AssigneeChanged(Some(Assignee.Agent(bob)), Some(Assignee.User)))
      )
      assertTrue(
        Rules.reviewable(review).isRight,
        Rules.reviewable(base).isLeft,
        Rules.reviewable(review.copy(status = IssueStatus.Done)).isLeft,
        Rules.sendBackTo(review, events).contains(bob),
        Rules.sendBackTo(review, Nil).contains(carol),
        Rules.sendBackTo(review.copy(owner = None), Nil).isEmpty
      )
    },
    test("依赖成环：给出首尾相同的环；无环为 None") {
      val c = Rules.cycle(Map(1 -> List(2), 2 -> List(3), 3 -> List(1), 4 -> List(1)))
      assertTrue(
        Rules.cycle(Map(1 -> List(2, 3), 2 -> List(3), 3 -> Nil)).isEmpty,
        c.exists(p => p.head == p.last && p.size == 4 && p.toSet == Set(1, 2, 3)),
        Rules.cycle(Map(1 -> List(1))).contains(List(1, 1))
      )
    },
    test("叫停去向：不能交审或完成；改方向要留言") {
      assertTrue(
        Halt.make(IssueStatus.Blocked, None).isRight,
        Halt.make(IssueStatus.InProgress, Some("  ")).isLeft,
        Halt.make(IssueStatus.InProgress, Some("改做 X")).map(_.message).contains(Some("改做 X")),
        Halt.make(IssueStatus.Done, Some("x")).isLeft,
        Halt.make(IssueStatus.InReview, None).isLeft
      )
    }
  )
