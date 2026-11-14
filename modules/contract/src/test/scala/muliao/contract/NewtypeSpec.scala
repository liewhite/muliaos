package muliao.contract

import zio.json.*
import zio.test.*

object NewtypeSpec extends ZIOSpecDefault:
  final case class Holder(agent: AgentId, issue: IssueId) derives JsonCodec

  def spec = suite("编号类型")(
    test("JSON 按底层类型编解码，外部输入经校验") {
      val h = Holder(AgentId.wrap("abc"), IssueId.wrap(7))
      assertTrue(
        h.toJson == """{"agent":"abc","issue":7}""",
        """{"agent":"abc","issue":7}""".fromJson[Holder] == Right(h),
        """{"agent":"","issue":7}""".fromJson[Holder].isLeft,
        """{"agent":"a","issue":0}""".fromJson[Holder].isLeft,
        AgentId.make("") == Left("不能为空"),
        IssueId.make(-1).isLeft
      )
    },
    test("错误体按种类对应状态码") {
      assertTrue(
        ErrorKind.NotFound.status.code == 404,
        ErrorKind.Unavailable.status.code == 503,
        ApiError(ErrorKind.Conflict, "忙").toJson == """{"kind":"Conflict","message":"忙"}"""
      )
    }
  )
