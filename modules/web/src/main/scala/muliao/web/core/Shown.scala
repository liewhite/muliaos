package muliao.web.core

import muliao.contract.{IssueStatus, Priority}

/** 一个类型在界面上显示的文字。每种枚举只写一份：接口里的枚举写在这里，页面自己的枚举继承 [[Shown.Self]] 自带文字。 */
trait Shown[-A]:
  def text(a: A): String

/** 显示文字（页面 `import muliao.web.core.*` 即可用）。 */
extension [A](a: A)(using s: Shown[A]) def shown: String = s.text(a)

object Shown:
  /** 页面自己的枚举：定义处就写好显示文字。 */
  trait Self:
    def label: String

  given Shown[Self] = _.label

  given Shown[IssueStatus] = {
    case IssueStatus.Backlog    => "Backlog"
    case IssueStatus.InProgress => "进行中"
    case IssueStatus.Blocked    => "受阻"
    case IssueStatus.InReview   => "待审核"
    case IssueStatus.Done       => "完成"
    case IssueStatus.Cancelled  => "已取消"
  }

  given Shown[Priority] = {
    case Priority.Urgent => "紧急"
    case Priority.High   => "高"
    case Priority.Medium => "中"
    case Priority.Low    => "低"
    case Priority.Unset  => "无优先级"
  }
