package muliao.web.ui

import com.raquo.laminar.api.L.*

/** 编辑表单的草稿：`opened` 是开始编辑时的值，`value` 是正在改的值，`latest` 是服务端最新的值。
  * 没改过时跟着刷新；改过之后不再跟（不冲掉正在改的内容），提交时只发改过的部分，不覆盖别处同时改的其他字段。
  */
final class Draft[A](init: A):
  private val base   = Var(init)
  private val newest = Var(init)
  val value: Var[A]  = Var(init)

  val dirty: Signal[Boolean] = value.signal.combineWith(base.signal).map(_ != _)

  def opened: A = base.now()
  def latest: A = newest.now()
  def now: A    = value.now()

  def update(f: A => A): Unit = value.update(f)

  /** 服务端来了新值。 */
  def follow(a: A): Unit =
    newest.set(a)
    if value.now() == base.now() then
      base.set(a)
      value.set(a)

  /** 保存成功：以存下的值为新的起点。 */
  def saved(a: A): Unit =
    newest.set(a)
    base.set(a)
    value.set(a)

  /** 放弃修改：回到服务端最新的值。 */
  def discard(): Unit =
    base.set(newest.now())
    value.set(newest.now())

  /** 一个字段要提交的值：改过才有。 */
  def changed[F](field: A => F): Option[F] = Option.when(field(value.now()) != field(base.now()))(field(value.now()))

  /** 合并成完整的一份：改过的字段用草稿，没改的用服务端最新的（接口只收整份时用）。 */
  def merged[F](field: A => F): F = changed(field).getOrElse(field(newest.now()))

object Draft:
  /** 从一份随刷新变化的数据做表单：第一次有数据时建草稿并渲染一次，之后的数据只交给草稿去跟（不重建表单）。 */
  def form[D, A](data: Signal[D], toForm: D => A)(render: Draft[A] => HtmlElement): HtmlElement =
    var made: Option[(Draft[A], HtmlElement)] = None
    div(
      cls := "contents",
      child <-- data.map { d =>
        val a = toForm(d)
        made match
          case Some((draft, el)) =>
            draft.follow(a)
            el
          case None =>
            val draft = Draft(a)
            val el    = render(draft)
            made = Some((draft, el))
            el
      }
    )
