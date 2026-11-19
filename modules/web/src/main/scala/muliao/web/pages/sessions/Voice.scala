package muliao.web.pages.sessions

import com.raquo.laminar.api.L.*
import muliao.contract.ModelsApi
import muliao.web.core.*
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers.{SetTimeoutHandle, clearTimeout, setTimeout}
import scala.util.{Failure as TryFailure, Success}

import Http.ec

/** 按住说话：按下开始录音，松手交给服务端转写，拿到文字后调用 `onText`（空录音、误触不调用）；移出按钮再松手是取消。
  * 麦克风授权是异步的，授权完成前就松手同样作废；离开页面时停止录音并释放麦克风。服务端没配置语音转写、或浏览器不能录音时不显示。
  */
final class Voice(onText: String => Unit):
  enum Phase:
    case Idle
    case Recording(since: Double)
    case Transcribing

  val phase: Var[Phase]           = Var(Phase.Idle)
  val error: Var[Option[String]] = Var(None)

  // 录音的容器格式：Chrome/Firefox 录 webm/ogg（opus），iOS/Safari 只能录 mp4（aac）；服务端都认
  private val recordingTypes = List("audio/webm;codecs=opus", "audio/webm", "audio/ogg;codecs=opus", "audio/mp4")
  // 一段录音最长这么久，到点自动停止并转写；短于半秒的当作误触
  private val MaxRecordingMs = 5 * 60 * 1000
  private val MinRecordingMs = 500

  private var held                          = false
  private var cancelled                     = false
  private var recorder: Option[js.Dynamic]  = None

  private def recordable: Boolean =
    !js.isUndefined(js.Dynamic.global.MediaRecorder) && !js.isUndefined(dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices)

  /** 服务端配置决定的可选能力，不随数据变化：只取一次。 */
  val available: Signal[Boolean] =
    if !recordable then Val(false)
    else EventStream.fromFuture(Http.call(ModelsApi.transcriptionInfo)(()).map(_.enabled).recover { case _ => false }).toSignal(false)

  private def start(): Unit =
    error.set(None)
    cancelled = false
    phase.set(Phase.Recording(js.Date.now()))
    val md = dom.window.navigator.asInstanceOf[js.Dynamic].mediaDevices
    md.getUserMedia(js.Dynamic.literal(audio = true)).asInstanceOf[js.Promise[js.Dynamic]].toFuture.onComplete {
      case TryFailure(e) =>
        phase.set(Phase.Idle)
        error.set(Some(s"无法使用麦克风：${e.getMessage}"))
      case Success(stream) if !held =>
        stream.getTracks().asInstanceOf[js.Array[js.Dynamic]].foreach(_.stop())
        phase.set(Phase.Idle)
      case Success(stream) =>
        val MR       = js.Dynamic.global.MediaRecorder
        val mimeType = recordingTypes.find(t => MR.isTypeSupported(t).asInstanceOf[Boolean])
        val rec      = js.Dynamic.newInstance(MR)(stream, mimeType.fold[js.Any](js.undefined)(t => js.Dynamic.literal(mimeType = t)))
        val chunks   = js.Array[dom.Blob]()
        val began    = js.Date.now()
        val timer: SetTimeoutHandle = setTimeout(MaxRecordingMs.toDouble){ if rec.state.asInstanceOf[String] == "recording" then { rec.stop(); () } }
        rec.ondataavailable = (e: js.Dynamic) => if e.data.size.asInstanceOf[Double] > 0 then chunks.push(e.data.asInstanceOf[dom.Blob]): Unit
        rec.onstop = () =>
          clearTimeout(timer)
          stream.getTracks().asInstanceOf[js.Array[js.Dynamic]].foreach(_.stop())
          recorder = None
          val tpe   = Option(rec.mimeType.asInstanceOf[String]).filter(_.nonEmpty).orElse(mimeType).getOrElse("audio/webm")
          val audio = new dom.Blob(chunks.asInstanceOf[js.Array[dom.BlobPart]], new dom.BlobPropertyBag { `type` = tpe })
          if cancelled || js.Date.now() - began < MinRecordingMs || audio.size == 0 then phase.set(Phase.Idle)
          else
            phase.set(Phase.Transcribing)
            Http.bytesOf(audio).flatMap(b => Http.call(ModelsApi.transcribe)((tpe.takeWhile(_ != ';'), b))).onComplete { r =>
              phase.set(Phase.Idle)
              r match
                case Success(t) if t.text.isEmpty => error.set(Some("没有听到说话内容"))
                case Success(t)                   => onText(t.text)
                case TryFailure(e)                => error.set(Some(s"语音转文字失败：${Http.messageOf(e)}"))
            }
        recorder = Some(rec)
        rec.start()
        phase.set(Phase.Recording(began))
    }

  def press(): Unit =
    if phase.now() == Phase.Idle && !held then
      held = true
      start()

  def release(cancel: Boolean = false): Unit =
    if held then
      held = false
      cancelled = cancel
      recorder.filter(_.state.asInstanceOf[String] == "recording").foreach(_.stop())

  /** 离开页面：停止录音、释放麦克风，不再转写。 */
  def dispose(): Unit =
    held = false
    recorder.filter(_.state.asInstanceOf[String] == "recording").foreach { rec =>
      rec.onstop = null
      rec.stream.getTracks().asInstanceOf[js.Array[js.Dynamic]].foreach(_.stop())
      rec.stop()
    }

  /** 按住说话的麦克风按钮：录音中变红并显示时长，转写中转圈。键盘上按住空格/回车同理。 */
  def button(disabledIf: Signal[Boolean]): Modifier[HtmlElement] =
    child.maybe <-- available.map(ok => Option.when(ok)(buttonEl(disabledIf)))

  private def buttonEl(disabledIf: Signal[Boolean]): HtmlElement =
    val outside  = Var(false)
    val now      = Clock.every(500)
    val disabled = disabledIf.combineWith(phase.signal).map((d, p) => d || p == Phase.Transcribing)
    val disabledNow = Var(false)
    def inside(el: dom.Element, e: dom.PointerEvent): Boolean =
      val r = el.getBoundingClientRect()
      e.clientX >= r.left && e.clientX <= r.right && e.clientY >= r.top && e.clientY <= r.bottom
    val labelText = phase.signal.combineWith(outside.signal).map {
      case (Phase.Recording(_), true)  => "松手取消"
      case (Phase.Recording(_), false) => "松手转成文字，移出按钮取消"
      case (Phase.Transcribing, _)     => "正在转成文字"
      case _                           => "按住说话"
    }
    com.raquo.laminar.api.L.button(
      tpe := "button",
      Laminar.disabled <-- disabled,
      disabled --> disabledNow,
      aria.label <-- labelText,
      title <-- labelText,
      styleAttr := "touch-action:none;-webkit-touch-callout:none",
      cls <-- phase.signal.combineWith(outside.signal).map { (p, o) =>
        "inline-flex h-10 min-w-10 shrink-0 select-none items-center justify-center gap-1.5 rounded-xl px-2.5 text-sm transition-colors disabled:opacity-50 " +
          (p match
            case Phase.Recording(_) if o => "bg-default text-muted"
            case Phase.Recording(_)      => "bg-danger text-danger-foreground"
            case _                       => "text-foreground hover:bg-default")
      },
      inContext { el =>
        Seq(
          onPointerDown.filter(_.button == 0) --> { e =>
            if !disabledNow.now() then
              e.preventDefault()
              // 松手、移出时事件仍发给按钮
              el.ref.setPointerCapture(e.pointerId)
              outside.set(false)
              press()
          },
          onPointerMove --> (e => if phase.now().isInstanceOf[Phase.Recording] then outside.set(!inside(el.ref, e))),
          onPointerUp --> { e => release(!inside(el.ref, e)); outside.set(false) },
          onPointerCancel --> (_ => release(true))
        )
      },
      onKeyDown.filter(e => e.key == " " || e.key == "Enter") --> { e => e.preventDefault(); if !e.repeat then press() },
      onKeyUp.filter(e => e.key == " " || e.key == "Enter") --> { e => e.preventDefault(); release() },
      onBlur --> (_ => release(true)),
      onContextMenu.preventDefault --> (_ => ()),
      child <-- phase.signal.combineWith(outside.signal).map {
        case (Phase.Transcribing, _)    => Icon("LoaderCircle", "size-4 animate-spin")
        case (Phase.Recording(_), false) => Icon("Mic", "size-4 animate-pulse")
        case _                          => Icon("Mic", "size-4")
      },
      child.maybe <-- phase.signal.combineWith(now).map {
        case (Phase.Recording(since), t) =>
          val s = ((t - since) / 1000).toInt.max(0)
          Some(span(cls := "tabular-nums", f"${s / 60}:${s % 60}%02d"))
        case _ => None
      },
      child.maybe <-- phase.signal.combineWith(outside.signal).map {
        case (Phase.Recording(_), o) => Some(span(cls := "hidden sm:inline", if o then "松手取消" else "松手发送"))
        case _                       => None
      },
      onUnmountCallback(_ => dispose())
    )

  private object Laminar:
    val disabled = com.raquo.laminar.api.L.disabled
