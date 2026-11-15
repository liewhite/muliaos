package muliao.web.core

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.eventbus.EventBus
import muliao.contract.{ApiError, ErrorKind}
import org.scalajs.dom
import sttp.client4.fetch.FetchBackend
import sttp.model.Uri
import sttp.tapir.{DecodeResult, Endpoint}
import sttp.tapir.client.sttp4.SttpClientInterpreter

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.concurrent.JSExecutionContext
import scala.scalajs.js
import scala.scalajs.js.typedarray.{Int8Array, ArrayBuffer}

/** 调接口失败：服务端的统一错误体，或请求根本没成（断网、解码失败）。 */
enum Failure:
  case Api(error: ApiError)
  case Transport(detail: String)

  def message: String = this match
    case Api(e)       => e.message
    case Transport(m) => m

/** 失败当成异常在 Future 里传：调用方用 [[Http.failureOf]] 取回原因。 */
final class FailureException(val failure: Failure) extends Exception(failure.message)

/** 调后端接口的唯一入口：用 contract 里的 tapir 接口声明生成请求（与服务端同一份类型），浏览器自动带登录 cookie。 */
object Http:
  given ec: ExecutionContext = JSExecutionContext.queue

  private val backend     = FetchBackend()
  private val interpreter = SttpClientInterpreter()
  private def base: Option[Uri] = Some(Uri.unsafeParse(dom.window.location.origin))

  /** 登录失效（任何接口回 401）：外壳据此回到登录页。 */
  val unauthorized: EventBus[Unit] = new EventBus[Unit]

  /** 接口的站内地址（路径加查询参数），由接口声明生成：给浏览器自己去取的场合（下载链接、图片、实时连接），不手拼。 */
  def href[I, O](e: Endpoint[Option[String], I, ApiError, O, Any])(in: I): String =
    interpreter.toSecureRequest(e, Some(hrefBase)).apply(None).apply(in).uri.toString.stripPrefix(hrefBase.toString)

  /** 生成地址时借用的源（只为得到绝对地址再去掉它）。 */
  private val hrefBase = Uri.unsafeParse("http://muliao.invalid")

  /** 调一个要登录的接口。cookie 由浏览器自己带，安全输入给 None。 */
  def call[I, O](e: Endpoint[Option[String], I, ApiError, O, Any])(in: I): Future[O] =
    finish(interpreter.toSecureRequest(e, base).apply(None).apply(in).send(backend).map(_.body))

  /** 只看状态码的 POST：登录、退出登录。它们的回应是 Set-Cookie，浏览器不让脚本读这个头，按接口声明解码必然失败，所以不走 tapir 客户端。 */
  def postStatus(path: String, json: Option[String]): Future[Unit] =
    val init = new dom.RequestInit {}
    init.method = dom.HttpMethod.POST
    init.credentials = dom.RequestCredentials.`same-origin`
    json.foreach { body =>
      init.body = body
      init.headers = js.Dictionary("Content-Type" -> "application/json")
    }
    val sent = dom.fetch(path, init).toFuture.flatMap { resp =>
      if resp.ok then Future.successful(DecodeResult.Value(Right(())))
      else
        resp.text().toFuture.map { body =>
          import zio.json.*
          body.fromJson[ApiError] match
            case Right(e) => DecodeResult.Value(Left(e))
            case Left(_)  => DecodeResult.Value(Left(ApiError(ErrorKind.Internal, s"HTTP ${resp.status}")))
        }
    }
    finish(sent)

  private def finish[O](f: Future[DecodeResult[Either[ApiError, O]]]): Future[O] =
    f.transform {
      case scala.util.Success(DecodeResult.Value(Right(o))) => scala.util.Success(o)
      case scala.util.Success(DecodeResult.Value(Left(e))) =>
        if e.kind == ErrorKind.Unauthorized then unauthorized.emit(())
        scala.util.Failure(FailureException(Failure.Api(e)))
      case scala.util.Success(other: DecodeResult.Failure) =>
        scala.util.Failure(FailureException(Failure.Transport(s"服务端的回应看不懂：$other")))
      case scala.util.Failure(e: FailureException) => scala.util.Failure(e)
      case scala.util.Failure(e) =>
        scala.util.Failure(FailureException(Failure.Transport(s"连不上服务端：${Option(e.getMessage).getOrElse(e.toString)}")))
    }

  /** 失败的原因（给用户看的一句话）。 */
  def failureOf(t: Throwable): Failure = t match
    case f: FailureException => f.failure
    case other               => Failure.Transport(Option(other.getMessage).getOrElse(other.toString))

  def messageOf(t: Throwable): String = failureOf(t).message

  def stream[O](f: => Future[O]): EventStream[scala.util.Try[O]] =
    EventStream.fromFuture(f.transform(scala.util.Success(_)))

  /** 浏览器的文件读成字节（上传、语音）。 */
  def bytesOf(blob: dom.Blob): Future[Array[Byte]] =
    blob.arrayBuffer().toFuture.map((buf: ArrayBuffer) => new Int8Array(buf).toArray)
