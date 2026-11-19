package muliao.web.core

import com.raquo.airstream.core.{EventStream, Signal}

import scala.scalajs.js

/** 每隔一段时间一跳的当前时刻（毫秒），给运行中的计时用；没人订阅时不跳。 */
object Clock:
  def every(ms: Int): Signal[Double] = EventStream.periodic(ms).map(_ => js.Date.now()).toSignal(js.Date.now())
