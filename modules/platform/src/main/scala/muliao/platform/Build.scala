package muliao.platform

/** 本程序的版本：打包时写进 jar 清单的 `<时间>-<commit>`（见 build.sbt 的 app 段），开发和测试时是 dev。 */
object Build:
  val version: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("dev")
