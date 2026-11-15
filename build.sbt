// muliao（Scala 版）构建。模块图与依赖方向见 docs/stack.md「模块与依赖方向」。
// 领域之间只能沿下面 `domain(...)` 里写的方向依赖：sbt 的模块依赖不能成环，
// 反向 import 在编译期就找不到类，所以这份列表就是依赖方向的唯一来源。

ThisBuild / scalaVersion := "3.10.1"
ThisBuild / organization := "muliao"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wunused:imports",
  "-Wvalue-discard",
  "-Werror"
)

// 各模块的测试各在自己的 JVM 里（Test / fork），sbt 默认同时只开 1 个测试 JVM，模块只能一个接一个跑；放开到 4 个，模块之间并行（#898）。
// 限制只能叠加不能放宽，所以去掉 sbt 自带的那条（规则类不公开，只能按它的说明文字认）；不整份重写，免得丢掉 Scala.js、assembly 插件加的限制。
// sbt 哪天改了这条默认值，这里认不出来，只是退回一个模块接一个跑（变慢，不影响对错）。
Global / concurrentRestrictions := (Global / concurrentRestrictions).value
  .filterNot(_.toString == s"Limit ${Tags.ForkedTestGroup.name} to 1") :+ Tags.limit(Tags.ForkedTestGroup, 4)

// quill 4.9.0 依赖 zio-json 0.10（只用于 Postgres jsonb，SQLite 用不到），允许被 1.x 取代
ThisBuild / libraryDependencySchemes += "dev.zio" %% "zio-json" % VersionScheme.Always

val V = new {
  val zio        = "2.1.27"
  val zioJson    = "1.1.2"
  val zioSchema  = "1.9.1"
  val zioProcess = "0.8.1"
  val zioLogging = "2.5.4"
  val zioHttp    = "3.11.6" // 与 tapir-zio-http-server 1.14.1 依赖的版本一致
  val tapir      = "1.14.1"
  val apispec    = "0.12.1"
  val quill      = "4.9.0"
  val sqlite     = "3.54.1.0"
  val flyway     = "13.12.2"
  val tomlj      = "2.2.0"
  val laminar    = "18.0.0-M5"
  val snakeyaml  = "3.2"
  val mcp        = "2.2.0"
  val twelvemonkeys = "3.15.3"
  val metadataExtractor = "2.22.0"
  val jsoup      = "1.24.1"
  val readability4j = "1.0.8"
  val flexmark   = "0.64.8"
  val cronUtils  = "9.2.1"
}

lazy val commonJvm = Seq(
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio"          % V.zio,
    "dev.zio" %% "zio-streams"  % V.zio,
    "dev.zio" %% "zio-test"     % V.zio % Test,
    "dev.zio" %% "zio-test-sbt" % V.zio % Test
  ),
  Test / fork := true,
  Test / javaOptions ++= Seq("-Dfile.encoding=UTF-8", "--enable-native-access=ALL-UNNAMED")
)

// ---- 前后端共用：编号、错误体、接口声明（tapir） ----
lazy val contract = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/contract"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                     %%% "zio-json"       % V.zioJson,
      "com.softwaremill.sttp.tapir" %%% "tapir-core"     % V.tapir,
      "com.softwaremill.sttp.tapir" %%% "tapir-json-zio" % V.tapir,
      "dev.zio"                     %%% "zio-schema"     % V.zioSchema,
      "dev.zio"                     %%% "zio-test"       % V.zio % Test,
      "dev.zio"                     %%% "zio-test-sbt"   % V.zio % Test
    )
  )

// ---- 平台基础：不含领域词汇 ----
lazy val platform = project
  .in(file("modules/platform"))
  .dependsOn(contract.jvm)
  .settings(commonJvm)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"                       %% "zio-http"                 % V.zioHttp,
      "dev.zio"                       %% "zio-json"                 % V.zioJson,
      "dev.zio"                       %% "zio-schema"               % V.zioSchema,
      "dev.zio"                       %% "zio-schema-json"          % V.zioSchema,
      "dev.zio"                       %% "zio-schema-derivation"    % V.zioSchema,
      "dev.zio"                       %% "zio-logging"              % V.zioLogging,
      "dev.zio"                       %% "zio-logging-slf4j2-bridge" % V.zioLogging,
      "com.softwaremill.sttp.tapir"   %% "tapir-zio-http-server"    % V.tapir,
      "com.softwaremill.sttp.tapir"   %% "tapir-openapi-docs"       % V.tapir,
      "com.softwaremill.sttp.apispec" %% "openapi-circe"            % V.apispec,
      ("dev.zio"                      %% "quill-jdbc-zio"           % V.quill)
        .excludeAll(
          ExclusionRule("com.mysql"),
          ExclusionRule("mysql"),
          ExclusionRule("com.h2database"),
          ExclusionRule("org.postgresql"),
          ExclusionRule("com.microsoft.sqlserver"),
          ExclusionRule("com.oracle.database.jdbc")
        ),
      "org.xerial"                     % "sqlite-jdbc"              % V.sqlite,
      "org.flywaydb"                   % "flyway-core"              % V.flyway,
      "org.tomlj"                      % "tomlj"                    % V.tomlj
    )
  )

// ---- 给 agent 的工具的共同契约 ----
lazy val toolkit = project
  .in(file("modules/toolkit"))
  .dependsOn(platform)
  .settings(commonJvm)

// ---- 以某身份起命令：子进程、进程组、写沙箱、secret 注入与打码 ----
lazy val exec = project
  .in(file("modules/exec"))
  .dependsOn(platform)
  .settings(commonJvm)
  .settings(libraryDependencies += "dev.zio" %% "zio-process" % V.zioProcess)

// ---- 领域：依赖方向就是这里的参数 ----
def domain(name: String, deps: ClasspathDep[ProjectReference]*): Project =
  Project(name, file(s"modules/$name"))
    .dependsOn((Seq[ClasspathDep[ProjectReference]](platform, toolkit) ++ deps)*)
    .settings(commonJvm)

lazy val resources = domain("resources", exec)                   // 资源、agent 与权限（exec：可写范围算成写沙箱）
  .settings(libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % V.snakeyaml)
lazy val models    = domain("models", exec)                      // 模型接入、MCP、外部服务
  .settings(
    libraryDependencies ++= Seq(
      "io.modelcontextprotocol.sdk" % "mcp"                          % V.mcp,               // MCP 客户端（官方 SDK）
      "com.twelvemonkeys.imageio"   % "imageio-webp"                 % V.twelvemonkeys,     // 读 WebP
      "com.drewnoakes"              % "metadata-extractor"           % V.metadataExtractor, // 照片 EXIF 方向
      "org.jsoup"                   % "jsoup"                        % V.jsoup,             // 直连抓取：解析 HTML
      "net.dankito.readability4j"   % "readability4j"                % V.readability4j,     // 直连抓取：提取正文
      "com.vladsch.flexmark"        % "flexmark-html2md-converter"   % V.flexmark           // 直连抓取：正文转 Markdown
    )
  )
lazy val sessions  = domain("sessions", resources, models)       // 会话与运行
lazy val workspace = domain("workspace", sessions % "compile->compile;test->test", exec)         // 文件、命令、后台任务、页面
lazy val notifications = domain("notifications", sessions % "compile->compile;test->test")                 // 通知与推送
lazy val board     = domain("board", sessions % "compile->compile;test->test", notifications) // issue 看板
lazy val triggers  = domain("triggers", sessions % "compile->compile;test->test", notifications) // 触发器
  .settings(libraryDependencies += "com.cronutils" % "cron-utils" % V.cronUtils)
lazy val history   = domain("history", sessions % "compile->compile;test->test", board) // 工作历史与活动回顾

lazy val domains = Seq(resources, models, sessions, workspace, notifications, board, triggers, history)

// ---- 组装入口：唯一依赖全部领域的模块 ----
lazy val app = project
  .in(file("modules/app"))
  .dependsOn(domains.map(p => p: ClasspathDep[ProjectReference])*)
  .dependsOn(sessions % "test->test") // 集成测试用假模型
  .settings(commonJvm)
  .settings(
    Compile / mainClass := Some("muliao.app.Main"),
    run / fork          := true,
    // ---- 打包（#877，由 deploy/build.sh 调用）：fat jar 里带上前端（资源路径 web/），清单写版本与本地访问许可 ----
    assembly / assemblyJarName := "muliao.jar",
    // 前端由 deploy/build.sh 先用 Vite 构建到 target/web-dist/web/；这里只把它当成一个类路径目录收进 jar
    assembly / fullClasspath += Attributed.blank(target.value / "web-dist"),
    assembly / packageOptions += Package.ManifestAttributes(
      "Implementation-Version" -> sys.props.getOrElse("muliao.version", "dev"),
      "Enable-Native-Access"   -> "ALL-UNNAMED" // sqlite-jdbc 等的本地库，免得 JDK 25 每次启动警告
    ),
    // 只在打包时出现的重名文件。io/getquill：dev.zio:quill-sql 4.9.0 自带了一份 io.getquill:quill-engine 4.8.5 里的类，
    // 开发时类路径上 quill-sql 在前，所以取第一个与开发时一致；其余是清单、版本说明、编译期配置，取哪份都不影响运行。
    // scalafmt 与 Scala 2.13 编译器（quill 带进来的）不排除：quill 运行时格式化 SQL 可能用到，省下的体积不值得冒找不到类的险。
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class")                            => MergeStrategy.discard
      case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
      case PathList("META-INF", xs @ _*) if xs.lastOption.contains("MANIFEST.MF") && xs.contains("OSGI-INF") => MergeStrategy.discard
      case PathList("META-INF", "io.netty.versions.properties")     => MergeStrategy.first
      case PathList("io", "getquill", _*)                           => MergeStrategy.first
      case PathList("scala", "annotation", f) if f.startsWith("unroll.") => MergeStrategy.first
      case "deriving.conf"                                          => MergeStrategy.first
      case x =>
        val old = (assembly / assemblyMergeStrategy).value
        old(x)
    }
  )

// ---- 前端（Scala.js + Laminar），只依赖接口契约 ----
// 由 modules/web 下的 Vite 驱动（@scala-js/vite-plugin-scalajs）：npm 依赖（Tailwind、HeroUI 样式、Markdown 等）由 Vite 打包。
lazy val web = project
  .in(file("modules/web"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(contract.js)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    // contract 的 zio-schema（给工具参数用）经 zio-prelude 带进时区库，约 600KB：前端不用 zio-schema、也不解析时区，排除（#890 实测前端包 +14%）
    excludeDependencies += ExclusionRule("io.github.cquiroz", "scala-java-time-tzdb_sjs1_3"),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule).withESFeatures(_.withESVersion(org.scalajs.linker.interface.ESVersion.ES2018))), // ES2018：正则的后行断言与 \p{L}
    libraryDependencies ++= Seq(
      "com.raquo"                   %%% "laminar"            % V.laminar,
      "com.softwaremill.sttp.tapir" %%% "tapir-sttp-client4" % V.tapir,
      "dev.zio"                     %%% "zio-test"           % V.zio % Test,
      "dev.zio"                     %%% "zio-test-sbt"       % V.zio % Test
    )
  )

lazy val root = project
  .in(file("."))
  .aggregate((Seq[ProjectReference](contract.jvm, contract.js, platform, toolkit, exec, app, web) ++ domains.map(p => p: ProjectReference))*)
  .settings(publish / skip := true)
