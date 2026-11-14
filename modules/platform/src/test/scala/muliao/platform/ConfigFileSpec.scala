package muliao.platform

import muliao.platform.config.{ConfigCodecs, ConfigFile, ConfigSection}
import zio.*
import zio.json.*
import zio.test.*

import java.nio.file.{Files, Path}

object ConfigFileSpec extends ZIOSpecDefault:

  @jsonMemberNames(SnakeCase)
  final case class Inner(maxItems: Int = 3, label: Option[String] = None) derives JsonCodec
  @jsonMemberNames(SnakeCase)
  final case class Demo(name: String, port: Int = 80, inner: Inner = Inner(), tags: List[String] = Nil) derives JsonCodec

  val demo = ConfigSection[Demo]("demo")

  def write(content: String): Task[Path] =
    ZIO.attempt {
      val f = Files.createTempFile("muliao-config", ".toml")
      Files.writeString(f, content)
      f
    }

  def load(content: String, sections: Set[String] = Set("demo")) =
    write(content).orDie.flatMap(ConfigFile.load(_, sections))

  def spec = suite("ConfigFile")(
    test("按段解码，缺省字段取默认值") {
      for
        f <- load("[demo]\nname = \"x\"\n[demo.inner]\nmax_items = 5\n")
        d <- f.section(demo)
      yield assertTrue(d == Demo("x", 80, Inner(5, None), Nil))
    },
    test("未知的顶层段报错") {
      for e <- load("[demo]\nname = \"x\"\n[other]\na = 1\n").flip
      yield assertTrue(e.message.contains("未知的段"), e.message.contains("other"))
    },
    test("段内（含嵌套）未知的键报错并指出位置") {
      for
        f  <- load("[demo]\nname = \"x\"\ntypo = 1\n[demo.inner]\nmax_itemz = 5\n")
        e  <- f.section(demo).flip
      yield assertTrue(e.message.contains("demo.typo"), e.message.contains("demo.inner.max_itemz"))
    },
    test("缺必填项报错") {
      for
        f <- load("[demo]\nport = 1\n")
        e <- f.section(demo).flip
      yield assertTrue(e.message.contains("name"))
    },
    test("文件里没有的段按空表解码") {
      for
        f <- load("", Set("demo"))
        e <- f.section(demo).flip
      yield assertTrue(e.message.contains("[demo]"))
    },
    test("不是合法 TOML 报错") {
      for e <- load("[demo\nname=").flip
      yield assertTrue(e.message.contains("TOML"))
    },
    test("路径开头的 ~ 换成用户主目录") {
      val home = java.lang.System.getProperty("user.home")
      assertTrue(ConfigCodecs.expandHome("~/a/b").toString == s"$home/a/b", ConfigCodecs.expandHome("/x/~").toString == "/x/~")
    },
    test("平台自带的段能解码") {
      for
        f <- load(
               "[general]\ndata_dir = \"/tmp/d\"\nassets_dir = \"/tmp/a\"\ntimezone = \"UTC\"\n[log]\nlevel = \"WARN\"\n",
               Set("general", "log")
             )
        g <- f.section(GeneralSettings.section)
        l <- f.section(log.LogSettings.section)
      yield assertTrue(g.databaseFile.toString == "/tmp/d/muliao.db", g.timezone.getId == "UTC", l.level == "WARN", l.keep == 5)
    },
    test("时区不对报错") {
      for
        f <- load("[general]\ndata_dir = \"/d\"\nassets_dir = \"/a\"\ntimezone = \"Mars/X\"\n", Set("general"))
        e <- f.section(GeneralSettings.section).flip
      yield assertTrue(e.message.contains("timezone"))
    }
  )
