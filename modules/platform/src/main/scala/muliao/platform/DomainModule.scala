package muliao.platform

import muliao.platform.config.ConfigSection
import muliao.platform.db.Migrations

/** 一个领域交给组装入口的基本信息。 */
trait DomainModule:
  def name: String
  /** 本领域的表结构迁移（`resources/db/<name>/`）。 */
  final def migrations: Migrations = Migrations(name)
  /** 本领域声明的配置段（配置文件里允许出现的段由各领域的段派生）。 */
  def sections: List[ConfigSection[?]] = Nil
  /** 配了才有的功能的配置段（文件里没有就是没这个功能）。 */
  def optionalSections: List[ConfigSection[?]] = Nil
