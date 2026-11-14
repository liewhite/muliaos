package muliao.platform

import muliao.platform.db.Migrations

/** 一个领域交给组装入口的基本信息。 */
trait DomainModule:
  def name: String
  /** 本领域的表结构迁移（`resources/db/<name>/`）。 */
  final def migrations: Migrations = Migrations(name)
