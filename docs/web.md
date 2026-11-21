# 前端（modules/web，#875）

Scala.js + Laminar，外观沿用现有 muliao 界面（HeroUI v3 的样式类 + Tailwind）。只依赖 contract（JS 版），接口一律用 contract 里的 tapir 声明调用。

## 构建与开发
- 目录：`modules/web/`（Vite 项目）：`index.html`、`main.js`（引入样式与 Scala.js 输出）、`styles.css`（Tailwind + `@heroui/styles` + KaTeX；`@source` 扫 Scala 源码里的类名）、`public/`（图标、manifest、`sw.js`）、`src/main/scala/muliao/web/`。
- 首次：`cd modules/web && npm install`。构建：`npx vite build`（Vite 插件 `@scala-js/vite-plugin-scalajs` 会调 sbt 链接 `web`，产物在 `modules/web/dist`）。
- 本地看效果：`dev-data/`（已 gitignore）放开发配置，`[server] web_dir` 指向 `modules/web/dist`，起服务 `dev-data/serve.sh`（见 docs/stack.md「工具链与开发」）；改完 `npx vite build` 刷新页面即可。也可 `MULIAO_BACKEND=http://127.0.0.1:<端口> npx vite --port <端口>` 起热更新开发服务（转发 /api、/site）。
- 只编 Scala：`sbt --client web/fastLinkJS`（`-Werror`：未用的 import、丢弃的值都是错误）。
- npm 依赖经 `@JSImport` 门面使用，门面集中在 `core/Js.scala`（lucide 图标、marked、DOMPurify、highlight.js、dagre）。

## 结构
| 包 | 内容 |
|---|---|
| `core` | `Http`（调接口的唯一入口，失败归为 `Failure`，401 回登录页；`Http.href(接口)(输入)` 给浏览器自己取的地址：下载、图片、实时连接）、`Auth`（登录状态）、`Live`（实时连接：`Live.frames(contract 里的 Feed)(输入)`，ready/ping、看门狗、后台一分钟断开，事件已解码）、`Events`（全局「数据变了」；`outdated`：重连时 ready 帧带的服务端版本和第一次连上时不同，外壳顶上提示刷新）、`Remote`/`Load`（随主题刷新的远端数据）、`Router`/`Route`（地址与页面，按布局分层：会话分屏、agent 分屏、整页 `Page`；参数是类型化的，`Route.parse`/`url` 互逆，有往返测试）、`Markdown`、`Highlight`（代码高亮）、`Format`（时间、时长、token 缩写、模型名的写法，每种只写一份）、`Clock`（每隔一段时间一跳的当前时刻）、`Theme`（深浅色：设置里三选一跟随系统/浅色/深色，存本设备 localStorage `muliao.theme`；index.html 内联脚本按同一个键先设首屏）、`Js` 门面 |
| `ui` | 组件：`Ui`（按钮、卡片与可点的列表卡片 `linkCard`、Chip、行内代码、页面框架 `page`、折叠与可折叠卡片 `collapsibleCard`、提示框 `alert`、`loading`、复制）、`Fields`（输入框、搜索框、文本域、下拉、分段切换、开关）、`AgentPicker`（所有选 agent 的地方：分组、可搜索，值类型由调用方定，可加「平台」「我」等固定项）、`Dropdown`（上两者共用的下拉壳：按钮、弹层、点外面或 Esc 收起）、`Dialog`（居中的弹窗，详情与确认共用）、`Overlay`（确认弹窗 `Confirm.ask`、`Toast`、左侧抽屉）、`Fold`（虚线折叠块与展开箭头）、`JobLog`（后台任务日志尾部）、`Action`（用户操作：busy、错误、成功后刷新；不关心结果用 `fire`）、`AgentsData`（全站一份的 agent 列表 `all`/`list`，外壳一直订阅着、失败时外壳顶上提示；按编号/名字取 agent 都走它）与 `AgentChip`（链到详情的 agent 名）、`Owner`（资源归属的平台/agent 名）、`Problems`（没加载的资源文件）、`Todo`（等用户处理的一件事的卡片外壳：头部、内容、几个选择；工具批准、提问、passkey 授权都用它，会话页、Secret 页、通知页共用，#1097）、`SecretAsks`（passkey 授权请求做成的卡片） |
| `shell` | 登录页、登录后的外壳（导航、管理菜单、列表+详情分屏、按 Route 选页面） |
| `pwa` | Service Worker 注册、推送订阅、点通知跳转（SwNavigate）、应用角标 |
| `pages/<领域>` | 各页面；外壳只调用每页的入口（`XxxPage.view()` 等，见 `shell/Shell.scala`） |

## 写页面的约定
- **数据**：`Remote(接口)(参数)` 得到 `Signal[Load[A]]`：挂上时拉、全局 SSE 连上/重连、接口声明的主题变了、用户刚做完操作时再拉。主题是 contract 的 `enum Topic`，跟着哪些主题刷新写在 contract 的接口声明上（`.refreshedBy(Topic.Issues, ...)`），调用点不写；要自己组织请求（连续翻页）或加别的重拉时机（「显示更多」、手动刷新）用 `Remote.following(接口, more)(fetch)`。要变换数据用 `.map(_.map(f))`。用 `Ui.loading(signal)(data => ...)` 显示：`render` 只调用一次、拿到数据的 Signal，刷新不重建 DOM。
- **列表**用 `children <-- signal.splitSeq(_.key)(strict => ...)`（Airstream 18 写法）按键复用元素；单个随路由变化的子页面用 `.distinct.map(...)`。
- **编辑表单不被刷新冲掉**：表单的草稿放在自己的 `Var` 里，只在打开（或切到另一条记录）时从数据初始化一次；提交时只发改过的字段（接口的 Patch 类型字段都是 Option）。
- **操作**：`val act = new Action`；`act.run(Http.call(...)(...))` 成功后自动让正在显示的数据重拉；`act.errorView()` 显示错误；按钮 `Ui.btn(pending = act.busy.signal)`。危险操作先 `Confirm.ask(..., danger = true)`。
- **跳转**：站内链接用 `Ui.appLink(Route.x(...).url)`（默认把当前页记为来源，目标页的返回回到这里）；页面返回按钮 `Ui.page(title, back = Some(Route.x))`。地址里的参数（筛选条件、编号）是 `Route`/`Page` 的字段：外壳把它们的 Signal 交给页面（改筛选不重建页面），页面改参数用 `Router.update(新的 Route)`；不手拼地址、不读查询串。加带参数的页面：在 `Page` 加分支，改 `Route.url`/`parse` 并在 `RouteSpec` 加一例。
- **样式**：沿用现有界面的 HeroUI 类（`button button--primary button--sm`、`card`、`chip chip--soft chip--accent`、`alert alert--danger`…）与 Tailwind 类，对照现有界面（http://127.0.0.1:1080，只看不改）的同一页面。图标 `Icon("lucide 导出名", "size-4")`。
- **枚举的显示文字**：每种只写一份：接口里的枚举（issue 状态、优先级）的 `given Shown` 在 `core/Shown.scala`，页面自己的枚举继承 `Shown.Self`（带 `label`）；用 `x.shown`、`Fields.segmented(..., 枚举.values.toList)`、`Opt.of(值列表)`。跨刷新记住的选择用 `Stored.choice` / `Stored.text`（放在页面对象的字段里）。
- 逻辑尽量写成纯函数与 ADT（`enum`、穷尽匹配），把接口里的字符串状态（如 `state: "running"`）在页面里先转成 enum 再用。
