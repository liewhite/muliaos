# Security policy

[中文](#安全策略)

## Reporting a vulnerability

Please report vulnerabilities privately through GitHub: open the repository's **Security** tab and choose **Report a vulnerability**.
Do not open a public issue. Only the latest `main` is supported; fixes land there.

## Threat model

muliao is built for one person on their own Mac. Knowing what it does and does not protect helps you judge whether something is a vulnerability.

What muliao aims to guarantee:

- Only someone with the login password can use the web UI and API (login attempts are rate-limited per source).
- Secret values do not appear in session records, logs, the UI or error messages; injected secrets are masked in command output;
  there is no tool that returns a secret's plain text.
- Passkey-locked secrets are only decrypted in your browser after you approve; signing-key secrets never leave the browser.
  The web page itself is served by the Mac running muliao, so an attacker who controls that Mac can replace it and capture the key
  the next time you confirm with your passkey: the guarantee is "a compromise still needs one more confirmation from you",
  not "a compromise can never get the key".
- Agent pages and downloaded workspace files are treated as untrusted: pages run in an isolated origin without your cookie or API access.
- Agent commands cannot write outside the directories their permissions allow (macOS write sandbox).

What muliao does **not** try to do:

- **Contain a malicious agent.** Agents run commands as your macOS user. The sandbox restricts writes only; agents can read anything your
  user can and use the network. Roles and rules prevent mistakes and keep context small; they are not an isolation boundary.
- **Defend against prompt injection.** Agents are instructed to treat web pages, messages and files as data, but a model can still be misled.
  Give agents only the secrets, servers and permissions you would trust them with, and require approval (`approval:` in `SKILL.md`) for scripts with serious side effects.
- **Serve multiple users.** There is one account and one password.

Reports about the guarantees above are welcome. Reports that an agent can read files or reach the network are expected behaviour.

---

# 安全策略

## 报告漏洞

请经 GitHub 私下报告：打开仓库的 **Security** 页，点 **Report a vulnerability**。不要开公开 issue。只维护最新的 `main`，修复也合到那里。

## 威胁模型

muliao 是给一个人在自己的 Mac 上用的。弄清它保护什么、不保护什么，才好判断算不算漏洞。

muliao 力求保证：

- 只有知道登录密码的人能用网页界面和接口（登录失败按来源退避）。
- secret 的值不出现在会话记录、日志、界面和错误信息里；注入命令的 secret 在输出里打码；没有能读出 secret 明文的工具。
- passkey 加密的 secret 只在你批准后在你的浏览器里解开；签名钥匙的私钥不出浏览器。网页本身由跑 muliao 的那台 Mac 提供，
  控制了那台 Mac 的攻击者可以换掉网页，在你下一次用 passkey 确认时拿走私钥：保证的是「被攻破后还要等你再确认一次」，不是「被攻破也拿不到」。
- agent 页面和下载的工作目录文件按不可信内容处理：页面跑在隔离的源上，拿不到你的 cookie，也调不了接口。
- agent 的命令写不了权限以外的目录（macOS 写沙箱）。

muliao **不**试图做到：

- **关住恶意的 agent。** agent 以你的 macOS 用户身份跑命令。沙箱只限制写；你的用户能读的它都能读，也能联网。角色和规则用来防误操作、省上下文，不是隔离边界。
- **防住提示注入。** agent 被要求把网页、消息、文件当数据，但模型仍可能被误导。只给 agent 你信得过的 secret、服务器和权限；副作用大的脚本在 `SKILL.md` 的 `approval:` 里要求批准。
- **多用户。** 只有一个账号、一个密码。

欢迎报告违反上述保证的问题；「agent 能读文件、能联网」属于预期行为。
