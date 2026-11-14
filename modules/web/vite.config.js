import scalaJSPlugin from '@scala-js/vite-plugin-scalajs'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

// 开发时把接口转发到本地起的 Scala 版服务（端口见 dev-data 的配置）
const backend = process.env.MULIAO_BACKEND ?? 'http://127.0.0.1:18080'

export default defineConfig({
  plugins: [scalaJSPlugin({ cwd: '../..', projectID: 'web' }), tailwindcss()],
  server: {
    proxy: {
      '/api': { target: backend, changeOrigin: false },
      '/site': { target: backend, changeOrigin: false },
    },
  },
})
