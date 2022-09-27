import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig({
  outDir: "docs",
  base: "/moving-motivators/",
  plugins: [elm()]
})

