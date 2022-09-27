import { defineConfig } from 'vite'
import elm from 'vite-plugin-elm'

export default defineConfig({
  build: {outDir: "docs" },
  base: "/moving-motivators/",
  plugins: [elm()]
})

