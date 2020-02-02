import elm from 'rollup-plugin-elm'
import serve from 'rollup-plugin-serve'
import css from 'rollup-plugin-css-only'
import { terser } from 'rollup-plugin-terser'

const production = process.env.NODE_ENV === 'production'
const dir = production ? '.' : './dist'

export default {
  input: 'src/index.js',
  output: {
    file: `${dir}/bundle.js`,
    format: 'iife',
    sourcemap: true
  },
  plugins: [
    elm({
      exclude: 'elm_stuff/**',
      optimize: production,
      debug: !production
    }),
    process.env.ROLLUP_WATCH && serve({
      contentBase: [''],
      port: 8080
    }),
    css({
      output: `${dir}/bundle.css`
    }),
    production && terser({
      sourcemap: true
    })
  ],
  watch: {
    include: 'src/**'
  }
}
