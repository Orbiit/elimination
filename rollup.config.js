import elm from 'rollup-plugin-elm'
import serve from 'rollup-plugin-serve'
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
      compiler: {
        optimize: production,
        debug: !production
      }
    }),
    process.env.ROLLUP_WATCH && serve({
      contentBase: [''],
      port: 8080
    }),
    production && terser({
      sourcemap: true,
      compress: {
        pure_funcs: ['F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9'],
        pure_getters: true,
        keep_fargs: false,
        unsafe_comps: true,
        unsafe: true
      }
    })
  ],
  watch: {
    include: 'src/**'
  }
}
