import elm from 'rollup-plugin-elm'
import serve from 'rollup-plugin-serve'

const production = process.env.NODE_ENV === 'production'

export default {
  input: 'src/index.js',
  output: {
    file: 'dist/bundle.js',
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
    })
  ],
  watch: {
    include: 'src/**'
  }
}
