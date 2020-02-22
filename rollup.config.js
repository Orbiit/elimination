import elm from 'rollup-plugin-elm'
import serve from 'rollup-plugin-serve'
import { terser } from 'rollup-plugin-terser'
import resolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import processEnv from './rollup-plugin-process-env.js'
import html from 'rollup-plugin-bundle-html'
import css from 'rollup-plugin-css-porter'
import path from 'path'

const production = process.env.NODE_ENV === 'production'
const dir = production ? '.' : './dist'

export default {
  input: 'src/index.js',
  output: {
    file: `${dir}/bundle.js`,
    format: 'iife',
    sourcemap: !production
  },
  plugins: [
    resolve({
      browser: true
    }),
    commonjs(),
    elm({
      exclude: 'elm_stuff/**',
      compiler: {
        optimize: production,
        debug: !production
      }
    }),
    html({
      template: 'src/template.html',
      dest: dir,
      filename: 'index.html',
      externals: [
        { type: 'css', file: 'https://fonts.googleapis.com/css?family=Playfair+Display:400,700&display=swap' }
      ],
      // It otherwise looks for all JS/CSS files in the dest folder, which is rather
      // undesirable
      ignore: `^\\./(?!${production ? '' : 'dist/'}bundle\\.(js|css))`
    }),
    css({
      raw: false,
      minified: `${dir}/bundle.css`,
      cleanCSSOptions: {
        format: production ? undefined : 'beautify',
        level: production ? 2 : 0,
        // ??? This works?
        rebaseTo: path.resolve(__dirname, production ? '..' : '../images')
      }
    }),
    processEnv(),
    process.env.ROLLUP_WATCH && serve({
      contentBase: production ? [''] : ['dist', 'images'],
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
