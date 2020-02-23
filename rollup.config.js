import elm from 'rollup-plugin-elm'
import serve from 'rollup-plugin-serve'
import { terser } from 'rollup-plugin-terser'
import resolve from '@rollup/plugin-node-resolve'
import commonjs from '@rollup/plugin-commonjs'
import processEnv from './rollup-plugin-process-env.js'
import html from 'rollup-plugin-bundle-html'
import CleanCSS from 'clean-css'
import path from 'path'
import fse from 'fs-extra'
import { createFilter } from 'rollup-pluginutils'

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
    // Based on
    // https://github.com/RJHwang/rollup-plugin-css-porter/blob/master/src/index.js
    // and
    // https://github.com/Evercoder/rollup-plugin-css-bundle/blob/master/src/index.js
    // to preserve override order and be able to minify
    (() => {
      const styles = {}
      const minifier = new CleanCSS({
        format: production ? undefined : 'beautify',
        level: production ? 2 : 0,
        // ??? This works?
        rebaseTo: path.resolve(__dirname, production ? '..' : '../images')
      })
      const cssFilter = createFilter('**/*.css')
      return {
        name: 'CSS bundlifier',
        transform (code, id) {
          if (!cssFilter(id)) return
          styles[id] = code
          return ''
        },
        generateBundle (opts, bundle) {
          for (const [file, { modules }] of Object.entries(bundle)) {
            const moduleNames = Array.isArray(modules) ? modules : Object.keys(modules)
            const css = Object.entries(styles)
              .sort(([a], [b]) => moduleNames.indexOf(a) - moduleNames.indexOf(b))
              .map(([_, css]) => css)
              .join('\n')
            const output = minifier.minify(css)
            if (output.errors.length) throw new Error(output.errors.join('\n'))
            if (output.warnings.length) console.warn(output.warnings.join('\n'))
            fse.outputFile(
              path.join(
                path.dirname(opts.file),
                path.basename(file, path.extname(opts.file)) + '.css'
              ),
              output.styles
            )
          }
        }
      }
    })(),
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
    include: ['src/**', 'css/**']
  }
}
