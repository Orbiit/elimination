// https://github.com/rollup/rollup/issues/487

import MagicString from 'magic-string'

// The virtual id for our shared "process" mock. We prefix it with \0 so that other plugins ignore it
const INJECT_PROCESS_MODULE_ID = '\0inject-process'

export default () => ({
  name: 'inject-process-plugin',
  resolveId (id) {
    // this tells Rollup not to try to resolve imports from our virtual id
    if (id === INJECT_PROCESS_MODULE_ID) {
      return INJECT_PROCESS_MODULE_ID
    }
  },
  load (id) {
    if (id === INJECT_PROCESS_MODULE_ID) {
      // All fields of 'process' we want to mock are top level exports of the module.
      // For now I hardcoded "NODE_ENV" but you probably want to put more logic here.
      return `export const env = ${JSON.stringify(process.env)};\n`
    }
  },
  transform (code, id) {
    // Each module except our virtual module gets the process mock injected.
    // Tree-shaking will make sure the import is removed from most modules later.
    // This will produce invalid code if a module defines a top-level "process" variable, so beware!
    if (id !== INJECT_PROCESS_MODULE_ID) {
      const magicString = new MagicString(code)
      magicString.prepend(`import * as process from '${INJECT_PROCESS_MODULE_ID}';\n`)
      return { code: magicString.toString(), map: magicString.generateMap({ hires: true }) }
    }
  }
})
