import Elm from './Main.elm'
import NProgress from 'nprogress'
import ClipboardJS from 'clipboard'
import '../js/tab-key-focus.js'

import '../css/nprogress.css'
import '../css/main.css'
import '../css/profile.css'
import '../css/settings.css'
import '../css/text-page.css'
import '../css/welcome-page.css'
import '../css/front-page.css'
import '../css/errors.css'

const SESSION_KEY = '[elimination] session'
const USERNAME_KEY = '[elimination] username'

const app = Elm.Main.init({
  flags: [
    process.env.NODE_ENV === 'production' ? 'https://sheep.thingkingland.app/assassin/' : 'http://localhost:3000/assassin/',
    localStorage.getItem(SESSION_KEY),
    localStorage.getItem(USERNAME_KEY)
  ]
})

app.ports.saveSession.subscribe(([session, username]) => {
  localStorage.setItem(SESSION_KEY, session)
  localStorage.setItem(USERNAME_KEY, username)
})
app.ports.logout.subscribe(() => {
  localStorage.removeItem(SESSION_KEY)
  localStorage.removeItem(USERNAME_KEY)
})

let preventUnload
window.addEventListener('beforeunload', e => {
  preventUnload = false
  app.ports.onBeforeUnload.send(null)
  if (preventUnload) {
    e.preventDefault()
    e.returnValue = ''
  }
})
app.ports.preventUnload.subscribe(() => {
  preventUnload = true
})

app.ports.start.subscribe(() => {
  NProgress.start()
})
app.ports.done.subscribe(() => {
  NProgress.done()
})

const clipboard = new ClipboardJS('.copy-btn')
