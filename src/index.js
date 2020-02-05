import Elm from './Main.elm'
import NProgress from 'nprogress'

const SESSION_KEY = '[elimination] session'
const USERNAME_KEY = '[elimination] username'

const app = Elm.Main.init({
  flags: [localStorage.getItem(SESSION_KEY), localStorage.getItem(USERNAME_KEY)]
})
app.ports.saveSession.subscribe(([session, username]) => {
  localStorage.setItem(SESSION_KEY, session)
  localStorage.setItem(USERNAME_KEY, username)
})
app.ports.logout.subscribe(() => {
  localStorage.removeItem(SESSION_KEY)
  localStorage.removeItem(USERNAME_KEY)
})
app.ports.start.subscribe(() => {
  NProgress.start()
})
app.ports.done.subscribe(() => {
  NProgress.done()
})
