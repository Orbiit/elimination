import Elm from './Main.elm'

const SESSION_KEY = '[elimination] session'
const USERNAME_KEY = '[elimination] username'

// TODO: Don't shove into <body> because it messes with extensions apparently
const app = Elm.Main.init({
  flags: [localStorage.getItem(SESSION_KEY), localStorage.getItem(USERNAME_KEY)]
})
app.ports.saveSession.subscribe((session, username) => {
  localStorage.setItem(SESSION_KEY, session)
  localStorage.setItem(USERNAME_KEY, username)
})
app.ports.logout.subscribe((session, username) => {
  localStorage.removeItem(SESSION_KEY)
  localStorage.removeItem(USERNAME_KEY)
})
