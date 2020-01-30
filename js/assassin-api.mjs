const HOST = 'https://sheep.thingkingland.app/assassin/'

function get (path, session) {
  return fetch(HOST + path, {
    headers: {
      'X-Session-ID': session
    }
  })
    .then(r => r.ok
      ? r.json()
      : r.json()
        .then(json => Promise.reject(json)))
}

function post (path, session, body) {
  return fetch(HOST + path, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'X-Session-ID': session
    },
    body: JSON.stringify(body)
  })
    .then(r => r.ok
      ? r.json()
      : r.json()
        .then(json => Promise.reject(json)))
}

export function createUser ({ username, name, bio, password, email }) {
  return post('create-user', null, { username, name, bio, password, email })
    .then(({ session }) => new User(username, session))
}

export function login ({ username, password }) {
  return post('login', null, { username, password })
    .then(({ session }) => new User(username, session))
}

export class User {
  constructor (username, session) {
    this.username = username
    this.session = session
  }

  logout () {
    return post('logout', this.session)
      .then(() => {
        this.session = null
      })
  }

  getSettings () {
    return get('user-settings', this.session)
  }

  setSettings ({ name, bio, password, oldPassword, email }) {
    return post('user-settings', this.session, { name, bio, password, oldPassword, email })
  }

  createGame ({ name, description, password }) {
    return post('create-game', this.session, { name, description, password })
      .then(({ game }) => game)
  }

  getGameSettings (gameID) {
    return get('game-settings?game=' + gameID, this.session)
  }

  setGameSettings (gameID, { name, description, password }) {
    return post('game-settings?game=' + gameID, this.session, { name, description, password })
  }

  join (gameID, password) {
    return post('join?game=' + gameID, this.session, { password })
  }

  leave (gameID) {
    return post('leave?game=' + gameID, this.session)
  }

  kick (gameID, target) {
    return post('leave?game=' + gameID, this.session, { target })
  }

  start (gameID) {
    return post('start?game=' + gameID, this.session)
  }

  shuffle (gameID) {
    return post('shuffle?game=' + gameID, this.session)
  }

  status (gameID) {
    return get('status?game=' + gameID, this.session)
  }

  kill (gameID, code) {
    return post('kill?game=' + gameID, this.session, { code })
  }
}

export function getUser (username) {
  return get('user?user=' + username)
}

export function getGame (gameID) {
  return get('game?game=' + gameID)
}
