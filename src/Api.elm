module Api exposing (..)

import Json.Decode as D
import Json.Encode as E
import Http

import Utils
import Pages

type alias SessionID = String

type Session
  = SignedIn { session : SessionID, username : String }
  | SignedOut

type PageCmd
  = ChangeSession Session
  | ChangePage Pages.Page
  | Batch (List PageCmd)
  | None

sessionCouldExpire : Utils.HttpError -> PageCmd
sessionCouldExpire (_, error) =
  if String.startsWith "(Invalid session)" error then
    ChangeSession SignedOut
  else
    None

type alias UserInfo =
  { username : String
  , name : String
  , password : String
  , email : String
  , bio : String
  }

-- Authenticate

createUser : UserInfo -> (String -> Result Utils.HttpError SessionID -> msg) -> Cmd msg
createUser info msg =
  Utils.post "create-user" Nothing (msg info.username) (E.object
    [ ("username", E.string info.username)
    , ("name", E.string info.name)
    , ("password", E.string info.password)
    , ("email", E.string info.email)
    ])
    (D.field "session" D.string)

login : String -> String -> (String -> Result Utils.HttpError SessionID -> msg) -> Cmd msg
login username password msg =
  Utils.post "login" Nothing (msg username) (E.object
    [ ("username", E.string username)
    , ("password", E.string password)
    ])
    (D.field "session" D.string)

-- Requires authentication

logout : SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
logout session msg =
  Utils.post "logout" (Just session) msg (E.object []) (D.succeed ())

type alias UserSettingsInfo =
  { name : String
  , email : String
  , bio : String
  }

getSettings : SessionID -> (Result Utils.HttpError UserSettingsInfo -> msg) -> Cmd msg
getSettings session msg =
  Utils.get "user-settings" (Just session) msg <|
    D.map3 UserSettingsInfo
      (D.field "name" D.string)
      (D.field "email" D.string)
      (D.field "bio" D.string)

setSettings : E.Value -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
setSettings changes session msg =
  Utils.post "user-settings" (Just session) msg changes (D.succeed ())

type alias GameInfo =
  { name : String
  , description : String
  , password : String
  }

type alias GameID = String

createGame : GameInfo -> SessionID -> (Result Utils.HttpError GameID -> msg) -> Cmd msg
createGame gameInfo session msg =
  Utils.post "create-game" (Just session) msg (E.object
    [ ("name", E.string gameInfo.name)
    , ("description", E.string gameInfo.description)
    , ("password", E.string gameInfo.password)
    ])
    (D.field "game" D.string)

type alias GameSettingsPlayer =
  { username : String
  , name : String
  , alive : Bool
  , kills : Int
  , joined : Int
  }

type alias GameSettingsInfo =
  { name : String
  , description : String
  , password : String
  , players : List GameSettingsPlayer
  , started : Bool
  , ended : Bool
  }

getGameSettings : GameID -> SessionID -> (Result Utils.HttpError GameSettingsInfo -> msg) -> Cmd msg
getGameSettings game session msg =
  Utils.get ("game-settings?game=" ++ game) (Just session) msg <|
    D.map6 GameSettingsInfo
      (D.field "name" D.string)
      (D.field "description" D.string)
      (D.field "password" D.string)
      (D.field "players" (D.list (D.map5 GameSettingsPlayer
        (D.field "username" D.string)
        (D.field "name" D.string)
        (D.field "alive" D.bool)
        (D.field "kills" D.int)
        (D.field "joined" D.int)
      )))
      (D.field "started" D.bool)
      (D.field "ended" D.bool)

setGameSettings : E.Value -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
setGameSettings changes game session msg =
  Utils.post ("game-settings?game=" ++ game) (Just session) msg changes (D.succeed ())

join : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
join password game session msg =
  Utils.post ("join?game=" ++ game) (Just session) msg (E.object [("password", E.string password)]) (D.succeed ())

leave : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
leave game session msg =
  Utils.post ("leave?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

kick : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
kick target game session msg =
  Utils.post ("kick?game=" ++ game) (Just session) msg (E.object [("target", E.string target)]) (D.succeed ())

start : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
start game session msg =
  Utils.post ("start?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

shuffle : GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
shuffle game session msg =
  Utils.post ("shuffle?game=" ++ game) (Just session) msg (E.object []) (D.succeed ())

type alias Status =
  { target : String
  , targetName : String
  , code : String
  }

statusDecoder : D.Decoder Status
statusDecoder =
  D.map3 Status
    (D.field "target" D.string)
    (D.field "targetName" D.string)
    (D.field "code" D.string)

status : GameID -> SessionID -> (Result Utils.HttpError Status -> msg) -> Cmd msg
status game session msg =
  Utils.get ("status?game=" ++ game) (Just session) msg statusDecoder

statuses : SessionID -> (Result Utils.HttpError (List Status) -> msg) -> Cmd msg
statuses session msg =
  Utils.get "statuses" (Just session) msg (D.list statusDecoder)

kill : String -> GameID -> SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
kill code game session msg =
  Utils.post ("kill?game=" ++ game) (Just session) msg (E.object [("code", E.string code)]) (D.succeed ())

type Notification
  = GameStarted String String
  | GameEnded String String String String
  | Killed String String String String
  | Kicked String String String

type alias NotificationMessage =
  { time : Int
  , read : Bool
  , message: Notification
  }

type alias NotificationResult =
  { end : Bool
  , notifications : List NotificationMessage
  }

notifications : Int -> Int -> SessionID -> (Result Utils.HttpError NotificationResult -> msg) -> Cmd msg
notifications from to session msg =
  Utils.get "statuses" (Just session) msg (D.map2 NotificationResult
    (D.field "end" D.bool)
    (D.field "notifications" (D.list (D.map3 NotificationMessage
      (D.field "time" D.int)
      (D.field "read" D.bool)
      (D.oneOf
        [ D.map2 GameStarted
          (D.field "game" D.string)
          (D.field "gameName" D.string)
        , D.map4 GameEnded
          (D.field "game" D.string)
          (D.field "gameName" D.string)
          (D.field "winner" D.string)
          (D.field "winnerName" D.string)
        , D.map4 Killed
          (D.field "game" D.string)
          (D.field "gameName" D.string)
          (D.field "by" D.string)
          (D.field "name" D.string)
        , D.map3 Kicked
          (D.field "game" D.string)
          (D.field "gameName" D.string)
          (D.field "reason" D.string)
        ]
      )
    )))
  )

read : SessionID -> (Result Utils.HttpError () -> msg) -> Cmd msg
read session msg =
  Utils.post "read" (Just session) msg (E.object []) (D.succeed ())

-- Public

type alias UserMyGame =
  { game : String
  , name : String
  , started : Bool
  , ended : Bool
  , players : Int
  }

type alias UserGame =
  { game : String
  , name : String
  , started : Bool
  , ended : Bool
  , players : Int
  , kills : Int
  , alive : Bool
  }

type alias User =
  { name : String
  , bio : String
  , myGames : List UserMyGame
  , games : List UserGame
  }

type alias Game =
  { name : String
  , description : String
  }

getUser : String -> (Result Utils.HttpError User -> msg) -> Cmd msg
getUser user msg =
  Utils.get ("user?user=" ++ user) Nothing msg <|
    D.map4 User
      (D.field "name" D.string)
      (D.field "bio" D.string)
      (D.field "myGames" (D.list (D.map5 UserMyGame
        (D.field "game" D.string)
        (D.field "name" D.string)
        (D.field "started" D.bool)
        (D.field "ended" D.bool)
        (D.field "players" D.int)
      )))
      (D.field "myGames" (D.list (D.map7 UserGame
        (D.field "game" D.string)
        (D.field "name" D.string)
        (D.field "started" D.bool)
        (D.field "ended" D.bool)
        (D.field "players" D.int)
        (D.field "kills" D.int)
        (D.field "alive" D.bool)
      )))

getGame : GameID -> (Result Utils.HttpError Game -> msg) -> Cmd msg
getGame game msg =
  Utils.get ("game?game=" ++ game) Nothing msg <|
    D.map2 Game
      (D.field "name" D.string)
      (D.field "description" D.string)

type alias Stats =
  { kills : Int
  , active : Int
  , games : Int
  }

getStats : (Result Utils.HttpError Stats -> msg) -> Cmd msg
getStats msg =
  Utils.get "stats" Nothing msg <|
    D.map3 Stats
      (D.field "kills" D.int)
      (D.field "active" D.int)
      (D.field "games" D.int)
