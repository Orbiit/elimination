module Api exposing (..)

import Json.Decode as D
import Json.Encode as E
import Http
import Time
import Url.Builder as Builder
import Dict exposing (Dict)

import Utils
import Utils.Request as Request
import Utils.HumanTime as HumanTime
import Pages

type alias SessionID = String

type Session
  = SignedIn { session : SessionID, username : String }
  | SignedOut

type alias GlobalModel m =
  { m
  | session : Session
  , zone : Time.Zone
  , host : String
  }

type PageCmd
  = ChangeSession Session
  | ChangePage Pages.Page
  | Redirect String
  | Batch (List PageCmd)
  | None

sessionCouldExpire : Request.HttpError -> PageCmd
sessionCouldExpire (_, error) =
  if String.endsWith "(Invalid session)" error then
    ChangeSession SignedOut
  else
    None

type alias Timestamp = Int

type GameState
  = WillStart
  | Started
  | Ended

gameStateParser : D.Decoder GameState
gameStateParser =
  D.string
    |> D.andThen
      (\string ->
        case string of
          "starting" ->
            D.succeed WillStart
          "started" ->
            D.succeed Started
          "ended" ->
            D.succeed Ended
          _ ->
            D.fail "Invalid game state")

gameStateName : GameState -> String
gameStateName state =
  case state of
    WillStart ->
      "Awaiting players"
    Started ->
      "Ongoing"
    Ended ->
      "Ended"

gameStateNameWithTime : Time.Zone -> GameState -> Timestamp -> String
gameStateNameWithTime zone state time =
  (case state of
    WillStart ->
      "Awaiting players since "
    Started ->
      "Ongoing since "
    Ended ->
      "Ended on ")
    ++ HumanTime.display zone time

type alias Response a = Result Request.HttpError a
type alias ResultMsg a msg = Response a -> msg

get : GlobalModel m -> String -> ResultMsg a msg -> D.Decoder a -> Cmd msg
get global path msg decoder =
  let
    sessionID =
      case global.session of
        SignedIn { session } ->
          Just session
        SignedOut ->
          Nothing
  in
  Request.request Request.Get (global.host ++ path) sessionID msg Nothing decoder

post : GlobalModel m -> String -> ResultMsg a msg -> E.Value -> D.Decoder a -> Cmd msg
post global path msg body decoder =
  let
    sessionID =
      case global.session of
        SignedIn { session } ->
          Just session
        SignedOut ->
          Nothing
  in
  Request.request Request.Post (global.host ++ path) sessionID msg (Just body) decoder

-- Authenticate

type alias UserInfo =
  { username : String
  , name : String
  , password : String
  , email : String
  , bio : String
  }

createUser : GlobalModel m -> (String -> ResultMsg SessionID msg) -> UserInfo -> Cmd msg
createUser global msg info =
  post global "create-user" (msg info.username) (E.object
    [ ("username", E.string info.username)
    , ("name", E.string info.name)
    , ("password", E.string info.password)
    , ("email", E.string info.email)
    ])
    (D.field "session" D.string)

login : GlobalModel m -> (String -> ResultMsg SessionID msg) -> String -> String -> Cmd msg
login global msg username password =
  post global "login" (msg username) (E.object
    [ ("username", E.string username)
    , ("password", E.string password)
    ])
    (D.field "session" D.string)

-- Requires authentication

logout : GlobalModel m -> ResultMsg () msg -> Cmd msg
logout global msg =
  post global "logout" msg (E.object []) (D.succeed ())

type alias UserSettingsInfo =
  { name : String
  , email : String
  , bio : String
  }

getSettings : GlobalModel m -> ResultMsg UserSettingsInfo msg -> Cmd msg
getSettings global msg =
  get global "user-settings" msg <|
    D.map3 UserSettingsInfo
      (D.field "name" D.string)
      (D.field "email" D.string)
      (D.field "bio" D.string)

setSettings : GlobalModel m -> ResultMsg () msg -> E.Value -> Cmd msg
setSettings global msg changes =
  post global "user-settings" msg changes (D.succeed ())

type alias GameID = String

createGame : GlobalModel m -> ResultMsg GameID msg -> E.Value -> Cmd msg
createGame global msg gameInfo =
  post global "create-game" msg gameInfo (D.field "game" D.string)

deleteGame : GlobalModel m -> ResultMsg () msg -> GameID -> Cmd msg
deleteGame global msg game =
  post global ("delete-game?game=" ++ game) msg (E.object []) (D.succeed ())

setGameSettings : GlobalModel m -> ResultMsg GameID msg -> GameID -> E.Value -> Cmd msg
setGameSettings global msg game changes =
  post global ("game-settings?game=" ++ game) msg changes (D.succeed game)

type alias GameSettingsPlayer =
  { username : String
  , name : String
  , alive : Bool
  , kills : Int
  , joined : Timestamp
  }

type alias GameSettingsInfo =
  { name : String
  , description : String
  , password : String
  , players : List GameSettingsPlayer
  , state : GameState
  }

getGameSettings : GlobalModel m -> ResultMsg GameSettingsInfo msg -> GameID -> Cmd msg
getGameSettings global msg game =
  get global ("game-settings?game=" ++ game) msg <|
    D.map5 GameSettingsInfo
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
      (D.field "state" gameStateParser)

-- https://til.hashrocket.com/posts/jtgloksqxf-where-is-listzip-in-elm
mapGameIDToName : List GameID -> List String -> D.Decoder (Dict GameID String)
mapGameIDToName games names =
  D.succeed (Dict.fromList (List.map2 Tuple.pair games names))

getNames : GlobalModel m -> ResultMsg (Dict GameID String) msg -> List GameID -> Cmd msg
getNames global msg games =
  get
    global
    (Builder.relative
      [ "names" ]
      [ Builder.string "games" (String.join "," games)
      , Builder.string "defaultGame" "Nonexistent game"
      ])
    msg <|
      D.andThen (mapGameIDToName games) (D.field "games" (D.list D.string))

join : GlobalModel m -> ResultMsg String msg -> GameID -> String -> Cmd msg
join global msg game password =
  post global ("join?game=" ++ game) msg (E.object [("password", E.string password)]) (D.field "name" D.string)

-- `leave` returns a string to have symmetry with `join`
leave : GlobalModel m -> ResultMsg String msg -> GameID -> Cmd msg
leave global msg game =
  post global ("leave?game=" ++ game) msg (E.object []) (D.succeed "")

kick : GlobalModel m -> ResultMsg () msg -> GameID -> String -> String -> Cmd msg
kick global msg game user reason =
  post global ("leave?game=" ++ game) msg (E.object
    [ ("user", E.string user)
    , ("reason", E.string reason)
    ]) (D.succeed ())

start : GlobalModel m -> ResultMsg () msg -> GameID -> Cmd msg
start global msg game =
  post global ("start?game=" ++ game) msg (E.object []) (D.succeed ())

shuffle : GlobalModel m -> ResultMsg () msg -> GameID -> Cmd msg
shuffle global msg game =
  post global ("shuffle?game=" ++ game) msg (E.object []) (D.succeed ())

announce : GlobalModel m -> ResultMsg () msg -> GameID -> String -> Bool -> Cmd msg
announce global msg game announcement includeDead =
  post global ("announce?game=" ++ game) msg (E.object
    [ ("message", E.string announcement)
    , ("includeDead", E.bool includeDead)
    ]) (D.succeed ())

type alias Status =
  { target : String
  , targetName : String
  , code : String
  , game : GameID
  , gameName : String
  }

statusDecoder : D.Decoder Status
statusDecoder =
  D.map5 Status
    (D.field "target" D.string)
    (D.field "targetName" D.string)
    (D.field "code" D.string)
    (D.field "game" D.string)
    (D.field "gameName" D.string)

status : GlobalModel m -> ResultMsg Status msg -> GameID -> Cmd msg
status global msg game =
  get global ("status?game=" ++ game) msg statusDecoder

type alias OtherGame =
  { game : GameID
  , gameName : String
  , state: GameState
  }

otherGameDecoder : D.Decoder OtherGame
otherGameDecoder =
  D.map3 OtherGame
    (D.field "game" D.string)
    (D.field "gameName" D.string)
    (D.field "state" gameStateParser)

type alias GameStatuses =
  { statuses : List Status
  , other : List OtherGame
  }

statuses : GlobalModel m -> ResultMsg GameStatuses msg -> Cmd msg
statuses global msg =
  get global "statuses?all=true" msg <|
    D.map2 GameStatuses
      (D.field "statuses" (D.list statusDecoder))
      (D.field "others" (D.list otherGameDecoder))

kill : GlobalModel m -> ResultMsg () msg -> GameID -> String -> Cmd msg
kill global msg game code =
  post global ("kill?game=" ++ game) msg (E.object [("code", E.string code)]) (D.succeed ())

-- "Be sincere!"
beHonest : GlobalModel m -> ResultMsg () msg -> GameID -> Cmd msg
beHonest global msg game =
  post global ("kill?self=true&game=" ++ game) msg (E.object []) (D.succeed ())

type Notification
  = GameStarted GameID String (Maybe String) (Maybe String)
  | GameEnded GameID String String String
  | Killed GameID String String String
  | KilledSelf GameID String (Maybe String) (Maybe String)
  | Kicked GameID String String
  | TargetKicked GameID String String String
  | Shuffle GameID String (Maybe String) (Maybe String)
  | Announcement GameID String String
  | Unknown String

type alias NotificationMessage =
  { time : Int
  , read : Bool
  , message: Notification
  }

type alias NotificationResult =
  { end : Bool
  , unread : Int
  , notifications : List NotificationMessage
  }

parseNotifType : String -> D.Decoder Notification
parseNotifType notifType =
  case notifType of
    "game-started" ->
      D.map4 GameStarted
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.maybe (D.field "target" D.string))
        (D.maybe (D.field "targetName" D.string))
    "game-ended" ->
      D.map4 GameEnded
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.field "winner" D.string)
        (D.field "winnerName" D.string)
    "killed" ->
      D.map4 Killed
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.field "by" D.string)
        (D.field "name" D.string)
    "killed-self" ->
      D.map4 KilledSelf
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.maybe (D.field "target" D.string))
        (D.maybe (D.field "targetName" D.string))
    "kicked" ->
      D.map3 Kicked
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.field "reason" D.string)
    "kicked-new-target" ->
      D.map4 TargetKicked
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.field "target" D.string)
        (D.field "targetName" D.string)
    "shuffle" ->
      D.map4 Shuffle
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.maybe (D.field "target" D.string))
        (D.maybe (D.field "targetName" D.string))
    "announcement" ->
      D.map3 Announcement
        (D.field "game" D.string)
        (D.field "gameName" D.string)
        (D.field "message" D.string)
    _ ->
      D.succeed (Unknown notifType)

notifications : GlobalModel m -> ResultMsg NotificationResult msg -> Int -> Int -> Cmd msg
notifications global msg from limit =
  get global ("notifications?from=" ++ String.fromInt from ++ "&limit=" ++ String.fromInt limit)
    msg (D.map3 NotificationResult
    (D.field "end" D.bool)
    (D.field "unread" D.int)
    (D.field "notifications" (D.list (D.map3 NotificationMessage
      (D.field "time" D.int)
      (D.field "read" D.bool)
      (D.andThen parseNotifType (D.field "type" D.string))
    )))
  )

read : GlobalModel m -> ResultMsg () msg -> Cmd msg
read global msg =
  post global "read" msg (E.object []) (D.succeed ())

-- Public

type alias UserMyGame =
  { game : GameID
  , name : String
  , state : GameState
  , time : Timestamp
  , players : Int
  }

type alias UserGame =
  { game : GameID
  , name : String
  , state : GameState
  , players : Int
  , kills : Int
  , alive : Bool
  , updated : Timestamp
  }

type alias User =
  { name : String
  , bio : String
  , myGames : List UserMyGame
  , games : List UserGame
  }

getUser : GlobalModel m -> ResultMsg User msg -> String -> Cmd msg
getUser global msg user =
  get global ("user?user=" ++ user) msg <|
    D.map4 User
      (D.field "name" D.string)
      (D.field "bio" D.string)
      (D.field "myGames" (D.list (D.map5 UserMyGame
        (D.field "game" D.string)
        (D.field "name" D.string)
        (D.field "state" gameStateParser)
        (D.field "time" D.int)
        (D.field "players" D.int)
      )))
      (D.field "games" (D.list (D.map7 UserGame
        (D.field "game" D.string)
        (D.field "name" D.string)
        (D.field "state" gameStateParser)
        (D.field "players" D.int)
        (D.field "kills" D.int)
        (D.field "alive" D.bool)
        (D.field "updated" D.int)
      )))

type alias GamePlayer =
  { username : String
  , name : String
  , alive : Bool
  , killTime : Maybe Timestamp
  , killer : Maybe String
  , killerName : Maybe String
  , kills : Int
  }

type alias GameAnnouncement =
  { message : String
  , includedDead : Bool
  , time : Timestamp
  }

type alias Game =
  { creator : String
  , creatorName : String
  , name : String
  , description : String
  , players : List GamePlayer
  , state : GameState
  , time : Timestamp
  , announcements : List GameAnnouncement
  }

getGame : GlobalModel m -> ResultMsg Game msg -> GameID -> Cmd msg
getGame global msg game =
  get global ("game?game=" ++ game) msg <|
    D.map8 Game
      (D.field "creator" D.string)
      (D.field "creatorName" D.string)
      (D.field "name" D.string)
      (D.field "description" D.string)
      (D.field "players" (D.list (D.map7 GamePlayer
        (D.field "username" D.string)
        (D.field "name" D.string)
        (D.field "alive" D.bool)
        (D.field "killTime" (D.nullable D.int))
        (D.field "killer" (D.nullable D.string))
        (D.field "killerName" (D.nullable D.string))
        (D.field "kills" D.int)
      )))
      (D.field "state" gameStateParser)
      (D.field "time" D.int)
      (D.field "announcements" (D.list (D.map3 GameAnnouncement
        (D.field "message" D.string)
        (D.field "includeDead" D.bool)
        (D.field "time" D.int)
      )))

type alias Stats =
  { kills : Int
  , active : Int
  , games : Int
  }

getStats : GlobalModel m -> ResultMsg Stats msg -> Cmd msg
getStats global msg =
  get global "stats" msg <|
    D.map3 Stats
      (D.field "kills" D.int)
      (D.field "active" D.int)
      (D.field "games" D.int)
