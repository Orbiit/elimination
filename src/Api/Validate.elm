module Api.Validate exposing (..)

import Regex
import Email

makeRegex : String -> Regex.Regex
makeRegex string =
  case Regex.fromString string of
    Just regex ->
      regex
    Nothing ->
      Regex.never

username = makeRegex "^[a-z0-9_-]{3,}$"

usernameLabel = "Only letters, digits, underscores, and hyphens are allowed. This cannot be changed later."

usernameOk : String -> Maybe String
usernameOk string =
  if Regex.contains username string then
    Nothing
  else
    Just "Usernames must be at least three characters long and may only contain lowercase letters, digits, underscores, and hyphens."

nameLabel = "Lets others be able to find and eliminate you, which makes the game fair."

nameOk : String -> Maybe String
nameOk string =
  if String.isEmpty string then
    Just "You need to have a name."
  else
    Nothing

passwordLabel = "Must be at least 8 characters long. We recommend using a memorable sentence, such as \"Do not use example passwords.\""

passwordOk : String -> Maybe String
passwordOk string =
  if string == "Do not use example passwords." then
    Just "Yeah, don't."
  else if String.length string >= 8 then
    Nothing
  else
    Just "Password too short. They must be at least eight characters long."

emailLabel = "For password reset forms. You can also turn on email notifications if you want."

emailOk : String -> Maybe String
emailOk string =
  if Email.isValid string then
    Nothing
  else
    Just "That does not look like an email."
