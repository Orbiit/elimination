# Elimination Game

It's this thing where people participate in a game, then they are assigned another player in the game as a target. They'll have to locate this person and ask for their code to eliminate them for a new target.

Inspired by theassassingame.com which apparently is down

## Development

Uses [Rollup](https://rollupjs.org/guide/en/), [Elm](https://elm-lang.org/), and [Node](https://nodejs.org/)

The following watches for file changes and builds automatically:

```sh
npm run watch
```

Build for production:

```sh
npm run build
```

## Things to add

- Errors persist when leaving and returning to a page
- Display list of upcoming and finished games
- broadcast to players of game (option to limit to alive ppl)
- (requires thinking) Finish content for about, welcome, and maybe terms pages (and meta desc tags?)
- (maybe tedious) Parse for \@mentions, !games, and external links. Could also add support for bold, italics, etc.
  - Current idea: use elm/parser to convert plain text -> `List (Html msg)` (the hard part). Needs a server endpoint for converting game IDs into names, then this will have to be updated again. Perhaps to avoid reparsing, it could return a different type that gets converted to `Html msg`
- (probably very hard/complicated) Emails:
  - Password reset forms
  - Option for email notifications

I don't think this should be a PWA since it requires an internet connection anyways. There shouldn't be a way to publicly list a game because the game requires real life interaction for eliminating people.

## Feedback

- How to find active games?

  - Public list of games is bad

- It is not intuitive to find what games you're in. The wording of "active games" on the front page is unclear or not obvious enough.

  - Can list games that haven't started yet (and/or the ones you've created) on front page too.

- It is not intuitive how to eliminate someone.

  - The about page may make this clear, but I think it should be intuitive on the front page too. For example, the kill modal can explain how/where to get the elimination sequence from.

- It is inconvenient/unintuitive to go to the profile.

  - The current system makes it easier to sign out, but I guess not many people need to sign out that often.

  - Trigonal recommends making it link to profile instead of settings

- Serif font

- Should be able to mark oneself as killed for honesty and speed
