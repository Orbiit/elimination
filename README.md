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

- (very easy; server-side) "Session doesn't exist!" -> "You are not signed in."
- (requires thinking) Finish content for about, welcome, and maybe terms pages
- (maybe tedious) Parse for \@mentions, !games, and external links. Could also add support for bold, italics, etc.
- (probably very hard/complicated) Emails:
  - Password reset forms
  - Option for email notifications

I don't think this should be a PWA since it requires an internet connection anyways. There shouldn't be a way to publicly list a game because the game requires real life interaction for eliminating people.
