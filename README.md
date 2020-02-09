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

- (probably easy) Mark the winner on the game page
  - Could also display the winner in other places too
  - (a bit less easy but not very hard nonetheless) If so, then it should link to the winner
- (maybe tedious) Parse for \@mentions, !games, and external links. Could also add support for bold, italics, etc.
- (requires thinking) Finish content for about, welcome, and maybe terms pages
- (probably very hard/complicated) Emails:
  - Password reset forms
  - Option for email notifications

I don't think this should be a PWA since it requires an internet connection anyways.
