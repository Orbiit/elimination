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

- (not super easy) Say "Shuffled targets" when targets are shuffled
- (requires thinking) Finish content for about, welcome, and maybe terms pages (and meta desc tags?)
- (could be complicated) Press escape to close modal/cancel unsaved changes warning
  - One way is to use Browser.Events to listen for the key press, then close every single modal manually. This would mean that each page's HideModal would lose its Cmd/Api.PageCmd functionality, but this is fine (assuming no page uses such for hiding modals). Cancelling the unsaved changes warning could be done directly
- (maybe tedious) Parse for \@mentions, !games, and external links. Could also add support for bold, italics, etc.
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
