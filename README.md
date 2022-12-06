# Babel Cards

A TUI flash cards application.

For right-to-left languages, you will need to start babel-cards from within [bicon](https://github.com/behdad/bicon).

![](img/demo.gif)

## [Development Blog](https://www.rhoulam.tech/blog/tags/babel-cards.html)

### Screenshots

![Main view](img/main-view.png)
![Deck building](img/cards-overview.png)
![Decks](img/decks-overview.png)
![Reviewing in standard mode](img/review-standard.png)
![Reviewing in reverse mode](img/review-reverse.png)

## Building

`stack build`

## Usage

`stack exec -- babel-cards`

## Environment Variables

`BABEL_DATABASE` can be used to provide a path to a different SQLite3 database than the default.
