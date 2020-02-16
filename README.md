# pine
## Project Setup

1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Run `stack build` at root
This should install all of the necessary Haskell dependencies

3. Run `stack run` at root
This runs the compiled binary created from `stack build`

!!! Do not use `stack install`, it doesn't do what it might sound like it does.
It builds the haskell program and installs it onto your computer, which might be useful if this is
a fully fleshed out version but it isn't.
Using something like `stack install elm-street` will install a stack libary/program onto your
computer which probably isn't something you're looking for either.

## Project Development Setup

1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Install Elm
Installation instructions (and other cool stuff about Elm) can be found here:
    guide.elm-lang.org/install/elm.html

3. Start Development

    #### Frontend:
    Our frontend is written in Elm and then compiled into Html/JS. To compile, run `elm make src/Main.elm` in the `/frontend/` directory.
    #### Backend:
    Our backend uses Scotty as our web framework, and should automatically serve the generated index.html file
