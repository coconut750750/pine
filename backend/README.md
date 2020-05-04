# pine backend

## Project Setup

1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Run `stack build` at backend root
This should install all of the necessary Haskell dependencies

3. Run `stack run` at backend root
This runs the compiled binary created from `stack build`

## Project Development Setup

1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Start Development

    #### Frontend:
    Our frontend is written in Elm and then compiled into Html/JS. To compile, run `elm make src/Main.elm` in the `/frontend/` directory.
    #### Backend:
    Our backend uses Scotty as our web framework, and should automatically serve the generated index.html file

3. If and when necessary, you can automatically generate Elm types from the Haskell src/Types.hs
    1. run `stack ghci` to open the interpreter from the backend directory
    2. type `:load GenerateElmTypes.hs` to load the generator function
    2. type `generate` to run the loaded function. This will generate type files for Elm under "./backend/GeneratedTypes" and should be moved to "./frontend/src/GeneratedTypes"
