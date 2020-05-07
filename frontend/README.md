# pine frontend

Our frontend is written in Elm and then compiled into Html/JS. 

## Usage
To see how to use this Elm component as a React component, the README.md at the project directory
provides a much better guide.

To run this as a standalone page/app:
1. Install Elm
Installation instructions (and other cool stuff about Elm) can be found here:
    guide.elm-lang.org/install/elm.html

2. Compile the Elm files 
Run `elm make src/Repl.elm`

3. Open the genrated `index.html` file in your browser.

## Development Setup

1. Install Elm
Installation instructions (and other cool stuff about Elm) can be found here:
    guide.elm-lang.org/install/elm.html

2. Start developmental server
Run `elm reactor` from `frontend/`.

 - To ensure that communication between the Haskell backend and Elm frontend is consistent, we used
a Haskell package called `elm-street` which is able to automatically generate compatible 
types, encoders, and decoders for both programs. If you would like to change the data type, here are the steps:

    1. Edit the `CodeSubmission` data type in the `backend/src/Types.hs` file 
    2. Run `stack ghci` from `backend/` to start the interpreter
    3. Type `:load GenerateElmTypes.hs` to load the generator function
    4. Type `generate` to run the loaded function. This will generate type files for Elm into `backend/GeneratedTypes` and these files should be moved to `frontend/src/GeneratedTypes`

