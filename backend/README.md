# pine backend

Our backend is written in Elm and uses Scotty as our web framework.
It doesn't serve any Html pages, but instead listens to a port (:3000 by default) for
incoming POST requests with a recognizable body of haskell code.

It then uses hint, the Runtime Haskell interpreter to evaluate the code and replies with the results.

## Usage
1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Install all of the necessary Haskell dependencies
Run `stack build` from `backend/`.

3. Start backend server 
Run `stack run` from `backend/`. 

## Development Setup

 - To ensure that communication between the Haskell backend and Elm frontend is consistent, we used
a Haskell package called `elm-street` which is able to automatically generate compatible 
types, encoders, and decoders for both programs. If you would like to change the data type, here are the steps:

    1. Edit the `CodeSubmission` data type in the `backend/src/Types.hs` file 
    2. Run `stack ghci` from `backend/` to start the interpreter
    3. Type `:load GenerateElmTypes.hs` to load the generator function
    4. Type `generate` to run the loaded function. This will generate type files for Elm into the `backend/GeneratedTypes` directory and these files should be moved to `frontend/src/GeneratedTypes`

