# pine

## Using the react component

1. In your project folder, create a `src/elm` directory

2. Copy over the files in this `frontend/src` directory into your project's `src/elm` directory.
    - Your project should now have an `src/elm/Repl.elm` file

3. From this repo's `frontend/` directory, copy over `elm.json` into your root

4. In your `elm.json` file, make sure that `"source-directories"` includes your "`src/elm`" directory.

5. Set up gatsby to compile and run `.elm` files by running:
    - `yarn add react-elm-components gatsby-plugin-elm`
    - `yarn add elm-webpack-loader -D`
    - Add `gatsby-plugin-elm` to gatsby-config.js plugins

6. That's it! `gatsby-example/src/pages/index.js` has several examples on how to use the react component, but for a quick tldr:
```javascript
import React from "react"
import Layout from "../components/layout"

import Elm from "react-elm-components"
import Pine from "./../../../frontend/src/Repl.elm"

var flags = {
    interpreter: "http://159.203.88.220:3000/",
    prefix:      "",
    infix:       "1 + 2",
    suffix:      "",
}

const IndexPage = () => (
  <Layout>
    <Elm src={ Pine.Elm.Repl } flags={ flags }/>
  </Layout>
)


export default ReplPage
```
 - The `interpreter` flag is the haskell backend's IP address. The IP shown is a Digital Ocean droplet that's being borrowed for the time being.

## Project Setup

1. Install Haskell & stack
Installation instructions for Haskell and stack could be found here:
    www.haskell.org/downloads/

2. Run `stack build` at backend root
This should install all of the necessary Haskell dependencies

3. Run `stack run` at backend root
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

4. If and when necessary, you can automatically generate Elm types from the Haskell src/Types.hs
    1. run `stack ghci` to open the interpreter from the backend directory
    2. type `:load GenerateElmTypes.hs` to load the generator function
    2. type `generate` to run the loaded function. This will generate type files for Elm under "./backend/GeneratedTypes" and should be moved to "./frontend/src/GeneratedTypes"

## FAQ
1. What is 'frontend/elm-stuff'
The elm-stuff/ directory is made by Elm, and is similar to what a node\_modules/ directory is
