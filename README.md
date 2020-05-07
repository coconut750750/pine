# pine

## Directory Layout
 - `frontend/` contains all the Elm code that manages the Repl on the user side. The `src/Repl.elm` file can be standalone or as a React component.
 - `backend/` contains the Haskell server code to listen for and  
 - `gatsby-example/` contains an example of a Gatsby project using our Elm component.

Each of these directories contains their on `README.md` file on how to run or set up the corresponding development environment.

## Using the react component in your Gatsby project

1. In your Gatsby project, create a `src/elm` directory

2. Copy over the files from this `frontend/src` directory into your project's `src/elm` directory.
    - Your project should now have an `src/elm/Repl.elm` file

3. From this repo's `frontend/` directory, copy over `elm.json` into your root

4. In your `elm.json` file, make sure that `"source-directories"` includes your `src/elm` directory.

5. Set up gatsby to compile and run `.elm` files by running:
    - `yarn add react-elm-components gatsby-plugin-elm`
    - `yarn add elm-webpack-loader -D`
    - Add `gatsby-plugin-elm` to the plugins in your `gatsby-config.js` file.

6. That's it! `gatsby-example/src/pages/index.js` has several examples on how to use the react component, but for a quick tldr:
```javascript
import React from "react"
import Layout from "../components/layout"

import Elm from "react-elm-components"
import Pine from "../elm/Repl.elm"

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

