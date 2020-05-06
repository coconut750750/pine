import React from "react"

import Layout from "../components/layout"
import SEO from "../components/seo"
import Elm from "react-elm-components"
import Pine from "./../../../frontend/src/Repl.elm"

var interpreterServer = "http://159.203.88.220:3000/";

var flags1 = {
    interpreter: interpreterServer,
    prefix:     "",
    infix:      "1 + 2",
    suffix:     "",
}

var flags2 = {
    interpreter: interpreterServer,
    prefix:     "let concatString :: String -> String -> String",
    infix:      "    concatString str1 str2 = str1 ++ str2",
    suffix:     "in concatString \"Hello\" \"World\"",
}

var flags3 = {
    interpreter: interpreterServer,
    prefix:     `let mysteryBool = not False && (True || (False && not True)) || (True)`,
    infix:      
`in 
    if mysteryBool
    then "True"
    else "False"`,
    suffix:     "",
}

const IndexPage = () => (
  <Layout>
    <SEO title="Home" />
    <h1>React component REPL demonstration!</h1>
    <p>Welcome! This has come a long way, and hopefully nothing breaks during this demonstration :)</p>
    <br/>
    <h3>Let&apos;s start simple</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags1 }todos/>
    <br/>
    <br/>
    <h3>String Concatenation</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags2 }todos/>
    <br/>
    <br/>
    <h3>Multiline expressions?</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags3 }todos/>
  </Layout>
)

export default IndexPage
