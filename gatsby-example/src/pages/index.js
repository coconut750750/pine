import React from "react"

import Layout from "../components/layout"
import SEO from "../components/seo"
import Elm from "react-elm-components"
import Pine from "./../../../pine/frontend/src/Repl.elm"

var haskellInterpreter = "http://159.203.88.220:3000";

const IndexPage = () => (
  <Layout>
    <SEO title="Home" />
    <h1>Hi people</h1>
    <p>Welcome to your new Gatsby site.</p>
    <p>Now go build something great.</p>
    <Elm src={ Pine.Elm.Repl } flags={ haskellInterpreter }todos/>
  </Layout>
)

export default IndexPage
