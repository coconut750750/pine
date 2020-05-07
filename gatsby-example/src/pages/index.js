import React from "react"

import Layout from "../components/layout"
import SEO from "../components/seo"
import Elm from "react-elm-components"
import Pine from "../elm/Repl.elm"

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

var flags4 = {
    interpreter: interpreterServer,
    prefix:     
`let
    quicksort []     = []
    quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)`,
    infix:      
`        where
             lesser  = filter (< p) xs
             greater = filter (>= p) xs`,
    suffix: `in 
    quicksort [2, 5, 9, 6, 1, 8, 4, 8, 0]`,
}

var flags5 = {
    interpreter: interpreterServer,
    prefix: 
`let
    getPage num  
        | num <= 0 = "There are no negative pages!"  
        | num <= 1 = "Once upon a time, there were guards."  
        | num <= 2 = "The end!"  
        | otherwise = "It's only a two page book!"  
in`,
    infix:  `getPage 1`,
    suffix: ``,
}

var flags6 = {
    interpreter: interpreterServer,
    prefix: ``,
    infix:  `map ($ 3) [(4+), (10*), (^2), sqrt]`,
    suffix: ``,
}

var flags7 = {
    interpreter: interpreterServer,
    prefix: ``,
    infix:  `[1..]`,
    suffix: ``,
}

var flags8 = {
    interpreter: interpreterServer,
    prefix: ``,
    infix:  `(*) <$> [1,2,3] <*> ([3,4,5] >>= \\x -> [x,-x])`,
    suffix: ``,
}

var flags9 = {
    interpreter: interpreterServer,
    prefix: 
`let 
    describeList xs = "The list is " ++ what xs  
        where what [] = "empty."  
              what [x] = "a singleton list."  
              what [x, y] = "a 2-tuple list."  
              what xs = "a longer list."`,
    infix:  
`    myList = [1]`,
    suffix: `in describeList myList`,
}

const IndexPage = () => (
  <Layout>
    <SEO title="Home" />
    <h1>React component REPL demonstration!</h1>
    <p>Welcome! This has come a long way, and hopefully nothing breaks during this demonstration :)</p>
    <p>Many of these examples were inspired by or directly from <a href="http://learnyouahaskell.com/">learnyouahaskell.com</a></p>
    <br/>
    <h3>Let&apos;s start simple</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags1 }/>
    <br/>
    <br/>
    <h3>String Concatenation</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags2 }/>
    <br/>
    <br/>
    <h3>This won&apos;t work, but it also won&apos;t crash the server!</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags7 }/>
    <br/>
    <br/>
    <h3>Multiline expressions too!</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags3 }/>
    <br/>
    <br/>
    <h3>Quicksorts, anyone?</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags4 }/>
    <br/>
    <br/>
    <h3>Guards!</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags5 }/>
    <br/>
    <br/>
    <h3>Astounding Applicatives</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags6 }/>
    <br/>
    <br/>
    <h3>Multitudes of Monads</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags8 }/>
    <br/>
    <br/>
    <h3>Lastly, pattern matching</h3>
    <Elm src={ Pine.Elm.Repl } flags={ flags9 }/>
    <br/>
  </Layout>
)

export default IndexPage
