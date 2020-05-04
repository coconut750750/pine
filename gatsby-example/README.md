# Gatsby Example

Most of the boilerplate code provided by https://github.com/gatsbyjs/gatsby-starter-default

## Steps to enable Elm components in Gatsby (React):
 - `yarn add react-elm-components gatsby-plugin-elm`
 - `yarn add elm-webpack-loader -D`
 - Add `gatsby-plugin-elm` to gatsby-config.js plugins

## Files of interest
Then, looking at gatsby-example/src/pages/index.js should be a good example of how to add an this Elm component.

The components are stored in gatsby-example/src/elm, copied straight from /frontend/src/

The gatsby-example/elm.json file is also necessary.

## Running the example

To see the website and component in action, run `gatsby develop`
