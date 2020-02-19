module Utils.CssUtils exposing (..)

import Css exposing (..)

calcDimension total padding border = 
  let 
    edgeSize = Css.calc (Css.calc padding Css.plus padding) Css.plus border
  in
  Css.calc total Css.minus edgeSize

