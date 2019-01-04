
module ViewUtils exposing(..)



import Svg exposing (..)
import Svg.Attributes exposing (fill, height, id, in_, result, rx, stdDeviation, viewBox, width, x, y)

backgroundColor : Attribute msg
backgroundColor =
    fill "#000"

renderBackground : Svg msg
renderBackground =
    rect [ x "0", y "0", width size, height size, backgroundColor ] []


size : String
size =
    "100"
    