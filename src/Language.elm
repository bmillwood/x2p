module Language exposing
  ( Language
  , all
  , init
  , toString
  , renderCode
  )

import C
import Code
import Python

type Language
  = C
  | Python

all : List Language
all = [C, Python]

init : Language
init = Python

toString : Language -> String
toString lang =
  case lang of
    C -> "C"
    Python -> "Python"

renderCode : Language -> Code.Code -> String
renderCode lang =
  case lang of
    C -> C.code
    Python -> Python.code
