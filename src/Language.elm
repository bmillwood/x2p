module Language exposing
  ( Language
  , all
  , init
  , toString
  , renderCode
  )

import Cpp
import Code
import Python

type Language
  = Cpp
  | Python

all : List Language
all = [Cpp, Python]

init : Language
init = Python

toString : Language -> String
toString lang =
  case lang of
    Cpp -> "C++"
    Python -> "Python"

renderCode : Language -> Code.Code -> String
renderCode lang =
  case lang of
    Cpp -> Cpp.code
    Python -> Python.code
