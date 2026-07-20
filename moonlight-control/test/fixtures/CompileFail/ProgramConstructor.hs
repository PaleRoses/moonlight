module ProgramConstructor where

import Moonlight.Control.Program
  ( Program,
  )

bad :: Program () Int
bad =
  Seq Skip Skip
