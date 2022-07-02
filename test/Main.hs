{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main (main) where

import Language.QBE

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Silver (goldenVsAction)
import System.FilePath ((</>), (<.>))
import Prettyprinter (Pretty(pretty), layoutPretty, defaultLayoutOptions)
import Prettyprinter.Render.Text (renderStrict)


data P = forall a. Pretty a => P a
instance Pretty P where
  pretty (P x) = pretty x

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests = testGroup "golden tests"
  [ t "ident"
      [ P $ Ident @'AggregateTy "aggregateTy"
      , P $ Ident @'Global "global"
      , P $ Ident @'Temporary "temporary"
      , P $ Ident @'Label "label"
      ]
  , t "type" ([Word, Long, Single, Double], [BaseTy Word, Byte, HalfWord])
  , t "const"
      [ CInt True 1
      , CInt False 2
      , CSingle 0.1
      , CDouble (-0.2)
      , CGlobal "global"
      ]
  , t "linkage" (Export, Section "secName" Nothing, Section "secName" $ Just "flag1 flag2")
  , t "typedef" $ Typedef "t" (Just 8)
      [ (SubExtTy HalfWord, Just 16)
      , (SubAggregateTy "t1", Nothing)
      ]
  , t "opaque" $ Opaque "t" 8 16
  , t "val" [ValConst (CInt False 0), ValTemporary "temporary", ValGlobal "global"]
  , t "jmp" $ Jmp "target"
  , t "jnz" $ Jnz (ValConst $ CInt False 0) "target1" "target2"
  , t "ret" $ Ret $ Just $ ValTemporary "x"
  ]
  where
    t name value = goldenVsAction
      name
      ("golden" </> name <.> "qbe")
      (pure $ pretty value)
      (renderStrict . layoutPretty defaultLayoutOptions)
