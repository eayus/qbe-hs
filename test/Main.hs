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
import Data.List.NonEmpty (NonEmpty((:|)))


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
  , t "data" $ DataDef [Export] "d" (Just 8)
      [ FieldZero 16
      , FieldExtTy Byte $ Symbol "g" (Just 32) :| [String "foo\nbar\0baz", Const $ CInt True 1]
      ]
  , t "function" $ FuncDef [Export] (Just $ AbiAggregateTy "t") "f"
      (Just "env") [Param (AbiBaseTy Word) "a", Param (AbiBaseTy Double) "b"] Variadic $
      Block "l" [] [] (Ret Nothing) :| []
  , t "val" [valInt 0, ValTemporary "temporary", ValGlobal "global"]
  , t "jmp" $ Jmp "target"
  , t "jnz" $ Jnz (valInt 0) "target1" "target2"
  , t "ret" $ Ret $ Just $ ValTemporary "x"
  , t "phi" $ Phi "a" Word [PhiArg "b" $ valInt 1, PhiArg "c" $ valInt 2]
  , t "call" $ Call (Just ("r", AbiBaseTy Word)) (ValGlobal "f") (Just $ valInt 1)
      [Arg (AbiBaseTy Word) $ valInt 2, Arg (AbiAggregateTy "t") $ ValTemporary "a"]
      [Arg (AbiBaseTy Word) $ valInt 3, Arg (AbiAggregateTy "t1") $ ValTemporary "b"]
  ]
  where
    t name value = goldenVsAction
      name
      ("golden" </> name <.> "qbe")
      (pure $ pretty value)
      (renderStrict . layoutPretty defaultLayoutOptions)

valInt :: Int -> Val
valInt i | i >= 0 = ValConst $ CInt False $ fromIntegral i
         | otherwise = ValConst $ CInt True $ fromIntegral $ negate i
