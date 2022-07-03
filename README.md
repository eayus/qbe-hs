# qbe-hs

[![Hackage](https://img.shields.io/hackage/v/qbe.svg)](https://hackage.haskell.org/package/qbe)
[![builds.sr.ht status](https://builds.sr.ht/~fgaz/qbe-hs/commits/master.svg)](https://builds.sr.ht/~fgaz/qbe-hs/commits/master?)

Haskell types and prettyprinter for the [IL](https://c9x.me/compile/doc/il.html)
of the [QBE](https://c9x.me/compile/) compiler backend

## Example

```haskell
helloWorld :: Program
helloWorld = Program [] [helloString] [helloMain]
  where
    helloString = DataDef [] "str" Nothing
      [ FieldExtTy Byte $ String "hello world" :| []
      , FieldExtTy Byte $ Const (CInt False 0) :| []
      ]
    helloMain = FuncDef [Export] (Just $ AbiBaseTy Word) "main"
      Nothing [] NoVariadic $
      Block "start"
        []
        [ Call (Just ("r", AbiBaseTy Word)) (ValGlobal "puts")
            Nothing
            [Arg (AbiBaseTy Long) $ ValGlobal "str"]
            []
        ]
        (Ret $ Just $ ValConst $ CInt False 0)
      :| []
```

Gets rendered to

```
data $str =
{b "hello world", b 0}
export
function w $main () {
@start
    %r =w call $puts (l $str)
    ret 0
}
```

## Contributing

You can send patches to my
[public-inbox mailing list](https://lists.sr.ht/~fgaz/public-inbox)
or to any of the contacts listed at [fgaz.me/about](https://fgaz.me/about).
Or you can send a pull request to the
[GitHub mirror](https://github.com/fgaz/qbe-hs).

Issues are tracked at https://todo.sr.ht/~fgaz/qbe-hs
