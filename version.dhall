let Prelude/List/map =
      https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/map

in  λ(low : Text) →
    λ(high : Text) →
    λ(srcs : List Text) →
    λ(ghc : { name : Text, mixin : List Text }) →
    λ(mods : List Text) →
      { condition = "impl(ghc >= ${low}) && impl(ghc < ${high})"
      , source-dirs =
          Prelude/List/map Text Text (λ(x : Text) → "src-ghc-${x}") srcs
      , dependencies = [ ghc ⫽ { version = ">=${low} && <${high}" } ]
      , other-modules = mods
      }
