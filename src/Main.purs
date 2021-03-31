module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


data Tree ∷ ∀ k. k → Type
data Tree k

foreign import data Branch ∷ forall k. k → Tree k → Tree k → Tree k

foreign import data Leaf ∷ forall k. k → Tree k


class Invert ∷ ∀ k. Tree k → Tree k → Constraint
class Invert i o | i → o where
  invert ∷ Proxy i → Proxy o


instance invertLeaf ∷ Invert ( Leaf k ) ( Leaf k ) where
  invert = identity

else

instance invertBranch ∷
  ( Invert j j'
  , Invert k k'
  ) ⇒ Invert ( Branch i j k ) ( Branch i k' j' ) where
  invert = unsafeCoerce


type T =
  Branch "T"
  ( Branch "L" ( Leaf "1" ) ( Leaf "2" ) )
  ( Branch "R" ( Leaf "3" ) ( Leaf "4" ) )


type I =
  Branch "T"
  ( Branch "R" ( Leaf "4" ) ( Leaf "3" ) )
  ( Branch "L" ( Leaf "2" ) ( Leaf "1" ) )


t ∷ Proxy I
t = invert ( Proxy ∷ _ T )


main :: Effect Unit
main = do
  log "🍝"
