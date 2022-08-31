import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.*
import magnolia1.*

import Macro.*
import magnolia1.SealedTrait.Subtype

trait CustomDerivation[TypeClass[_]] extends Derivation[TypeClass]:
  inline def derivedMirror2[A](using mirror: Mirror.Of[A]): Typeclass[A] =
    inline mirror match
      case _: Mirror.SumOf[A]           => summon[Typeclass[A]]
      case product: Mirror.ProductOf[A] => derivedMirrorProduct[A](product)

  inline def derived2[A](using Mirror.Of[A]): Typeclass[A] = derivedMirror2[A]

  inline given autoDerived[A](using Mirror.Of[A]): TypeClass[A] = derived2
