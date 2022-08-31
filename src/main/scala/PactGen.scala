import magnolia1.*
import au.com.dius.pact.consumer.dsl.*
import au.com.dius.pact.consumer.ConsumerPactBuilder

trait PactGen[T]:
  def generatePactBody(
      parent: Option[PactDslJsonBody] = None,
      name: Option[String] = None
  ): DslPart

object PactGen extends AutoDerivation[PactGen] {
  override def join[T](ctx: CaseClass[PactGen, T]): PactGen[T] = {
    case (Some(parent), Some(name)) =>
      val nested = parent.`object`(name)
      ctx.params.foreach { param =>
        param.typeclass.generatePactBody(Some(nested), Some(param.label))
      }
      nested.closeObject()
      parent.closeObject()
      parent
    case (None, None) =>
      val parent = PactDslJsonBody()
      parent.`object`("root")
      ctx.params.foreach { param =>
        param.typeclass.generatePactBody(Some(parent), Some(param.label))
      }
      parent.closeObject()
      parent
    case x => throw IllegalArgumentException(s"Invalid case: $x")
  }

  override def split[T](ctx: SealedTrait[PactGen, T]): PactGen[T] =
    ???

  // int pact
  given PactGen[Int] = {
    case (Some(parent), Some(name)) => parent.integerType(name)
    case _ => PactDslJsonRootValue.integerType()
  }

  // boolean pact
  given PactGen[Boolean] = {
    case (Some(parent), Some(name)) => parent.booleanType(name)
    case _ => PactDslJsonRootValue.booleanType()
  }

  // string pact
  given PactGen[String] = {
    case (Some(parent), Some(name)) => parent.stringType(name)
    case _ => PactDslJsonRootValue.stringType()
  }

  // double pact
  given PactGen[Double] = {
    case (Some(parent), Some(name)) => parent.numberType(name)
    case _ => PactDslJsonRootValue.numberType()
  }

  // option pact
  given option[T: PactGen]: PactGen[Option[T]] =
    summon[PactGen[T]].generatePactBody(_, _)

  // list pact
  given list[T: PactGen]: PactGen[List[T]] = {
    case (Some(parent), Some(name)) =>
      val itemGen = summon[PactGen[T]]
      val itemPactBody = itemGen.generatePactBody().asInstanceOf[PactDslJsonRootValue]
      parent.minArrayLike(name, 1, itemPactBody, 1)
      parent
      
    case x => throw IllegalArgumentException(s"Invalid case: $x")
  }
}
