import magnolia1.*
import au.com.dius.pact.consumer.dsl.*
import au.com.dius.pact.consumer.ConsumerPactBuilder

trait PactGen[T]:
  def generatePactBody(
      parent: Option[DslPart] = None,
      name: Option[String] = None
  ): DslPart

object PactGen extends AutoDerivation[PactGen] {
  def apply[T: PactGen]: PactGen[T] = summon[PactGen[T]]

  private def caseClass[T](parent: DslPart, name: String)(
      ctx: CaseClass[PactGen, T]
  ) =
    val nested = parent.`object`(name)
    ctx.params.foreach { param =>
      param.typeclass.generatePactBody(Some(nested), Some(param.label))
    }
    nested.closeObject()
    nested

  override def join[T](ctx: CaseClass[PactGen, T]): PactGen[T] = {
    case (Some(parent), Some(name)) => caseClass(parent, name)(ctx)
    case (None, None)               => caseClass(PactDslJsonBody(), ".")(ctx)
    case x => throw IllegalArgumentException(s"Invalid case: $x")
  }

  override def split[T](ctx: SealedTrait[PactGen, T]): PactGen[T] =
    throw new IllegalArgumentException(
      "Sealed traits are not supported. please use instance based pact generation"
    )

  // int pact
  given PactGen[Int] = {
    case (Some(parent: PactDslJsonBody), Some(name)) => parent.integerType(name)
    case _ => PactDslJsonRootValue.integerType()
  }

  // boolean pact
  given PactGen[Boolean] = {
    case (Some(parent: PactDslJsonBody), Some(name)) => parent.booleanType(name)
    case _ => PactDslJsonRootValue.booleanType()
  }

  // string pact
  given PactGen[String] = {
    case (Some(parent: PactDslJsonBody), Some(name)) => parent.stringType(name)
    case _ => PactDslJsonRootValue.stringType()
  }

  // double pact
  given PactGen[Double] = {
    case (Some(parent: PactDslJsonBody), Some(name)) => parent.numberType(name)
    case _ => PactDslJsonRootValue.numberType()
  }

  // option pact
  given option[T: PactGen]: PactGen[Option[T]] =
    summon[PactGen[T]].generatePactBody(_, _)

  // list pact
  given list[T: PactGen]: PactGen[List[T]] = {
    case (Some(parent), Some(name)) => array(parent, name)
    case (None, None)               => array(PactDslJsonArray(), ".")
    case x => throw IllegalArgumentException(s"Invalid case: $x")
  }

  private def array[T: PactGen](parent: DslPart, name: String) = {
    val itemPactBody = PactGen[T].generatePactBody()
    val arr = parent match {
      case p: PactDslJsonArray => p
      case p: PactDslJsonBody  => p.array(name)
    }
    itemPactBody match
      case item: PactDslJsonRootValue => arr.putObjectPrivate(item)
      case item: PactDslJsonArray     => arr.putObjectPrivate(item)
      case item: PactDslJsonBody      => arr.putObjectPrivate(item)
      case x => throw IllegalArgumentException(s"Invalid case: $x")
    arr.closeArray()
    parent
  }
}
