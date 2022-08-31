import magnolia1.*
import au.com.dius.pact.consumer.dsl.*
import au.com.dius.pact.consumer.ConsumerPactBuilder

trait PactGen[T]:
  def generatePactBody(
      parent: Option[DslPart] = None,
      name: Option[String] = None
  ): DslPart

  def widen[A >: T]: PactGen[A] = this.asInstanceOf[PactGen[A]]

object PactGen extends CustomDerivation[PactGen] {
  def apply[T: PactGen]: PactGen[T] = summon[PactGen[T]]

  private def caseClass[T](parent: DslPart, name: String)(
      ctx: CaseClass[PactGen, T]
  ) =
    val isObj = ctx.isObject || ctx.params.isEmpty
    if !isObj then
      val nested = parent.`object`(name)
      ctx.params.foreach { param =>
        param.typeclass.generatePactBody(Some(nested), Some(param.label))
      }
      nested.closeObject()
      nested
    else 
      parent match {
        case obj: PactDslJsonBody => obj.stringValue(name, ctx.typeInfo.short)
        case arr: PactDslJsonArray => ??? //todo: add support for array of sums
      }

  override def join[T](ctx: CaseClass[PactGen, T]): PactGen[T] = {
    case (Some(parent), Some(name)) => caseClass(parent, name)(ctx)
    case (None, None)               => caseClass(PactDslJsonBody(), ".")(ctx)
    case x => throw IllegalArgumentException(s"Invalid case: $x")
  }

  override def split[T](ctx: SealedTrait[PactGen, T]): PactGen[T] =
    //this is not required since we never automatically chose which case to use
    (_, _) => ??? // unreachable code

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
