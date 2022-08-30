import magnolia1.*
import com.bilalfazlani.rainbowcli.{given, *}

trait PrettyPrint[T]:
  extension (value: T) def print: String

object PrettyPrint extends AutoDerivation[PrettyPrint] {

  override def join[T](ctx: CaseClass[PrettyPrint, T]): PrettyPrint[T] =
    new PrettyPrint {
      extension (value: T)
        def print: String =
          val values = ctx.params
            .map { param =>
              val pName = param.label.blue
              val pValueString = param.typeclass.print(param.deref(value)).yellow
              s"$pName = $pValueString"
            }
          values.mkString(s"${ctx.typeInfo.short}(".green, ", ", ")".green)
    }

  override def split[T](ctx: SealedTrait[PrettyPrint, T]): PrettyPrint[T] = 
    new PrettyPrint[T] {
      extension (value: T)
        def print: String = 
          ctx.choose(value)(subType => subType.subtype.typeclass.print(subType.cast(value)))
    }

  given PrettyPrint[Int] = _.toString
  given PrettyPrint[String] = (x: String) => s""""$x""""
  given PrettyPrint[Boolean] = _.toString
  given PrettyPrint[Double] = _.toString
  given PrettyPrint[Char] = _.toString
}