import au.com.dius.pact.consumer.dsl.PactDslJsonBody

case class Category(
    label: String,
    id: String
)
enum Color:
  case White
  case Brown
  case Black

sealed trait ColorCategory
object ColorCategory{
  case class White(brightness: Int) extends ColorCategory
  case object Brown extends ColorCategory
  case object Black extends ColorCategory
}  
  
given PactGen[Color] = PactGen[Color.Brown.type].widen[Color]
given PactGen[ColorCategory] = PactGen[ColorCategory.White].widen[ColorCategory]

case class Animal(
    name: String,
    age: Int,
    rank: Double,
    category: Category,
    color: Color,
    colorCategory: ColorCategory,
    breed: Option[String],
    domestic: Option[Boolean],
    tags2: List[List[Category]],
    tags3: List[List[String]],
    tags4: List[String],
    tags5: List[List[List[Category]]]
) derives PactGen

@main def hello(): Unit =
  val pact = PactGen[Animal].generatePactBody()
  println(pact)
