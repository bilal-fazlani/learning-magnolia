import au.com.dius.pact.consumer.dsl.PactDslJsonBody

case class Category(
  label: String,
  id: String,
)

case class Animal(
    name: String,
    age: Int,
    category: Category,
    breed: Option[String],
    domestic: Option[Boolean],
    tags2: List[List[Category]],
    tags3: List[List[String]],
    tags4: List[String],
    tags5: List[List[List[Category]]],
) derives PactGen

@main def hello(): Unit =
  val pact = PactGen[List[String]].generatePactBody()
  println(pact)
