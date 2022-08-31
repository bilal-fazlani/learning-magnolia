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
    tags: List[String]
) derives PactGen

@main def hello(): Unit =
  val pact = summon[PactGen[Animal]].generatePactBody()
  println(pact)
