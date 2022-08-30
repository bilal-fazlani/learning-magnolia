enum Tree[+T] derives PrettyPrint:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(value: T)

case class Animal(
    name: String,
    age: Int,
    breed: Option[String],
    demestic: Option[Boolean],
    tags: List[String]
) derives PrettyPrint

@main def hello(): Unit =
  val animal = Animal("cat", 2, Some("normal"), None, List("cat", "pet", "brown")).print
  val en = Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)).print
  println(animal)
  println(en)
