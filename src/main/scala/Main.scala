enum Tree[+T] derives PrettyPrint:
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(value: T)

case class Animal(name: String, age: Int, breed: String, demestic: Boolean) derives PrettyPrint

@main def hello(): Unit =
  val animal = Animal("cat", 2, "normal", true).print
  val en = Tree.Branch(Tree.Leaf(1), Tree.Leaf(2)).print
  println(animal)
  println(en)
