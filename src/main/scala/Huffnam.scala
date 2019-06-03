object Huffnam extends App{

  sealed trait HuffTree {
    def weight: Int
    def chars: List[Char]
  }

  case class Leaf(chars: List[Char], weight: Int) extends HuffTree
  case class Node(left: HuffTree, right: HuffTree, chars: List[Char], weight: Int) extends HuffTree



  def toLeafs(leafs: List[(Char, Int)]): List[HuffTree] = leafs.map(leaf => Leaf(List(leaf._1), leaf._2))

  def joinTree(left: HuffTree, right: HuffTree): HuffTree =
    Node(left, right, left.chars ++ right.chars, left.weight + right.weight)

  def getHuffTree(pairs: List[(Char, Int)]): HuffTree = {
    val leafs = toLeafs(pairs);

    def iterate(nodes: List[HuffTree]): HuffTree = nodes match {
      case left :: right :: Nil => joinTree(left, right)
      case left :: right :: rest => {
        val node = joinTree(left, right)
        val newNodes = node :: rest
        iterate(newNodes.sortBy(_.weight))
      }
    }

    iterate(leafs)
  }


  def getCodeForLetter(ch: Char, tree: HuffTree): List[Int] = tree match {
    case Leaf(_, _) => Nil
    case Node(left, right, _, _) =>
      if (left.chars.contains(ch)) 0 :: getCodeForLetter(ch, left)
      else 1 :: getCodeForLetter(ch, right)
  }

  def encode(str: String, tree: HuffTree): List[Int] =
    str.toList.flatMap{ getCodeForLetter(_, tree) }

  def getCharByCode(code: List[Int], tree: HuffTree): (Char, List[Int]) = tree match {
    case Leaf(chars, _) => (chars.head, code)
    case Node(left, right, _, _) =>
      if (code.head == 0)
        getCharByCode(code.tail, left)
      else getCharByCode(code.tail, right)
  }
  def decode(code: List[Int], tree: HuffTree): List[Char] = code match {
    case Nil => Nil
    case _ => {
      val (char, list) = getCharByCode(code, tree)
      char :: decode(list, tree)
    }
  }

  val tree = getHuffTree(List(('x', 9), ('y', 6), ('h', 1), ('k', 1), ('m', 1), ('w', 1)))

  val text = "wkymhx"
  val code = encode(text, tree);
  val letters = decode(code, tree)
  print(text == letters.mkString)

}
