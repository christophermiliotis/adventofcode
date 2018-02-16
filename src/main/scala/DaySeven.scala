
object DaySeven {

  def bottonProgramName(input: Seq[Node]): String = {
    val parentChildrenMap =
      input
        .filter(isParent)
        .map(n => (n.name, n.children))
        .toMap

    parentChildrenMap.map { case (parent, children) =>
      val childrenDepths = children.map { child =>
        getDepth(parentChildrenMap, child, 0)
      }
      (parent, childrenDepths.max)
    }.maxBy(_._2)._1
  }

  private def getDepth(parentChildrenMap: Map[String, Seq[String]], child: String, startingDepth: Int): Int = {
    parentChildrenMap.get(child) match {
      case Some(children) => children.map(c => getDepth(parentChildrenMap, c, startingDepth + 1)).max
      case None => startingDepth + 1
    }
  }

  private def isParent(n: Node): Boolean = {
    n.children.nonEmpty
  }
}

case class Node(name: String, weight: Int, children: Seq[String])
