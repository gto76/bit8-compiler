import scala.collection.mutable.ListBuffer

/**
 */
object Compiler {
   
   def buildExecutionTree(parsedSource: List[String]): Node = {
       expandNode("ROOT", List(0), parsedSource)
   }
   
   class Node(	
		val text: String, 
		val id: List[Int], 
		val children: ListBuffer[Node] = new ListBuffer[Node], 
		val isReferenced: Boolean = false	
   )

   def expandNode(nodeText: String, nodeId: List[Int], tokens: List[String]): Node = {
       val node = new Node(nodeText, nodeId)
       ///
       if (tokens == null || tokens.isEmpty) {
           return node
       } else if (tokens.size == 1) {
           val child = expandNode(tokens.head, nodeId:+ 0, null)
           node.children.append(child)
           return node
       } else {
           moreThanOneChild(tokens, node, nodeId);        
       }
       
       node
   }
   
   def moreThanOneChild(tokens: List[String], node: Node, nodeId: List[Int]) {
       val sortedTokens = tokens.sorted
       val highestOperator = sortedTokens.head
       val highestOperatorLevel = highestOperator.split(",").head
       val highestOperators = sortedTokens.takeWhile(text => text.startsWith(highestOperatorLevel))
       val noOfHighestOperators = highestOperator.size
       val indexesOfHighestOperators = new ListBuffer[Int]
       for (oneOfHighestOperators <- highestOperators) {
           indexesOfHighestOperators += tokens.indexOf(oneOfHighestOperators)
       }
       
       if (noOfHighestOperators > 1) { // LB, IDENT, DEDENT
           lines(indexesOfHighestOperators, node, tokens, nodeId)
       }
       else { // all other operators
           nonLines(indexesOfHighestOperators, node, tokens, nodeId)
       }
   }
   
   def lines(indexesOfHighestOperators: ListBuffer[Int], node: Node, tokens: List[String], nodeId: List[Int]) {
       val sortedIndexesOfHighestOperators = indexesOfHighestOperators.sorted.toList
       for (i <- 0 to sortedIndexesOfHighestOperators.size - 2) {
           val grandChildren = tokens.slice(sortedIndexesOfHighestOperators(i), sortedIndexesOfHighestOperators(i+1))
           val child = expandNode(tokens(sortedIndexesOfHighestOperators(i)), nodeId:+ i, grandChildren)
           node.children:+ child
       }
       val grandChildren = tokens.takeRight(sortedIndexesOfHighestOperators.last) 
       val lastChild = expandNode(tokens(sortedIndexesOfHighestOperators.last), nodeId:+ sortedIndexesOfHighestOperators.size-1, grandChildren)
       node.children:+ lastChild
   }
   
   def nonLines(indexesOfHighestOperators: ListBuffer[Int], node: Node, tokens: List[String], nodeId: List[Int]) {
       val operatorsIndex = indexesOfHighestOperators.head
       if (operatorsIndex == 0) { // Prefix Operator
           val child = expandNode(tokens.head, nodeId:+ 0, tokens.tail) // Not neceseraly
           // ok; what if child is...
           node.children:+ child
       }
       else { // Operator between parameters
           // TODO!!!
           node.children:+ expandNode(tokens(operatorsIndex), nodeId:+ 0, tokens.slice(0, operatorsIndex-1))
           node.children:+ expandNode(tokens(operatorsIndex), nodeId:+ 1, tokens.slice(operatorsIndex, tokens.size))
       }
   }
   
   ////////////////////////////
   
   def printTheTree(root: Node) {
       walkTheTree(root, node => print(node.text))
   }
   
   def walkTheTree(node: Node, f: Node => Unit) {
       if (node.children.size == 0) {
           f(node)
           return
   		}
       for (child <- node.children) {
           walkTheTree(child, f)
       }
   }
   
   /*
   def buildTree(parsedProgram: List[List[String]]): Node = {
       val root = new Node("[MAIN,]")
       for (line <- parsedProgram) {
           val index = line.indexWhere(a => a.contains("ASIGN") || a.contains("IO"))
           val operator = line(index)
           val left = line.take(index)
           val right = line.drop(index+1)
           if (operator.contains("ASIGN")) {
               val child = develop(right)
               val list = new ListBuffer[Node]
               list.append(child)
               val node = new Node(operator, list)
               root.c.append(node)
           }
           //print("node: " + node)
           //print("left: " + left)
           //print("right: " + right)
       }
       root
   }

   def develop(s : List[String]): Node = {
       if (s.length == 0) {
    	   return null
       }
       if (s.length == 1) {
           return new Node(s(0))
       }
       val index = getIndexOfHighestOperator(s)
       val operator = s(index)
       val left = develop(s.take(index))
       val right = develop(s.drop(index+1))
       //print("#" + indexOfhighestOperator)
       val node = new Node(operator)
       if (left != null)
    	   node.c.append(left)
       if (right != null)
    	   node.c.append(right)
       node
   }
   
   def getIndexOfHighestOperator(s : List[String]): Int = {
       val sorted = s.sorted
       val first = sorted(0)
       return s.indexOf(first)
   }
   */
}