import scala.collection.mutable.ListBuffer

class Node(	 
	val text: String,
	val id: List[Int],
	var children: ListBuffer[Node] = new ListBuffer[Node], 
	var isReferenced: Boolean = false	
	) {
	override def toString() = text
	def toStrinAll() = text+" ; "+id+" ; "+isReferenced+" ; "+children
	def identifier = "[A-Z_]+".r.findFirstIn(text).get
	def parentsId = id.dropRight(1)
	def id_if = Compiler.transposeId_if(id)
	def id_jump = Compiler.transposeId_jump(id) 
	//TODO get id as string
}

/**
 * nameless
 */
object Compiler {
   val DEBUG = false
   
   def buildExecutionTree(parsedSource: List[String]): Node = {
       val root = expandNode(parsedSource, List(0))
       setIsReferencedFlags(root)
       return root
   }
   
   ////////////////////////////////////////////////

   def expandNode(tokens: List[String], nodeId: List[Int]): Node = {
       if (tokens != null)
    	   println(tokens)

       if (tokens == null || tokens.isEmpty) {
           println("############ EXPAND NODE NULL ##############: " + tokens +" "+ nodeId)
           null
       } else if (tokens.size == 1) {
           new Node(tokens.head, nodeId)
       } else {
           moreThanOneChild(tokens, nodeId);        
       }
   } 
   
   def moreThanOneChild(tokens: List[String], nodeId: List[Int]): Node = {
       if (getIdentifier(tokens.head).get == "IDENT") {
           println("IDENT is head")
           processDividers(tokens, nodeId)
       } else {
           processOperators(tokens, nodeId)
       }
   }
   
   def getIdentifier(token: String) = "[A-Z_]+".r.findFirstIn(token)
   def getNumber(token: String) = token.split(",").head

   def getIndexesOfHighestOperators(tokens: List[String]) = {
	   val sortedTokens = tokens.sorted
       val highestOperator = sortedTokens.head
       val highestOperatorNumber = getNumber(highestOperator)
       val highestOperators = sortedTokens.takeWhile(text => text.startsWith(highestOperatorNumber.toString))
       val indexesOfHighestOperators = new ListBuffer[Int]
       for (oneOfHighestOperators <- highestOperators) {
           indexesOfHighestOperators += tokens.indexOf(oneOfHighestOperators)
       }
       if (DEBUG) Util.printAll2(tokens, sortedTokens, highestOperator, highestOperators, indexesOfHighestOperators)
	   indexesOfHighestOperators
   }
   
   def processDividers(tokensIn: List[String], nodeId: List[Int]): Node = {
       val node = new Node(tokensIn.head, nodeId)
       val tokens = tokensIn.tail
       val indexesOfHighestOperators = getIndexesOfHighestOperators(tokens)
       val sortedIndexesOfHighestOperators = (indexesOfHighestOperators.sorted :+ tokens.size).toList

       for (i <- 0 to sortedIndexesOfHighestOperators.size - 2) {
           val descendants = tokens.slice(sortedIndexesOfHighestOperators(i), sortedIndexesOfHighestOperators(i+1)) 
           val children = expandNode(descendants, nodeId:+ i)
           node.children += children
       }
       //////
       if (DEBUG) Util.printAll(tokens, indexesOfHighestOperators, tokens.head, sortedIndexesOfHighestOperators, nodeId)
       node
   }
   
   def processOperators(tokens: List[String], nodeId: List[Int]): Node = {
       val indexesOfHighestOperators = getIndexesOfHighestOperators(tokens)
       val sortedIndexesOfHighestOperators = indexesOfHighestOperators.toList.sorted
       val operatorsIndex = sortedIndexesOfHighestOperators.head
   
       if (operatorsIndex == 0) { // Prefix Operator
           prefixOperator(tokens, nodeId)
       } else { // Operator between parameters
           inBetweenOperator(tokens, nodeId, operatorsIndex)
       }
   }
   
   def prefixOperator(tokens: List[String], nodeId: List[Int]): Node = {
       val node = new Node(tokens.head, nodeId)
       node.children += expandNode(tokens.tail, nodeId:+ 0)
       node
   }
   
   def inBetweenOperator(tokens: List[String], nodeId: List[Int], operatorsIndex: Int): Node = {
       val node = new Node(tokens(operatorsIndex), nodeId)
       node.children += expandNode(tokens.slice(0, operatorsIndex), nodeId:+ 0)
       node.children += expandNode(tokens.slice(operatorsIndex+1, tokens.size), nodeId:+ 1)
       node
   }
   
   ///////////////////////////////////////////////////////
   
   def setIsReferencedFlags(root: Node) {
       val nodes = Traverser.getAllNodes(root)
       var referencedNodes = getReferencedNodes(nodes)
       setFlags(nodes, referencedNodes)
       println("FLAGS nodes: "+nodes)
       println("FLAGS referencedNodes: "+referencedNodes)
   }
   
   def setFlags(nodes: Set[Node], referencedNodes: Set[List[Int]]) {
       for (node <- nodes) {
           if (referencedNodes.contains(node.id)) {
               node.isReferenced = true
           }
       }
   }
   
   def getReferencedNodes(nodes: Set[Node]) = {
       var referencedNodes: Set[List[Int]] = Set()
       for (node <- nodes) {
           val identifier = "[A-Z_]+".r.findFirstIn(node.text)
           if (identifier.get == "IF") {
               println ("FLAGS node id: "+node.id)
               referencedNodes += node.id_if
           }
           if (identifier.get == "JUMP_BACK") {
               println ("FLAGS node id: "+node.id)
               referencedNodes += node.id_jump
           }
       }
       referencedNodes
   }
   
   def transposeId_if(id: List[Int]) = {
       val partial = id.dropRight(1)
       val lastNumber = partial.last + 1
       partial.dropRight(1) :+ (lastNumber)
   }
   
   def transposeId_jump(id: List[Int]) = {
       val partial = id.dropRight(2)
       val lastNumber = partial.last
       if (lastNumber < 2) {
           partial.dropRight(1)
       } else {
    	   partial.dropRight(1) :+ (lastNumber-2)
       }
   }
   
}