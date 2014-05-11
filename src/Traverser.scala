import scala.collection.mutable.SetBuilder
import scala.collection.mutable.ListBuffer

object Traverser {
    
   def printTheTree(root: Node) {
       walkTheTree(root, node => println(node.toStrinAll))
   }
   
   def walkTheTree(node: Node, f: Node => Unit) {
       if (node == null) {
           return
       }
       if (node.children == null || node.children.size == 0) {
    	   f(node)
           return
   	   }
       for (child <- node.children) {
           walkTheTree(child, f)
       }
       f(node)
   }
    
    def getAssebly(root: Node): String = {
        val sb = new StringBuilder
        traverse(root, sb)
        sb.toString
    }
    
    def traverse(node: Node, sb: StringBuilder) {
       // if no children print
       if (node.children == null || node.children.size == 0) {
           sb.append(getText(node))
           return
       }
       // for all children -> traverse
       for (child <- node.children) {
           traverse(child, sb)
       }
       // print
       sb.append(getText(node))
    }

    def getAllNodes(node: Node): Set[Node] = {
       val descendants = collection.mutable.Set[Node]()
       descendants ++= node.children
       for (child <- node.children) {
           descendants ++= getAllNodes(child)
       }
       descendants.toSet
    }
    
    def getText(node: Node): String = {
        // TODO if level 6: push TEXT
        // TODO referenced

		if (node.identifier == "FUNCTION")
		    "?"
		else if (node.identifier == "PARAMETERS")
		    "?"
		else if (node.identifier == "IDENT")
		    ""
		else if (node.identifier == "DEDENT")	
		    ""
		else if (node.identifier == "EXIT")	
		    "DB 0\n"
		else if (node.identifier == "ASSIGN")
		    "POP a\n" +
		    "POP b\n" +
		    "MOV [a], b\n"
		else if (node.identifier == "PRINT") 
		    "POP a\n" +
		    "MOV [232] a\n"
		else if (node.identifier == "IF") 
		    "POP a\n" +
		    "MOV b 0\n" +
		    "JE "+node.id_if+"\n"
		else if (node.identifier == "RETURN") 	
		    "?"
		else if (node.identifier == "EQUALS") 
		    getConditionalBoilerPlate(node, "JE")
		else if (node.identifier == "MULTIPLY")   
		    getMathBoilerPlate("MUL")
		else if (node.identifier == "DIVIDE") 	
		    getMathBoilerPlate("DIV")
		else if (node.identifier == "ADD")  		
		    getMathBoilerPlate("ADD")
		else if (node.identifier == "SUBTRACT")  	
			getMathBoilerPlate("SUB")	    
		else if (node.identifier == "ADDRESS")  	
		    "PUSH t\n"
		else if (node.identifier == "VALUE")  	
		    "PUSH t\n"
		else if (node.identifier == "NUMBER")  	
		    "PUSH t\n"
		else
		    throw new IllegalArgumentException("Node has illegal identifier: "+node.identifier)
    }
    
    def getMathBoilerPlate(instruction: String) = {
        "POP a\n" +
        "POP b\n" +
        instruction +" a, b\n" +
        "PUSH a\n"
    }
    
    def getConditionalBoilerPlate(node: Node, instruction: String) = {
    	"POP a\n" +
    	"POP b\n" +
    	"CMP a, b\n" +
    	instruction +" "+ node.id +"-true\n" +
    	"PUSH 0\n" +
    	"JMP "+ node.parentsId +"\n" +
    	node.id +"-true: PUSH 1\n"
    }
    



}