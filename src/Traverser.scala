import scala.collection.mutable.SetBuilder
import scala.collection.mutable.ListBuffer

object Traverser {
    
    
    def getAssebly(root: Node): String = {
        val sb = new StringBuilder
        val nodes = getAllNodes(root)
        putVariables(nodes, sb)
        traverse(root, sb)
        sb.append("DB 0\n")
        sb.toString
    }
    
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
    
    ///////////////////
    
    def putVariables(nodes: Set[Node], sb: StringBuilder) {
        sb.append("JMP main\n")
        val variables = getVariables(nodes)
        for (variable <- variables) {
            sb.append(variable+": DB 0\n")
        }
        //sb.append("\nstart:\n")
    }
    
    def getVariables(nodes: Set[Node]) = {
        val variables = collection.mutable.Set[String]()
        for (node <- nodes) {
            if (node.identifier == "ADDRESS" || node.identifier == "VALUE") {
                variables += node.token
            }
        }
        variables.toSet
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
        addAddressIfReferenced(node) +
    	{
            if (node.identifier == "MAIN") 
                "\nmain:\n"
            else if (node.identifier == "FUNCTION") 
    	    	"\n"+node.token+":\n" +
    	        "POP d\n"
			else if (node.identifier == "PARAMETERS")
			    "PUSH d\n" // not if main TODO
			else if (node.identifier == "PARAMETER")
			    "POP a\n" +
			    "MOV ["+node.token+"], a\n"
			else if (node.identifier == "FUNCTION_CALL")
			    "CALL "+node.token+"\n"
			else if (node.identifier == "RETURN") {
			    if (node.children == null || node.children.size == 0) {
			    	"RET\n"
			    } else {
			        "POP a\n" +
			        "POP b\n" +
			        "PUSH a\n" +
			        "PUSH b\n" +
			        "RET\n"
			    }
			} 	
			else if (node.identifier == "IDENT")
			    ""
			else if (node.identifier == "DEDENT")	
			    ""
			else if (node.identifier == "LINE_BREAK")	
			    ""
			else if (node.identifier == "EXIT")	
			    "DB 0\n"
			else if (node.identifier == "ASSIGN")
			    "POP a\n" +
			    "POP b\n" +
			    "MOV [b], a\n"
			else if (node.identifier == "PRINT") 
			    "POP a\n" +
			    "MOV [232], a\n"
			else if (node.identifier == "IF") 
			    "POP a\n" +
			    "MOV b, 0\n" +
			    "JE "+node.id_if_string+"\n"
			else if (node.identifier == "JUMP_BACK") 
			    "JMP "+node.id_jump_string+"\n"   
			else if (node.identifier == "EQUALS") 
			    getConditionalBoilerPlate(node, "JE")
			else if (node.identifier == "NOT_EQUALS") 
			    getConditionalBoilerPlate(node, "JNE")
			else if (node.identifier == "GRATER_THAN") 
			    getConditionalBoilerPlate(node, "JA")
			else if (node.identifier == "SMALLER_THAN") 
			    getConditionalBoilerPlate(node, "JB")
			else if (node.identifier == "GRATER_OR_EQUAL") 
			    getConditionalBoilerPlate(node, "JAE")
			else if (node.identifier == "SMALER_OR_EQUAL") 
			    getConditionalBoilerPlate(node, "JB")
			else if (node.identifier == "MULTIPLY")   
			    getMathBoilerPlate("MUL")
			else if (node.identifier == "DIVIDE") 	
			    getMathBoilerPlate("DIV")
			else if (node.identifier == "ADD")  		
			    getMathBoilerPlate("ADD")
			else if (node.identifier == "SUBTRACT")  	
				getMathBoilerPlate("SUB")	    
			else if (node.identifier == "ADDRESS")  	
			    "PUSH "+node.token+"\n"
			else if (node.identifier == "VALUE")  	
			    "PUSH ["+node.token+"]\n"
			else if (node.identifier == "NUMBER")  	
			    "PUSH "+node.token+"\n"
			else
			    throw new IllegalArgumentException("Node has illegal identifier: "+node.identifier)
    	}
    }
    
    def addAddressIfReferenced(node: Node) = {
    	if (node.isReferenced) 
        	node.id_string + ": "
       	else
       		""
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
    	instruction +" "+ node.id_string +"_true\n" +
    	"PUSH 0\n" +
    	"JMP "+ node.id_string +"_end\n" +
    	node.id_string +"_true: PUSH 1\n"+
    	node.id_string +"_end:\n"
    }
    
}