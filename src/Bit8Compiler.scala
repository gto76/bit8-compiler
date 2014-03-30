import scala.reflect.macros.TreeBuilder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Bit8Compiler {

   class Node(val s: String, val c: ListBuffer[Node] = new ListBuffer[Node])
   
   def main(args: Array[String]) {
       val program = "increase a {\n" +
		   "	a = a + 1\n" +
		   "	return a\n" +
		   "}" +
		   "\n" +
           "main {\n" +
		   "	a = 5\n" +
		   "	b = increase 5\n" +
		   "	print b\n" +
		   "}\n" +
		   "\n"
		   
	 	val program2 = "main {\n" +
	 			"	i = 0\n" +
	 			"	while i < 5 {\n" +
	 			"		i = i + 1\n" +
	 			"		print i\n" +
	 			"	}\n" +
	 			"}\n"
		   
	 val testProgram = program2
	 val parsedProgram = parse(testProgram)
	 print(testProgram)
	 print(parsedProgram)
	 //val tree = buildTree(parsedProgram)
	 //print(tree.s)
	 //walkTheTree(tree)
   }
  
   def walkTheTree(node: Node) {
       if (node.c.size == 0) {
           print(node.s)
           return
   		}
       for (child <- node.c) {
           walkTheTree(child)
       }
   }
   
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
   
   //////////////////////////////////////////////////////////
   
   def parse(program : String): List[String] = {
       val lines =  program.split("\n")
       var prgOut = new ListBuffer[String]  
       var lineNo = 1
       for (line <- lines) {
           prgOut.appendAll(parseLine(line, lineNo))
           lineNo = lineNo + 1
       }
       prgOut.toList
   }
   
   /*
    * 0	IDENT DEDENT LB FUNCTION
    * 1	ASSIGN PRINT IF WHILE JUMP_BACK RETURN PARAMETERS EXIT
    * 2 EQUALS NE GT ...
    * 3 MULTIPLY DIVIDE
    * 4 ADD SUBTRACT
    * 5 FUNCTION_CALL
    * 6 NUMBER ADDRESS VALUE
    */
  
   /*
   "main {" +
   "	a = 5" +
   "	b = increase 5" +
   "	print b" +
   "}" +
   "" +
   "increase a {" +
   "	a = a + 1" +
   "	return a" +
   "}
   */
  
   val number = "[0-9]+"
   val identifier = "[a-zA-Z][a-zA-Z0-9]*"
   
   val functionNames = new ListBuffer[String]
   var depth = 0
   val whileJump = new HashMap[Int, Boolean]
   
   def parseLine(lineIn: String, lineNo: Int) = {
       val line = lineIn.replaceAll("\\s+", " ").trim
       var parsedLine = new ListBuffer[String]
       var tokenNo = 0
       for (token <- line.split(" ")) {
           if (depth == 0) {
	           
               if (token == "{") {
	               parsedLine.append(depth+ "0,IDENT," +token+","+ lineNo)
	               depth = depth + 1
	           } else if (token.matches(identifier)) {
	               if (tokenNo == 0) {
	            	   parsedLine.append(depth+ "0,FUNCTION," +token+","+lineNo)
	            	   functionNames += token
	               } else if (tokenNo == 1) {
	            	   parsedLine.append(depth+ "1,PARAMETERS," +","+lineNo)
	            	   parsedLine.append(depth+ "6,ADDRESS," +token+","+lineNo)
	               } else {
	            	   parsedLine.append(depth+ "6,ADDRESS," +token+","+lineNo)
	               }
	           }
	             
	       } else { // depth != 0
	    	   
	           if (token == "{") {
	               parsedLine.append(depth+ "0,IDENT," +token+","+lineNo)
	               depth = depth + 1
	           } else if (token == "}") {
	               if (whileJump.get(depth).isDefined) {
	            	   parsedLine.append(depth+ "1,JUMP_BACK," +","+lineNo)
	            	   whileJump.remove(depth)
	               }
	               depth = depth - 1
	        	   parsedLine.append(depth+ "0,DEDENT," +token+","+lineNo)
	        	   if (depth == 0 && functionNames.last == "main") {
	        		   parsedLine.append(depth+ "1,EXIT," +","+lineNo)
	        	   }
	           } else if (token == "=") {
	        	   parsedLine.append(depth+ "1,ASSIGN," +token+","+lineNo)
	           } else if (token == "print") {
	        	   parsedLine.append(depth+ "1,PRINT," +token+","+lineNo)
	           } else if (token == "if") {
	        	   parsedLine.append(depth+ "1,IF," +token+","+lineNo)
	           } else if (token == "while") {
	        	   parsedLine.append(depth+ "1,WHILE," +token+","+lineNo)
	        	   whileJump.put(depth+1, true)
	           } else if (token == "return") {
	        	   parsedLine.append(depth+ "1,RETURN," +token+","+lineNo)
	           } else if (token == "==") {
	        	   parsedLine.append(depth+ "2,EQUALS," +token+","+lineNo)
	           } else if (token == "!=") {
	        	   parsedLine.append(depth+ "2,NOT_EQUALS," +token+","+lineNo)
	           } else if (token == ">") {
	        	   parsedLine.append(depth+ "2,GRATER_THAN," +token+","+lineNo)
	           } else if (token == "<") {
	        	   parsedLine.append(depth+ "2,SMALLER_THAN," +token+","+lineNo)
	           } else if (token == ">=") {
	        	   parsedLine.append(depth+ "2,GRATER_OR_EQUAL," +token+","+lineNo)
	           } else if (token == "<=") {
	        	   parsedLine.append(depth+ "2,SMALER_OR_EQUAL," +token+","+lineNo)
	           } else if (token == "*") {
	        	   parsedLine.append(depth+ "3,MULTIPLY," +token+","+lineNo)
	           } else if (token == "/") {
	        	   parsedLine.append(depth+ "3,DIVIDE," +token+","+lineNo)
	           } else if (token == "+") {
	        	   parsedLine.append(depth+ "4,ADD," +token+","+lineNo)
	           } else if (token == "-") {
	        	   parsedLine.append(depth+ "4,SUBTRACT," +token+","+lineNo)
	           } else if (token.matches(identifier)) {
	               if (functionNames.contains(token)) {
	                   parsedLine.append(depth+ "5,FUNCTION_CALL," +token+","+lineNo)
	               } else if (tokenNo == 0) {
	            	   parsedLine.append(depth+ "6,ADDRESS," +token+","+lineNo)
	               } else {
	            	   parsedLine.append(depth+ "6,VALUE," +token+","+lineNo)
	               }
	           } else if (token.matches(number)) {
	        	   parsedLine.append(depth+ "6,NUMBER," +token+","+lineNo)
	           } 
	           
	           /*
	           else if (token == "") {
	        	   parsedLine.append(depth+ ",," +token+","+lineNo)
	           } 
	           */
	             
	       }
           tokenNo = tokenNo + 1
      }
      parsedLine.append(depth+"0,LINE_BREAK,\\n,"+lineNo)
      parsedLine.toList
   }
   
}