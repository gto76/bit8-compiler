import scala.collection.mutable.ListBuffer
import java.util.HashMap

object Parser {
   
   def parse(program : String): List[String] = {
       val lines =  program.split("\n")
       var prgOut = new ListBuffer[String]
       prgOut.append("00,IDENT,,0"); // root
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
   var depth = 1
   val whileJump = new HashMap[Int, Boolean]
   
   def parseLine(lineIn: String, lineNo: Int) = {
       val line = lineIn.replaceAll("\\s+", " ").trim
       var parsedLine = new ListBuffer[String]
       var tokenNo = 0
       for (token <- line.split(" ")) {
           if (depth == 1) {
	           
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
	               if (whileJump.get(depth)) { // was before: .isDefined 
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
	        	   parsedLine.append(depth+ "1,IF," +token+","+lineNo) //Old: WHILE
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