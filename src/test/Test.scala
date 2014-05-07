package test

import scala.collection.mutable.ListBuffer
import java.util.ArrayList

object Test {
	def main(args: Array[String]) {
//		 val identifier = "[a-zA-Z][a-zA-Z0-9]*"
//		 if ("bla".matches(identifier)) {
//		     print("OK")
//		 } else {
//		     print("FAIL")
//		 }
	    
	    val tokens = new ListBuffer[String]
	    tokens.append("first")
	    tokens.append("second")
	    tokens.append("third")
	    tokens.append("forth")
	    val tokensList = tokens.toList
	    
	    val INDEX = 3
	    println(tokensList.slice(0, INDEX-1))
	    println(tokensList.slice(INDEX, tokensList.size))
	    
	    
//	    tokens.dropRight(operatorsIndex))
//        tokens.slice(operatorsIndex, tokens.size-1)
	}
}