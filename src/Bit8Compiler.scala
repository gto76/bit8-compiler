import scala.reflect.macros.TreeBuilder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Bit8Compiler {
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
	 val parsedProgram = Parser.parse(testProgram)
	 println(testProgram)
	 println(parsedProgram)
	 
	 val root = Compiler.buildExecutionTree(parsedProgram);
     Compiler.printTheTree(root)
	 //val tree = buildTree(parsedProgram)
	 //print(tree.s)
	 //walkTheTree(tree)
   }
   
}