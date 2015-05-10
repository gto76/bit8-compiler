//import scala.reflect.macros.TreeBuilder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object Bit8Compiler {

   val RUN_SHELL = false

   var root: Node = null
    
   def main(args: Array[String]) {
       val program = "increase x {\n" +
		   "	x = x + 1\n" +
		   "	return x\n" +
		   "}" +
		   "\n" +
           "main {\n" +
		   "	va = 5\n" +
		   "	vb = increase 5\n" +
		   "	print vb\n" +
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
	 println("### SOURCE CODE ###");
	 println(testProgram)

	 val parsedProgram = Parser.parse(testProgram)
	 println("### PARSED SOURCE ###");
	 //println(parsedProgram)

 	 root = Compiler.buildExecutionTree(parsedProgram);
	 println("\n### SOURCE TREE ###");
     Traverser.printTheTree(root)
     
     println("\n### ASSEBLY ###");
     println(Traverser.getAssebly(root))
	
	 if (RUN_SHELL) {
     	runShell()
	 }
   }
   
   def runShell() {
       var in = ""
       do {
           print("[bit8-shell]: ")
    	   in = readLine
    	   execute(in)
       } while (in != "quit" && in != "exit")
       println("END")
   }
   
   def execute(in: String) {
       if (in == "root") {
           println(root.text)
       } else if (in.matches("root.[0-9]")) {
           val i = in.replaceFirst("root.", "")
           val children = root.children.toList
           try {
        	   println(children(i.toInt).text)
           } catch {
               case e: Exception => print(e)
           }
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       } else if (in == "") {
           println()
       }
   }
   
   
}
