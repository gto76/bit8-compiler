import scala.collection.mutable.ListBuffer

object Util {

   /////////////////////////////////
   
   def printAll(tokens: List[String], indexesOfHighestOperators: ListBuffer[Int], 
           head: String, sortedIndexesOfHighestOperators: List[Int], nodeId: List[Int]) {
       println("### PRINT ALL ###");
       println(tokens)
       println(indexesOfHighestOperators)
       println(head)
       println(sortedIndexesOfHighestOperators)
       println(nodeId)
   }

   def printAll2(tokens: List[String], sortedTokens: List[String], highestOperator: String, 
           highestOperators: List[String], indexesOfHighestOperators: ListBuffer[Int]) {
       println("### PRINT ALL 2 ###");
       println("tokens: "+tokens);
       println("sortedTokens: "+sortedTokens);
       println("highestOperator: "+highestOperator);
       println("highestOperators: "+highestOperators);
       println("indexesOfHighestOperators: "+indexesOfHighestOperators);
   }
   
   ////////////////////////////
   
}