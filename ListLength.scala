/**
  * QUS1 : Compute the length of a list using method foldRight. The signature of the method should be:
  * def length[A](l: List[A]): Int

  * QUS2 : Implement a method hasSubsequence for checking whether a List contains another List as a subsequence.
  * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences.
  * The signature of the method should be:
  * def hasSubsequence[A](list:List[A],sub:List[A]):Boolean

  *
  * Created by prashant on 24-01-2017.
  */

class ListAssignment {                            //class that will provide methods as solution to assignment.

  def length(list: List[Any]): Int = {
    val listSize = list.foldRight(0) { (_, i) => i + 1 }
    listSize
  }                                          // first method ends returning back the length of list being passed to it.


  def hasSubsequence(mainList: List[Any], subList: List[Any]): Boolean = {
    val result = mainList.containsSlice(subList)
    result
  }                               // second method ends returning a boolean depending on subsequence being found or not.

  def customFunction(x : Int) : Boolean = x % 2 == 0

  def splitList(list : List[Int]) : (List[Int],List[Int]) = {
    list.groupBy(customFunction)
    (list.groupBy(customFunction)(true),list.groupBy(customFunction)(false))
  }                              // splits the given list on given split field.

  def concatenateLists(list1: List[Int], list2: List[Int]) : List[Int] = {
    list1 match{
      case Nil => list2
      case head :: tail => head :: concatenateLists(tail,list2)
      case _ => List()
    }
  }                             // provides for concatenation of the lists.


}

object list_length {
  def main(args: Array[String]): Unit = {
    val myList1 = List(10,20,8,7,4,5,44,54,12)
    val myList2 = List(20,8,7,4)
    val lengthObject = new ListAssignment

    val length = lengthObject.length(myList1)       // call for length method.
    println(s"length of the list being passed is :: $length \n\n")

    val sequence_found = lengthObject.hasSubsequence(myList1, myList2)      // call for subsequence matcher method.
    println(s"result of subsequence search is :: $sequence_found \n\n")

    println("lists after splitting are "+lengthObject.concatenateLists(myList1,myList2))  // call for concatenation on lists.

    println("\n\nlists after splitting are "+ lengthObject.splitList(myList1))      // call for splitting of list.
  }

}
