//package file prajwal
package prajwal

object homework1_PAJ {
  type BasicType = Any // BasicType will be Any

  //Creating a Map[String,Any]
  private val a: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map()
  private val ourMacro :String = "ourMacro"
  a += ourMacro -> scala.collection.mutable.Map[String , Any]()


  //enumeration with different cases
  enum hw1:
    case Value(input: BasicType)
    case Var(name: String)
    case Assign(name: String, args:Any*)//ourInput:hw1
    case newSet(name:String , args: Any*)
    case ourScope(ourScopeName: String, exp: hw1)
    case setInsert(name: String, args: Any*)
    case setDelete(name: String, args: Any*)
    case ourMacro(name: String, exp: hw1)
    case setUnion(set1: hw1, set2: hw1)
    case setIntersection(set1: hw1, set2: hw1)
    case setDifference(set1: hw1, set2: hw1)
    case setSymmetricDifference(set1: hw1, set2: hw1)
    case setCartesianProduct(set1: hw1, set2: hw1)


    // eval function using match expression with many cases
    def eval(problem: scala.collection.mutable.Map[String, Any] = a): BasicType =
      this match {
        case Value(i) => i
        case Var(name: String) => a(name)

        //case for Assign which takes String and an input value
        case Assign(name: String, ourInput: Value) => a += name -> ourInput.eval

        //case for creating a set which takes the name of the set and any number of elements(args*) of Any type and the forms a set (toSet)
        case newSet(name:String ,args*) => Set[Any]() ++ (args.map(one => one).toSet) //add name:String

        //case for inserting the elements into an existing set using scala concatenation (++) and I have used a type cast (asInstanceof) in order for the compiler to consider given inputs by the user as a set (Set[Any])
        case setInsert(name: String, args*) =>
          a(name).asInstanceOf[Set[Any]] ++ args.map(p => p).toSet

        //case for deleting the elements from an existing set and again I have used type cast (asInstanceof) in order for the compiler to consider given inputs by the user to delete the elements in the existing set as a set (Set[Any)
        case setDelete(name:String , args*) =>
          a(name).asInstanceOf[Set[Any]] -- args.map(p => p).toSet


        case ourScope(ourScopeName , hw1)=>
          ourScopeName match {
            case "None"=>
              val scoping: scala.collection.mutable.Map[String,Any] = scala.collection.mutable.Map()
              ourScopeName ++ ourScopeName -> scoping
              hw1.eval(scoping)
          }
//

        //case for the Union binary operation on sets
        case setUnion(set1: hw1, set2: hw1) =>
          (set1.eval, set2.eval) match {
            //ourSet1 and ourSet2 are the two sets and it takes the input
            case (ourSet1: Set[Any], ourSet2: Set[Any]) =>
              ourSet1.union(ourSet2) //operation for union of two sets using .union
          }

        //case for the Intersection operation on sets
        case setIntersection(set1: hw1, set2: hw1) =>
          (set1.eval, set2.eval) match {
            //ourSet1 and ourSet2 are the two sets and it takes the input
            case (ourSet1: Set[Any], ourSet2: Set[Any]) =>
              ourSet1.intersect(ourSet2) // operation for intersection of two sets using .intersect
          }

        //case for the Difference operation on sets
        case setDifference(set1: hw1, set2: hw1) =>
          (set1.eval, set2.eval) match {
            //ourSet2 and ourSet2 are the two sets and it takes the input
            case (ourSet1: Set[Any], ourSet2: Set[Any]) =>
              ourSet1.diff(ourSet2) // operation for difference of two sets using .diff
          }

        //case for the symmetric set difference operation on sets
        case setSymmetricDifference(set1: hw1, set2: hw1) =>
          (set1.eval, set2.eval) match {
            //ourSet1 and ourSet2 are the two sets and it take the input
            case (ourSet1: Set[Any], ourSet2: Set[Any]) =>
              //operation for symmetric set difference.Since we dont have a standard operation,we can use combination of difference operation and union operation to define the operation for our symmetric set difference.
              //symmetric set difference  =  (set1-set2)U(set2-set1)
              (ourSet1.diff(ourSet2)).union(ourSet2.diff(ourSet1))

          }
        //case for the cartesian product operation of sets
        case setCartesianProduct(set1: hw1, set2: hw1) =>
          (set1.eval, set2.eval) match {
            //ourSet1 and ourSet2 are the two sets and it takes the input
            case (ourSet1: Set[Any], ourSet2: Set[Any]) =>
              //operation for cartesian product of two sets.Again since we dont have a standard operation,we can use flatMap() method where it takes the predicate and applies it to each element of the collection and returns a new collection which is a new set in our case
              //so with the help of flatMap() method,element from OurSet1 is taken and paired with element from ourSet2 and then it returns a newSet with these elements which gives us the cartesian product of two sets
              //ourSet1 X ourSet2 = {(a,b): a € ourSet1, b € ourSet2}
              ourSet1.flatMap(s1 => ourSet2.map(s2 => (s1, s2)))
              // operation for the cartesian product of ourSet1 and ourSet2


          }


      }

  //main method for Assigning the elements to the set and performing all the binary operations
  @main def homework1(): Unit =
    import hw1.*
    println(newSet("firstSet",1,2,3))
    println(newSet("secondSet",4,5,6))
    println(newSet("thirdset","a","b","c","d"))
    Assign("thirdSet",newSet("thirdSet",11,12,13,14)).eval()
    Assign("firstSet",setInsert("firstSet",9,10)).eval()
    Assign("firstSet",setDelete("firstSet",2,3)).eval()
    Assign("setUnion",setUnion(Var("firstSet"),Var("secondSet"))).eval()
    Assign("setIntersection",setIntersection(Var("firstSet"),Var("secondSet"))).eval()
    ourScope("setScope" ,Assign("set1",setDelete("set1",2))).eval()
  //    Assign("set1",1,2,3,4).eval()
  //    Assign("set2",5,6,7,8).eval()
  //    Assign("set1",setInsert("set1",1,2,3,4)).eval()
  //    Assign("set3" ,setDelete("set2",6)).eval()
  //    println(setUnion(Var("set1"),Var("set2")).eval())
  //    ourMacro("setMacro" , setInsert("set1",2,9))


}
