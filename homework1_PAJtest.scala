import prajwal.homework1_PAJ.hw1.*

import org.scalatest._
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class homework1_PAJtest extends featurespec.AnyFeatureSpec with GivenWhenThen{
  Feature("Sets Operation"){
    Scenario("newSet"){
      Assign("thirdSet",newSet("thirdSet",11,12,13,14)).eval()
      assert(Var("thirdSet").eval() == Set(11,12,13,14))
    }
    Scenario("Inserting elements into the Set"){
      Assign("firstSet",setInsert("firstSet",9,10)).eval()
      assert(Var("firstSet").eval() == Set(6,7,8))
    }
    Scenario("Deleting elements from the Set"){
      Assign("firstSet",setDelete("firstSet",2,3)).eval()
      assert(Var("firstSet").eval() == Set(1,2))
    }

  }
}






