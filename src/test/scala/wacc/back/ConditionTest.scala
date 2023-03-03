package wacc
package back

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConditionTests extends AnyFlatSpec with Matchers {
    "Condition AL" should "invert to NO" in {
        val i = Condition.invert(Condition.AL)
        i should equal (Condition.NO) 
    }

    "Condition NO" should "invert to AL" in {
        val i = Condition.invert(Condition.NO)
        i should equal (Condition.AL) 
    }

    "Condition EQ" should "invert to NE" in {
        val i = Condition.invert(Condition.EQ)
        i should equal (Condition.NE) 
    }

    "Condition NE" should "invert to EQ" in {
        val i = Condition.invert(Condition.NE)
        i should equal (Condition.EQ) 
    }

    "Condition LT" should "invert to GE" in {
        val i = Condition.invert(Condition.LT)
        i should equal (Condition.GE) 
    }

    "Condition GE" should "invert to LT" in {
        val i = Condition.invert(Condition.GE)
        i should equal (Condition.LT) 
    }

        "Condition GT" should "invert to LE" in {
        val i = Condition.invert(Condition.GT)
        i should equal (Condition.LE) 
    }

    "Condition LE" should "invert to GT" in {
        val i = Condition.invert(Condition.LE)
        i should equal (Condition.GT) 
    }
}