package wacc
package back

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OperandTests extends AnyFlatSpec with Matchers {
    
    "ImmInt operand" should "return a valid string representation of immediate integers" in {

        val i = ImmInt(6).toString
        i should equal ("#6") 

    }

    "ImmChar operand" should "return a valid string representation of immediate characters" in {

        val i = ImmChar('a').toString
        i should equal ("#97") 

    }

    "DataLabel operand" should "return a valid string representation of labels" in {

        val i = DataLabel("L.str.0").toString
        i should equal ("=L.str.0") 

    }

    "Address operand" should "return a valid string representation of addresses" in {

        val i =Address(Register(0), ImmInt(5)).toString
        i should equal ("[r0,#5]") 

    }

    "ASR operand" should "return a valid string representation of the instruction ASR" in {

        val i = ASR(Register(3), ImmInt(90)).toString
        i should equal ("r3, asr #90") 

    }

    "LSL operand" should "return a valid string representation of the instruction LSL" in {

        val i = LSL(Register(7), Register(2)).toString
        i should equal ("r7, lsl r2")
         
    }



}