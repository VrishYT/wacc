package wacc 
import parsley.Parsley
import parsley.position._
import parsley.implicits.zipped.Zipped2
import parsley.implicits.zipped.Zipped3

object ParserBridge {

    trait ParserBridgePos0[+A] {
        def con(pos: (Int, Int)): A
        def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
    }

    trait ParserBridgePos1[-A, +B] extends ParserBridgePos0[A => B]{
        def apply(x: A)(pos: (Int, Int)): B
        def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)
        override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
    }

    trait ParserBridgePos2[-A, -B, +C] extends ParserBridgePos0[(A, B) => C]{
        def apply(x: A, y: B)(pos: (Int, Int)): C
        def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] =
            pos <**> (x, y).zipped(this.apply(_, _) _)
        override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
    }

    trait ParserBridgePos3[-A, -B, -C, +D] extends ParserBridgePos0[(A, B, C) => D]{
        def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
        def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]) : Parsley[D]  =
            pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
        override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
    }
}
