package wacc
package front

import parsley.Parsley
import parsley.position.pos
import parsley.implicits.zipped.Zipped2
import parsley.implicits.zipped.Zipped3
import parsley.implicits.zipped.Zipped4


object ParserBridge {

  /*defined our own parser bridges that can take in positions as metadata
      con defines the constructor for each of the parser bridges
      we have the apply function of parser bridges that only takes in types and not the position
      so that we can call the apply function that takes in a position within the parser bridge
      and avoid passing the pos as a parameter while parsing.
    */

  trait ParserSingletonBridge[+A] {
    def con(pos: (Int, Int)): A

    def <#(op: Parsley[_]): Parsley[A] = pos.map(this.con(_)) <* op
  }

  trait ParserBridgePos0[+A] extends ParserSingletonBridge[A] {
    this: A =>
    override final def con(pos: (Int, Int)): A = this
  }

  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridge[A => B] {
    def apply(x: A)(pos: (Int, Int)): B

    def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(this.apply(_) _)

    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridge[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C

    def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] =
      pos <**> (x, y).zipped(this.apply(_, _) _)

    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridge[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]): Parsley[D] =
      pos <**> (x, y, z).zipped(this.apply(_, _, _) _)

    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridge[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, a: D)(pos: (Int, Int)): E

    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], a: Parsley[D]): Parsley[E] =
      pos <**> (x, y, z, a).zipped(this.apply(_, _, _, _) _)

    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }
}
