package wacc

import parsley.Parsley

object GenericBridges {
    
    import parsley.implicits.zipped.{Zipped2, Zipped3}
    trait ParserSingletonBridge[+A] {
        def con: A
        def <#(op: =>Parsley[_]): Parsley[A] = op #> con
    }

    trait ParserBridge0[R] extends ParserSingletonBridge[R] { this: R =>
        final override def con: R = this
    }

    trait ParserBridge1[-A, +B] extends ParserSingletonBridge[A => B] {
        def apply(x: A): B
        def apply(x: =>Parsley[A]): Parsley[B] = x.map(this.con)
        override final def con: A => B = this.apply(_)
    }

    trait ParserBridge2[-A, -B, +C] extends ParserSingletonBridge[(A, B) => C] {
        def apply(x: A, y: B): C
        def apply(x: =>Parsley[A], y: =>Parsley[B]): Parsley[C] = (x, y).zipped(this.con)
        override final def con: (A, B) => C = this.apply(_, _)
    }

    trait ParserBridge3[-A, -B, -C, +D] extends ParserSingletonBridge[(A, B, C) => D] {
        def apply(x: A, y: B, z: C): D
        def apply(x: =>Parsley[A], y: =>Parsley[B], z: =>Parsley[C]): Parsley[D] = (x, y, z).zipped(this.con)
        override final def con: (A, B, C) => D = this.apply(_, _, _)
    }
}
