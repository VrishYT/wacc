package wacc

import AST._

object SemanticChecker {

    import scala.collection.immutable.Map

    def typecheck(program: Program) {
        val statements = program.stats
        val functions = program.fs

        functions.foreach(func => {
            checkFunction(func)        
        })

    }

    def checkFunction(func: Func) {
        val vars = func.args.map(param => (param.id -> param.t)).toMap
        checkStatements(func.stats, vars)
    }

    def checkStatements(statements: List[Stat], vars: Map[String, Type]) {
        ???
    }
    
}