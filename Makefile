all: compiler

compiler:
	sbt compile assembly

test:
	sbt test

test_front: test_parse test_typecheck

test_parse:
	sbt 'testOnly *ValidParseTests *InvalidParseTests'

test_typecheck:
	sbt 'testOnly *ValidTypecheckerTests *InvalidTypecheckerTests'

test_execution:
	sbt 'testOnly *ExecutionTest'

test_unit:
	sbt 'testOnly *ConditionTests *MemoryAllocatorTests *OperandTests *RegisterAllocatorTests'

clean:
	sbt clean && rm -rf wacc-11-compiler.jar

.PHONY: all clean compiler test test_front test_parse test_typecheck test_execution test_unit
