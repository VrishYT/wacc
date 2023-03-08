all: compiler

compiler:
	sbt compile assembly

test:
	sbt test

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

.PHONY: all clean compiler test
