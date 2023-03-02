all: compiler

compiler:
	sbt compile assembly

test_parse:
	sbt 'testOnly *ValidParseTests *InvalidParseTests'

test_typecheck:
	sbt 'testOnly *ValidTypecheckerTests *InvalidTypecheckerTests'

test_compile:
	sbt 'testOnly *ExecutionTest'

test:
	sbt test

clean:
	sbt clean && rm -rf wacc-11-compiler.jar

.PHONY: all clean compiler test test_parse test_typecheck test_compile
