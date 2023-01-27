all: compiler

compiler:
	sbt compile assembly

test:
	sbt test

clean:
	sbt clean && rm -rf wacc-11-compiler.jar

.PHONY: all clean
