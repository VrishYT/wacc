all:
	sbt compile assembly

test:
	./run_test.sh

clean:
	sbt clean && rm -rf wacc-11-compiler.jar

.PHONY: all clean
