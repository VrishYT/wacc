all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-11-compiler.jar

.PHONY: all clean
