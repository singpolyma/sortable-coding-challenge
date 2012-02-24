GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8

.PHONY: all shell clean

all: match Main.class

match: match.hs
	ghc $(GHCFLAGS) -o $@ $^

Main.class: match.scala jackson.jar com/codahale/jerkson/Json.class
	scalac -classpath .:jackson.jar -optimise -unchecked match.scala

com/codahale/jerkson/Json.class: jackson.jar com/codahale/jerkson/JsonSnakeCase.class
	find com/codahale/jerkson/ -name '*.scala' | xargs scalac -classpath .:jackson.jar

com/codahale/jerkson/JsonSnakeCase.class: jackson.jar
	find com/codahale/jerkson/ -name '*.java' | xargs javac -classpath jackson.jar

report.html: match.hs
	-hlint $(HLINTFLAGS) --report $^

shell:
	ghci $(GHCFLAGS)

clean:
	find . -name '*.o' -o -name '*.hi' | xargs $(RM)
	find . -name '*.class' | xargs $(RM)
	$(RM) match
