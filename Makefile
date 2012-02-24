GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use head' -i 'Use string literal' -i 'Use list comprehension' --utf8

match: match.hs
	ghc $(GHCFLAGS) -o $@ $^

report.html: match.hs
	-hlint $(HLINTFLAGS) --report $^

.PHONY: shell clean

shell:
	ghci $(GHCFLAGS)

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) match
