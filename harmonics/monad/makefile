default: build test

build:
	ghc -O2 SampleApp.hs
	ghc -O2 SampleGraphApp.hs

test:
	runghc PriceBarTests.hs
	runghc ExtremumMatcherTests.hs
	runghc PatternMatcherTests.hs

