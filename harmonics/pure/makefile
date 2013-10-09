default: build

build:
	ghc --make -fspec-constr-count=16 -O2 -rtsopts SimRunner.hs
	ghc --make -fspec-constr-count=16 -O2 -rtsopts SampleGraphApp.hs
	HLint . --report -i 'Use camelCase' -i 'Reduce duplication'

prof:
	ghc --make -fspec-constr-count=16 -O2 -prof -auto-all -caf-all -fforce-recomp -rtsopts SimRunner.hs

clean:
	rm *.hi *.o SimRunner HarmonicsTests SampleGraphApp

test:
	ghc HarmonicsTests.hs
	./HarmonicsTests




