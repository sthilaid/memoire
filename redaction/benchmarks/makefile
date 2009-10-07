
GSI = gsi
GSC = gsc

meroon-path=MeroonV3-2008Mar01
meroon-prelude=-e '(load "$(meroon-path)/_meroon.o1")'

oops-path=oops/src
oops-prelude=-e '(parameterize ((current-directory "$(oops-path)")) (load "oops"))'

class-path=class
class-prelude=-e '(current-directory "$(class-path)") (load "lib/scm-lib")'

define-type-prelude=

all: define-type meroon oops class

define-type:  define-type-benchmark.o1
	@echo "*******************************************"
	@echo " Performing Gambit-C define-type benchmark"
	@echo "*******************************************"
	$(GSI) $(define-type-prelude) $^

define-type-benchmark.o1: define-type-benchmark.scm
	$(GSC) -o $@ $(define-type-prelude) $^

meroon:  meroon-benchmark.o1
	@echo "***************************************"
	@echo "      Performing Meroon benchmark"
	@echo "***************************************"
	$(GSI) $(meroon-prelude) $^

meroon-benchmark.o1: meroon-benchmark.scm
	$(GSC) -o $@ $(meroon-prelude) $^

oops: oops-benchmark.o1
	@echo "***************************************"
	@echo "       Performing OOPS benchmark"
	@echo "***************************************"
	$(GSI) $(oops-prelude) $^

oops-benchmark.o1: oops-benchmark.scm
	$(GSC) -o $@ $(oops-prelude) $^

class: class-benchmark.o1
	@echo "***************************************"
	@echo "      Performing Class benchmark"
	@echo "***************************************"
	$(GSI) $(class-prelude) ../$<

class-benchmark.o1: class-benchmark.scm
	$(GSC) -o ../$@ $(class-prelude) ../$<
