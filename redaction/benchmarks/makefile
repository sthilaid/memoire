
GSI = gsi -:dar
GSC = gsc -prelude '(declare (not safe) (block) (standard-bindings) (extended-bindings))'

meroon-path=MeroonV3-2008Mar01
meroon-prelude=-e '(load "$(meroon-path)/_meroon.o1")'

oops-path=oops/src
oops-prelude=-e '(parameterize ((current-directory "$(oops-path)")) (load "oops"))'

class-path=class
class-prelude=-e '(current-directory "$(class-path)") (load "lib/scm-lib")'

threadsim-path=thread-simulation
threadsim-prelude=-e '(current-directory "$(threadsim-path)") (load "lib/scm-lib") (load "lib/rbtree") (load "lib/thread-simulation")'

termite-prelude=

define-type-prelude=

all: define-type meroon oops class threadsim gambit-thread termite erlang

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

threadsim: threadsim-benchmark.o1
	@echo "***************************************"
	@echo "      Performing Thread Simulation benchmark"
	@echo "***************************************"
	$(GSI) $(threadsim-prelude) ../$<

threadsim-benchmark.o1: threadsim-benchmark.scm
	$(GSC) -o ../$@ $(threadsim-prelude) ../$<

gambit-thread: gambit-thread-benchmark.o1
	@echo "***************************************"
	@echo "      Performing Gambit Thread benchmark"
	@echo "***************************************"
	$(GSI) $<

gambit-thread-benchmark.o1: gambit-thread-benchmark.scm
	$(GSC) -o $@ $<

termite: termite-benchmark.o1
	@echo "***************************************"
	@echo "      Performing Termite benchmark"
	@echo "***************************************"
	$(GSI) $(termite-prelude) $<

termite-benchmark.o1: termite-benchmark.scm
	$(GSC) -o $@ $(termite-prelude) $<


erlang: 
	@echo "***************************************"
	@echo "      Performing Erlang benchmark"
	@echo "***************************************"
	./erlang_benchmark.erl


clean:
	rm -rf *.o1