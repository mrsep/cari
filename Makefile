BINDIR  = bin
FLAGS   = -g
FC      = ifort
FCFLAGS = $(FLAGS)
CC		  = gcc
CCFLAGS = $(FLAGS)

all: modules programs doc

programs: ulp ulp_gf inspect intinq realinq

modules: cari.o viscari.o ieeearith.o ivalarith.o fi_lib.o ivaltaylor.o

libs: fi_lib.a

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

bin:
	mkdir bin

ivalarith.o: cari.o ieeearith.o
ivaltaylor.o: fi_lib.o

fi_lib.a: fi_lib.o
	cp fi_lib/fi_lib.a fi_lib.a
	ar rv fi_lib.a fi_lib.o

fi_lib.o: ivalarith.o fi_lib/fi_lib.a
	$(FC) $(FCFLAGS) -c fi_lib.f90

fi_lib/fi_lib.a: 
	$(MAKE) -C fi_lib/ 

ulp: bin cari.o
	$(FC) $(FCFLAGS) cari.o ulp.f90 -o $(BINDIR)/ulp

ulp_gf: bin cari.o
	$(FC) $(FCFLAGS) cari.o ulp_gf.f90 -o $(BINDIR)/ulp_gf

inspect: bin cari.o viscari.o
	$(FC) $(FCFLAGS) cari.o viscari.o inspect.f90 -o $(BINDIR)/inspect

intinq: bin
	$(FC) $(FCFLAGS) intinq.f90 -o $(BINDIR)/intinq

realinq: bin
	$(FC) $(FCFLAGS) realinq.f90 -o $(BINDIR)/realinq

itmultest: $(OBJ)
	$(FC) $(FCARGS) $^ itmultest.f90 $(LIB) -o $@

itdivtest: $(OBJ)
	$(FC) $(FCARGS) $^ itdivtest.f90 $(LIB) -o $@

itfuntest: $(OBJ)
	$(FC) $(FCARGS) $^ itfuntest.f90 $(LIB) -o $@


doc:
	doxygen Doxyfile

clean:
	rm -rf doc bin
	rm -f *.o	*.mod *.a
	$(MAKE) -C fi_lib/ clean

