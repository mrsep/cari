BINDIR = bin
FC     = ifort
FCARGS = 


all: modules programs doc

programs: ulp ulp_gf inspect intinq realinq

modules: cari.o viscari.o ieeearith.o ivalarith.o fi_lib.o

%.o: %.f90
	$(FC) $(FCARGS) -c $<

bin:
	mkdir bin

ivalarith.o: cari.o ieeearith.o
	$(FC) $(FCARGS) -c ivalarith.f90

fi_lib.o: ivalarith.o fi_lib.a
	$(FC) $(FCARGS) cari.o ieeearith.o ivalarith.o -c fi_lib.f90 fi_lib/fi_lib.a

fi_lib.a: 
	$(MAKE) -C fi_lib/ 

ulp: bin cari.o
	$(FC) $(FCARGS) cari.o ulp.f90 -o $(BINDIR)/ulp

ulp_gf: bin cari.o
	$(FC) $(FCARGS) cari.o ulp_gf.f90 -o $(BINDIR)/ulp_gf

inspect: bin cari.o viscari.o
	$(FC) $(FCARGS) cari.o viscari.o inspect.f90 -o $(BINDIR)/inspect

intinq: bin
	$(FC) $(FCARGS) intinq.f90 -o $(BINDIR)/intinq

realinq: bin
	$(FC) $(FCARGS) realinq.f90 -o $(BINDIR)/realinq

doc:
	doxygen Doxyfile

clean:
	rm -rf doc
	rm -rf bin
	rm -f *.o
	rm -f *.mod
	$(MAKE) -C fi_lib/ clean

