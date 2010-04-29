BINDIR = bin
FC     = ifort
FCARGS = 


all: modules programs doc

programs: ulp ulp_gf inspect intinq realinq

modules: cari.o viscari.o ieeearith.o

%.o: %.f90
	$(FC) $(FCARGS) -c $<

bin:
	mkdir bin

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
	rm *.o
	rm *.mod

