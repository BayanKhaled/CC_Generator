# Makefile for cc_mod
#
include ../../config/makeconfig

modules = \
cc_constant_module.o \
cc_module.o \
lrcc_module.o

all: modcomp

modcomp : $(modules)

clean :
	$(RM) *.o *.mod

veryclean :
	$(RM) *.o *.mod 

.SUFFIXES:.f90
.f90.o: 
	$(F90C) $(F90FLAGS) -c $<
