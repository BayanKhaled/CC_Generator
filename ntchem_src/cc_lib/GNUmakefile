# Makefile for cc_lib
#              ?? Nov 2011 Written by  Yoshinobu Akinaga
#              03 Mar 2015 Modifled by Takahito Nakajima
#
include ../../config/makeconfig

objects = \
cc_copyresidue.o \
cc_writev.o \
cc_readv.o \
cc_readmoint.o \
cc_readamp.o \
cc_updatet1_qn.o \
cc_updatet2_qn.o \
cc_updatet3_qn.o \
cc_updatet4_qn.o \
cc_updatet5_qn.o \
cc_updatet1_qn_ascc.o \
cc_updatet2_qn_ascc.o \
cc_updatet3_qn_ascc.o \
cc_updatet4_qn_ascc.o \
cc_updatet5_qn_ascc.o \
cclib_clock.o \
cclib_dclear.o \
cclib_initarcwgt.o \
cclib_int2char.o \
cclib_makearcwgt.o \
cclib_mergestrings.o \
cclib_normvec_d.o \
cclib_normvec_z.o \
cclib_ncomb.o \
cclib_stringaddress.o \
cclib_zclear.o \
socc_copyresidue.o \
socc_writev.o \
socc_readv.o \
socc_readmoint.o \
socc_readamp.o \
socc_updatet1_qn.o \
socc_updatet2_qn.o \
socc_updatet3_qn.o \
socc_updatet4_qn.o \
socc_updatet5_qn.o

all: objcomp

objcomp : $(objects)

clean :
	$(RM) *.o *.mod *.a

veryclean :
	$(RM) *.o *.mod *.a

.SUFFIXES:.f90
.f90.o:
	$(F90C) $(F90FLAGS) -I$(NTQC_TOP)/src/cc_mod -c $<
