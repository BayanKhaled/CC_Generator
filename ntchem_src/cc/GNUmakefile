# Makefile for cc
#                     08 Jan 2011 Written  by Yoshinobu Akinaga
#                     12 Jan 2013 Modified by Takahito Nakajima
#
# "make cleanautogen" deletes automatically-generated programs
#
include ../../config/makeconfig

program = cc.exe

objects_base = \
cc_antisym_cbia.o \
cc_antisym_cdba.o \
cc_antisym_iabc.o \
cc_antisym_iajb.o \
cc_antisym_ijab.o \
cc_antisym_ijak.o \
cc_antisym_lkij.o \
cc_antisym_pqrs.o \
cc_arcwgt_allocate.o \
cc_arcwgt_deallocate.o \
cc_calcresidued.o \
cc_calcresidueq.o \
cc_calcresidues.o \
cc_calcresiduet.o \
cc_checkconv.o \
cc_checkmoint.o \
cc_convscf_allocate.o \
cc_convscf_deallocate.o \
cc_ecorr.o \
cc_ecorr_t1g.o \
cc_exe.o \
cc_gen08to10.o \
cc_getmoande.o \
cc_initmodule.o \
cc_initwrkt.o \
cc_main.o \
cc_mp.o \
cc_read_input.o \
cc_saveint.o \
cc_setvar.o \
cc_t2init.o \
cc_trans.o \
cc_trn1_cqrs.o \
cc_trn1_lqrs.o \
cc_trn1_pqrs.o \
cc_trn2_cqbs.o \
cc_trn2_cqis.o \
cc_trn2_iqas.o \
cc_trn2_lqis.o \
cc_trn2_pqrs.o \
cc_trn3_cbis.o \
cc_trn3_cdbs.o \
cc_trn3_cqia.o \
cc_trn3_iajs.o \
cc_trn3_iqak.o \
cc_trn3_iqas.o \
cc_trn3_iqbc.o \
cc_trn3_lkis.o \
cc_trn3_pqrs.o \
cc_trn4_cbia.o \
cc_trn4_cdba.o \
cc_trn4_iabc.o \
cc_trn4_iajb.o \
cc_trn4_ijak.o \
cc_trn4_iqap.o \
cc_trn4_lkij.o \
cc_trn4_pqrs.o \
cc_trn_1emat.o \
cc_updatet.o \
cc_updatet_diis.o \
cc_write_info.o

objects_autogen = \
cc_cc2_t2_autogen.o \
cc_ccd_t2_autogen.o \
cc_ccs_t1_autogen.o \
cc_ccsd_t1_autogen.o \
cc_ccsd_t2_autogen.o \
cc_ccsd_t1_autogen_mod.o \
cc_ccsd_t2_autogen_mod.o \
cc_ccsdt_t1_autogen.o \
cc_ccsdt_t2_autogen.o \
cc_ccsdt_t3_autogen.o \
cc_ccsdtq_t1_autogen.o \
cc_ccsdtq_t2_autogen.o \
cc_ccsdtq_t3_autogen.o \
cc_ccsdtq_t4_autogen.o \
cc_contract_ao00io00at00it00j01b01v_autogen.o \
cc_contract_ao00io00at00it01j00b01v_autogen.o \
cc_contract_ao00io00at00it01j01b02v_autogen.o \
cc_contract_ao00io00at00it02j00b02v_autogen.o \
cc_contract_ao00io00at01it00j01b00v_autogen.o \
cc_contract_ao00io00at01it00j02b01v_autogen.o \
cc_contract_ao00io00at01it01j01b01s_autogen.o \
cc_contract_ao00io00at01it01j01b01v_autogen.o \
cc_contract_ao00io00at01it01j02b02s_autogen.o \
cc_contract_ao00io00at01it02j00b01v_autogen.o \
cc_contract_ao00io00at01it02j01b02v_autogen.o \
cc_contract_ao00io00at02it00j02b00v_autogen.o \
cc_contract_ao00io00at02it01j01b00v_autogen.o \
cc_contract_ao00io00at02it01j02b01v_autogen.o \
cc_contract_ao00io00at02it02j01b01s_autogen.o \
cc_contract_ao00io01at00it00j01b01v_autogen.o \
cc_contract_ao00io01at00it01j00b01v_autogen.o \
cc_contract_ao00io01at01it00j01b00s_autogen.o \
cc_contract_ao00io01at01it00j01b00v_autogen.o \
cc_contract_ao00io01at01it00j02b01s_autogen.o \
cc_contract_ao00io01at01it01j01b01v_autogen.o \
cc_contract_ao00io01at02it00j02b00v_autogen.o \
cc_contract_ao00io01at02it01j01b00s_autogen.o \
cc_contract_ao00io01at02it01j02b01s_autogen.o \
cc_contract_ao00io01at03it02j01b00s_autogen.o \
cc_contract_ao00io02at01it00j01b00v_autogen.o \
cc_contract_ao00io02at02it00j02b00s_autogen.o \
cc_contract_ao00io02at03it01j02b00s_autogen.o \
cc_contract_ao01io00at00it00j01b01v_autogen.o \
cc_contract_ao01io00at00it01j00b01s_autogen.o \
cc_contract_ao01io00at00it01j00b01v_autogen.o \
cc_contract_ao01io00at00it01j01b02s_autogen.o \
cc_contract_ao01io00at00it02j00b02v_autogen.o \
cc_contract_ao01io00at01it00j01b00v_autogen.o \
cc_contract_ao01io00at01it01j01b01v_autogen.o \
cc_contract_ao01io00at01it02j00b01s_autogen.o \
cc_contract_ao01io00at01it02j01b02s_autogen.o \
cc_contract_ao01io00at02it03j00b01s_autogen.o \
cc_contract_ao01io01at00it00j01b01s_autogen.o \
cc_contract_ao01io01at00it01j00b01v_autogen.o \
cc_contract_ao01io01at01it00j01b00v_autogen.o \
cc_contract_ao01io01at01it01j01b01s_autogen.o \
cc_contract_ao01io01at02it02j01b01s_autogen.o \
cc_contract_ao01io02at01it00j01b00s_autogen.o \
cc_contract_ao01io02at01it00j02b01s_autogen.o \
cc_contract_ao01io02at02it01j01b00s_autogen.o \
cc_contract_ao02io00at00it01j00b01v_autogen.o \
cc_contract_ao02io00at00it02j00b02s_autogen.o \
cc_contract_ao02io00at01it03j00b02s_autogen.o \
cc_contract_ao02io01at00it01j00b01s_autogen.o \
cc_contract_ao02io01at00it01j01b02s_autogen.o \
cc_contract_ao02io01at01it02j00b01s_autogen.o \
cc_contractgemm_ao00io00at00it00v_autogen.o \
cc_contractgemm_ao00io00at00it01v_autogen.o \
cc_contractgemm_ao00io00at00it02v_autogen.o \
cc_contractgemm_ao00io00at01it00v_autogen.o \
cc_contractgemm_ao00io00at01it01s_autogen.o \
cc_contractgemm_ao00io00at01it01v_autogen.o \
cc_contractgemm_ao00io00at01it02v_autogen.o \
cc_contractgemm_ao00io00at01it03v_autogen.o \
cc_contractgemm_ao00io00at02it00v_autogen.o \
cc_contractgemm_ao00io00at02it01v_autogen.o \
cc_contractgemm_ao00io00at02it02s_autogen.o \
cc_contractgemm_ao00io00at02it02v_autogen.o \
cc_contractgemm_ao00io00at02it03v_autogen.o \
cc_contractgemm_ao00io00at02it04v_autogen.o \
cc_contractgemm_ao00io00at03it02v_autogen.o \
cc_contractgemm_ao00io00at03it03s_autogen.o \
cc_contractgemm_ao00io00at03it04v_autogen.o \
cc_contractgemm_ao00io01at00it00v_autogen.o \
cc_contractgemm_ao00io01at00it01v_autogen.o \
cc_contractgemm_ao00io01at01it00s_autogen.o \
cc_contractgemm_ao00io01at01it00v_autogen.o \
cc_contractgemm_ao00io01at01it01v_autogen.o \
cc_contractgemm_ao00io01at02it00v_autogen.o \
cc_contractgemm_ao00io01at02it01s_autogen.o \
cc_contractgemm_ao00io01at02it02v_autogen.o \
cc_contractgemm_ao00io01at03it01v_autogen.o \
cc_contractgemm_ao00io01at03it02s_autogen.o \
cc_contractgemm_ao00io01at03it03v_autogen.o \
cc_contractgemm_ao00io01at04it03s_autogen.o \
cc_contractgemm_ao00io02at01it00v_autogen.o \
cc_contractgemm_ao00io02at02it00s_autogen.o \
cc_contractgemm_ao00io02at02it01v_autogen.o \
cc_contractgemm_ao00io02at03it01s_autogen.o \
cc_contractgemm_ao00io02at04it02s_autogen.o \
cc_contractgemm_ao01io00at00it00v_autogen.o \
cc_contractgemm_ao01io00at00it01s_autogen.o \
cc_contractgemm_ao01io00at00it01v_autogen.o \
cc_contractgemm_ao01io00at00it02v_autogen.o \
cc_contractgemm_ao01io00at01it00v_autogen.o \
cc_contractgemm_ao01io00at01it01v_autogen.o \
cc_contractgemm_ao01io00at01it02s_autogen.o \
cc_contractgemm_ao01io00at01it02v_autogen.o \
cc_contractgemm_ao01io00at01it03v_autogen.o \
cc_contractgemm_ao01io00at02it02v_autogen.o \
cc_contractgemm_ao01io00at02it03s_autogen.o \
cc_contractgemm_ao01io00at02it04v_autogen.o \
cc_contractgemm_ao01io00at03it04s_autogen.o \
cc_contractgemm_ao01io01at00it00s_autogen.o \
cc_contractgemm_ao01io01at00it01v_autogen.o \
cc_contractgemm_ao01io01at01it00v_autogen.o \
cc_contractgemm_ao01io01at01it01s_autogen.o \
cc_contractgemm_ao01io01at01it02v_autogen.o \
cc_contractgemm_ao01io01at02it02s_autogen.o \
cc_contractgemm_ao01io01at02it03v_autogen.o \
cc_contractgemm_ao01io01at03it03s_autogen.o \
cc_contractgemm_ao01io02at01it00s_autogen.o \
cc_contractgemm_ao01io02at01it01v_autogen.o \
cc_contractgemm_ao01io02at02it00v_autogen.o \
cc_contractgemm_ao01io02at02it01s_autogen.o \
cc_contractgemm_ao01io02at03it02s_autogen.o \
cc_contractgemm_ao01io03at01it00v_autogen.o \
cc_contractgemm_ao01io03at02it00s_autogen.o \
cc_contractgemm_ao01io03at03it01s_autogen.o \
cc_contractgemm_ao02io00at00it01v_autogen.o \
cc_contractgemm_ao02io00at00it02s_autogen.o \
cc_contractgemm_ao02io00at01it02v_autogen.o \
cc_contractgemm_ao02io00at01it03s_autogen.o \
cc_contractgemm_ao02io00at02it04s_autogen.o \
cc_contractgemm_ao02io01at00it01s_autogen.o \
cc_contractgemm_ao02io01at00it02v_autogen.o \
cc_contractgemm_ao02io01at01it02s_autogen.o \
cc_contractgemm_ao02io01at02it03s_autogen.o \
cc_contractgemm_ao02io02at01it02v_autogen.o \
cc_contractgemm_ao02io02at02it02s_autogen.o \
cc_contractgemm_ao02io03at01it00s_autogen.o \
cc_contractgemm_ao02io03at02it01s_autogen.o \
cc_contractgemm_ao02io04at01it00v_autogen.o \
cc_contractgemm_ao02io04at02it00s_autogen.o \
cc_contractgemm_ao03io02at01it02s_autogen.o \
cc_contractgemm_ao03io04at01it00s_autogen.o \
cc_rearrange_b00j01d00l02x_autogen.o \
cc_rearrange_b00j01d01l01n_autogen.o \
cc_rearrange_b00j01d01l01x_autogen.o \
cc_rearrange_b00j01d01l02x_autogen.o \
cc_rearrange_b00j01d02l01x_autogen.o \
cc_rearrange_b00j01d02l02n_autogen.o \
cc_rearrange_b00j02d01l02x_autogen.o \
cc_rearrange_b00j02d02l02n_autogen.o \
cc_rearrange_b01j00d01l01n_autogen.o \
cc_rearrange_b01j00d01l01x_autogen.o \
cc_rearrange_b01j00d01l02x_autogen.o \
cc_rearrange_b01j00d02l00x_autogen.o \
cc_rearrange_b01j00d02l01x_autogen.o \
cc_rearrange_b01j00d02l02n_autogen.o \
cc_rearrange_b01j01d01l02x_autogen.o \
cc_rearrange_b01j01d02l01x_autogen.o \
cc_rearrange_b01j01d02l02n_autogen.o \
cc_rearrange_b01j01d03l03n_autogen.o \
cc_rearrange_b01j02d02l02n_autogen.o \
cc_rearrange_b01j02d03l03n_autogen.o \
cc_rearrange_b01j03d03l03n_autogen.o \
cc_rearrange_b02j00d02l01x_autogen.o \
cc_rearrange_b02j00d02l02n_autogen.o \
cc_rearrange_b02j01d02l02n_autogen.o \
cc_rearrange_b02j01d03l03n_autogen.o \
cc_rearrange_b02j02d03l03n_autogen.o \
cc_rearrange_b02j02d04l04n_autogen.o \
cc_rearrange_b02j03d03l03n_autogen.o \
cc_rearrange_b02j03d04l04n_autogen.o \
cc_rearrange_b02j04d04l04n_autogen.o \
cc_rearrange_b03j01d03l03n_autogen.o \
cc_rearrange_b03j02d03l03n_autogen.o \
cc_rearrange_b03j02d04l04n_autogen.o \
cc_rearrange_b03j03d04l04n_autogen.o \
cc_rearrange_b03j04d04l04n_autogen.o \
cc_rearrange_b04j02d04l04n_autogen.o \
cc_rearrange_b04j03d04l04n_autogen.o

objects = $(objects_base) $(objects_autogen)

all :program mvbin

program : $(objects)
	$(LD) $(LDFLAGS) $(OMPLDFLAGS) $(NTQC_TOP)/src/cc_mod/*.o $(objects) ../util_lib/libutil.a \
	../cc_lib/*.o \
	$(LAPACKLIB) $(BLASLIB) $(ATLASLIB) \
	-o $(program)

mvbin :
	$(MV) $(program) $(BIN)/.

cleanautogen :
	$(RM) cc_*_autogen.f90

clean :
	$(RM) *.o *.mod $(BIN)/$(program)

veryclean :
	$(RM) *.o *.mod $(BIN)/$(program)

.SUFFIXES:.f90
.f90.o:
	$(F90C) $(F90FLAGS) $(OMPFLAGS) -I$(NTQC_TOP)/src/cc_mod -c $< 
