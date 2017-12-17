      PROGRAM CC_Main
!
      USE CC_Module, ONLY : NMax, CMO, CMOA, CMOB, Ene, CalcMP, DoTran, IPrint
!
!     o Main driver for coupled-cluster calculations
!
      IMPLICIT NONE
!
      CHARACTER(LEN=15), PARAMETER :: ExeName = "CC"
      REAL(8):: CTim1, CTim2
      REAL(8) :: TimeBgnExe
!
      CALL Util_Header(ExeName, TimeBgnExe)
!
!     o Read Input
!
      CALL CC_Read_Input
!
!     o Set up variables in CC_Module
!
      CALL CC_InitModule
      WRITE(*, '(/, " ... Cluster operator involves up to ", I3, "-body excitations"/)') NMax
!
      CALL CC_ArcWgt_Allocate
!
!     o Generate MO integrals
!
      IF (DoTran) THEN
         CALL CC_ConvSCF_Allocate
         CALL Util_Enter("CC_Trans", 1, IPrint)
         CALL CPU_TIME(CTim1)
         CALL CC_Trans
         CALL CPU_TIME(CTim2)
         CALL Util_CPUTime("CC_Trans", CTim2 -  CTim1, 1, IPrint)
         CALL CC_ConvSCF_Deallocate
      ELSE
         PRINT '(" ...   Skip     (CC_Trans        )")'
      END IF
!
!     o Moller-Plesset energy
!
      IF (CalcMP) THEN
!Debug   CALL CC_CheckMOint   ! For debugging MO integrals
         CALL Util_Enter("CC_MP", 1, IPrint)
         CALL CPU_TIME(CTim1)
         CALL CC_MP
         CALL CPU_TIME(CTim2)
         CALL Util_CPUTime("CC_MP", CTim2 - CTim1, 1, IPrint)
      END IF
!
!     o CC amplitude determination
!
      CALL Util_Enter("CC_Exe", 1, IPrint)
      CALL CPU_TIME(CTim1)
      CALL CC_Exe
      CALL CPU_TIME(CTim2)
      CALL Util_CPUTime("CC_Exe", CTim2 - CTim1, 1, IPrint)
!
!     o Save CC information to file
!
      CALL CC_Write_Info
!
      CALL CC_ArcWgt_Deallocate
!
!     o Free MO coef. and ene. arrays
!
      DEALLOCATE(CMOA)
      DEALLOCATE(CMOB)
      DEALLOCATE(CMO)
      DEALLOCATE(Ene)
!
      CALL Util_Footer(ExeName, TimeBgnExe)
!
      END PROGRAM
