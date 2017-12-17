      SUBROUTINE CC_CalcResidueQ(NRank)
!
      USE CC_Module, ONLY : IPrint
!
      IMPLICIT NONE
!
!     o Calculate T_NRank residues for CCSDTQ wavefunction
!
      INTEGER, INTENT(IN) :: NRank
!
      REAL(8) :: CTim1, CTim2
!
      CALL CPU_TIME(CTim1)
!
      SELECT CASE (NRank)
         CASE (1)
            CALL CC_CCSDTQ_T1_AutoGen
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T1 Residue", CTim2 - CTim1, 2, IPrint)
         CASE (2)
            CALL CC_CCSDTQ_T2_AutoGen
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T2 Residue", CTim2 - CTim1, 2, IPrint)
         CASE (3)
            CALL CC_CCSDTQ_T3_AutoGen
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T3 Residue", CTim2 - CTim1, 2, IPrint)
         CASE (4)
            CALL CC_CCSDTQ_T4_AutoGen
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T4 Residue", CTim2 - CTim1, 2, IPrint)
         CASE DEFAULT
            STOP 'Error: Check NRank'
      END SELECT
!
      END SUBROUTINE
