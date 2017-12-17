      SUBROUTINE CC_CalcResidueD(NRank)
!
      USE CC_Module, ONLY : NoT1, CC2, IPrint
!
      IMPLICIT NONE
!
!     o Calculate T_NRank residues for CCSD-derived wavefunctions
!
      INTEGER, INTENT(IN) :: NRank
!
      REAL(8) :: CTim1, CTim2
!
      CALL CPU_TIME(CTim1)
!
      SELECT CASE (NRank)
         CASE (1)
            IF (NoT1) RETURN
            CALL CC_CCSD_T1_AutoGen_Mod
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T1 Residue", CTim2 - CTim1, 2, IPrint)
         CASE (2)
            IF (NoT1) THEN
               CALL CC_CCD_T2_AutoGen
            ELSE IF (CC2) THEN
               CALL CC_CC2_T2_AutoGen
            ELSE
               CALL CC_CCSD_T2_AutoGen_Mod
            END IF
            CALL CC_CopyResidue("W", NRank)
            CALL CPU_TIME(CTim2)
            CALL Util_CPUTime("T2 Residue", CTim2 - CTim1, 2, IPrint)
         CASE DEFAULT
            STOP 'Error: CheckNRank'
      END SELECT
!
      END SUBROUTINE
