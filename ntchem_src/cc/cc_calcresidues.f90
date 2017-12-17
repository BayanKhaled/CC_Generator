      SUBROUTINE CC_CalcResidueS
!
      USE CC_Module, ONLY : IPrint
!
      IMPLICIT NONE
!
!     o Calculate T1 residues for CCS wavefunction
!
      REAL(8) :: CTim1, CTim2
!
      CALL CPU_TIME(CTim1)
      CALL CC_CCS_T1_AutoGen
      CALL CC_CopyResidue("W", 1)
      CALL CPU_TIME(CTim2)
      CALL Util_CPUTime("T1 Residue", CTim2 - CTim1, 2, IPrint)
!
      END SUBROUTINE
