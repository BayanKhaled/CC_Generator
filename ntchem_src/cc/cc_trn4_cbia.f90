      SUBROUTINE CC_Trn4_CBIA(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr, NBF, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th virtual transformation (CB|IA)
!                                    ^
!       Integrals are stored as G2(B,A*I,C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NVr*NOc,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NVr,NVr*NOc,*)
!
      INTEGER :: IVrA, IVrB, IVrBX
!
      CALL CCLib_DClear(G2, NOc*NVr*NVr*NVr)
!
      DO IVrA = 1, NVrA
         CALL DGEMM('T', 'T', NVrA, (NVr*NOc), NBF, One, CMOA(1,NOcA+1), NBF, &
     &      G1(1,1,IVrA), (NOc*NVr), Zero, G2(1,1,IVrA), NVr) 
      END DO
!
      DO IVrB = 1, NVrB
         IVrBX = NVrA + IVrB
         CALL DGEMM('T', 'T', NVrB, (NVr*NOc), NBF, One, CMOB(1,NOcB+1), NBF, &
     &      G1(1,1,IVrBX), (NOc*NVr), Zero, G2(NVrA+1,1,IVrBX), NVr) 
      END DO
!
      END SUBROUTINE
