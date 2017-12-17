      SUBROUTINE CC_Trn3_CqIA(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr, NBF, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (Cq|IA)
!                                     ^
!       Integrals are stored as G1(A,I,q*C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NOc,NBF,*)
      REAL(8), INTENT(OUT) :: G1(NVr,NOc,*)
!
      INTEGER :: IVrq
!
      CALL CCLib_DClear(G1, NOc*NBF*NVr*NVr)
!
      DO IVrq = 1, NVr*NBF
         CALL DGEMM('T', 'T', NVrA, NOcA, NBF, One, CMOA(1,NOcA+1), NBF, &
     &      G2(1,1,IVrq), NOc, Zero, G1(1,1,IVrq), NVr)
         CALL DGEMM('T', 'T', NVrB, NOcB, NBF, One, CMOB(1,NOcB+1), NBF, &
     &      G2(NOcA+1,1,IVrq), NOc, Zero, G1(NVrA+1,NOcA+1,IVrq), NVr)
      END DO
!
      END SUBROUTINE
