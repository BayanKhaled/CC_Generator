      SUBROUTINE CC_Trn4_IJAK(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NBF, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th Occupied transformation (IJ|AK)
!                                     ^
!       Integrals are stored as G2(J,K*A,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NOc*NVr,*)
      REAL(8), INTENT(OUT) :: G2(NOc,NOc*NVr,*)
!
      INTEGER :: IOcA, IOcB, IOcBX
!
      CALL CCLib_DClear(G2, NVr*NOc*NOc*NOc)
!
      DO IOcA = 1, NOcA
         CALL DGEMM('T', 'N', NOcA, (NOc*NVr), NBF, One, CMOA, NBF, &
     &      G1(1,1,IOcA), NBF, Zero, G2(1,1,IOcA), NOc)
      END DO
!
      DO IOcB = 1, NOcB
         IOcBX = NOcA + IOcB
         CALL DGEMM('T', 'N', NOcB, (NOc*NVr), NBF, One, CMOB, NBF, &
     &      G1(1,1,IOcBX), NBF, Zero, G2(NOcA+1,1,IOcBX), NOc)
      END DO
!
      END SUBROUTINE
