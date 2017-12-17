      SUBROUTINE CC_Trn4_IABC(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NBF, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th virtual transformation (IA|BC)
!                                    ^
!       Integrals are stored as G2(A,C*B,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NVr*NVr,*)
      REAL(8), INTENT(OUT) :: G2(NVr,NVr*NVr,*)
!
      INTEGER :: IOcA, IOcB, IOcBX
!
      CALL CCLib_DClear(G2, NVr*NVr*NVr*NOc)
!
      DO IOcA = 1, NOcA
         CALL DGEMM('T', 'N', NVrA, (NVr*NVr), NBF, One, CMOA(1,NOcA+1), NBF, &
     &      G1(1,1,IOcA), NBF, Zero, G2(1,1,IOcA), NVr)
      END DO
!
      DO IOcB = 1, NOcB
         IOcBX = NOcA + IOcB
         CALL DGEMM('T', 'N', NVrB, (NVr*NVr), NBF, One, CMOB(1,NOcB+1), NBF, &
     &      G1(1,1,IOcBX), NBF, Zero, G2(NVrA+1,1,IOcBX), NVr)
      END DO
!
      END SUBROUTINE
