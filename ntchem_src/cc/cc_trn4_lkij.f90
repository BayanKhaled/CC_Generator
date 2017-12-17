      SUBROUTINE CC_Trn4_LKIJ(G1, G2)
!
      USE CC_Module, ONLY : NOc, NBF, CMOA, CMOB, NOcA, NOcB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th Occupied transformation (LK|IJ)
!                                        ^
!       Integrals are stored as G2(J,K,IL) (I < L)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NOc,*)
      REAL(8), INTENT(OUT) :: G2(NOc,NOc,*)
!
      INTEGER :: LOcA, IOcA, LIOcAA, LOcB, IOcB, LIOcBB, LIOcBA, LOff
!
      CALL CCLib_DClear(G2, NOc*NOc*(NOc*(NOc-1))/2)
!
      DO LOcA = 1, NOcA
         LOff = ((LOcA - 1) * (LOcA - 2)) / 2
         DO IOcA = 1, LOcA - 1
            LIOcAA = LOff + IOcA
            CALL DGEMM('T', 'N', NOcA, NOcA, NBF, One, CMOA, NBF, &
     &         G1(1,1,LIOcAA), NBF, Zero, G2(1,1,LIOcAA), NOc)
         END DO
      END DO
!
      DO LOcB = 1, NOcB
         LOff = ((NOcA + LOcB - 1) * (NOcA + LOcB - 2)) / 2
         DO IOcA = 1, NOcA
            LIOcBA = LOff + IOcA
            CALL DGEMM('T', 'N', NOcA, NOcB, NBF, One, CMOA, NBF, &
     &         G1(1,NOcA+1,LIOcBA), NBF, Zero, G2(1,NOcA+1,LIOcBA), NOc)
         END DO
         DO IOcB = 1, LOcB - 1
            LIOcBB = LOff + (NOcA + IOcB)
            CALL DGEMM('T', 'N', NOcB, NOcB, NBF, One, CMOB, NBF, &
     &         G1(1,NOcA+1,LIOcBB), NBF, Zero, G2(NOcA+1,NOcA+1,LIOcBB), NOc)
         END DO
      END DO
!
      END SUBROUTINE
