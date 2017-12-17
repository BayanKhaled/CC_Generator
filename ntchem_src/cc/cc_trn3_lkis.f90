      SUBROUTINE CC_Trn3_LKIs(G1, G2)
!
      USE CC_Module, ONLY : NOc, NBF, CMOA, CMOB, NOcA, NOcB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd Occupied transformation (LK|Is)
!                                     ^
!       Integrals are stored as (s,K,IL) (I < L)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(NBF,NOc,*)
      REAL(8), INTENT(IN) :: G2(NBF,NBF,*)
!
      INTEGER :: LOcA, IOcA, LIOcAA, LOcB, LIOcBA, IOcB, LIOcBB, LOff
!
      CALL CCLib_DClear(G1, NBF*NOc*(NOc*(NOc-1))/2)
!
      DO LOcA = 2, NOcA
         LOff = ((LOcA - 1) * (LOcA - 2)) / 2
         DO IOcA = 1, LOcA - 1
            LIOcAA = LOff + IOcA
            CALL DGEMM('N', 'N', NBF, NOcA, NBF, One, G2(1,1,LIOcAA), NBF, &
     &         CMOA, NBF, Zero, G1(1,1,LIOcAA), NBF)
         END DO
      END DO
!
      DO LOcB = 1, NOcB
         LOff = ((NOcA + LOcB - 1) * (NOcA + LOcB - 2)) / 2
         DO IOcA = 1, NOcA
            LIOcBA = LOff + IOcA
            CALL DGEMM('N', 'N', NBF, NOcB, NBF, One, G2(1,1,LIOcBA), NBF, &
     &         CMOB, NBF, Zero, G1(1,NOcA+1,LIOcBA), NBF)
         END DO
         DO IOcB = 1, LOcB - 1
            LIOcBB = LOff + (NOcA + IOcB)
            CALL DGEMM('N', 'N', NBF, NOcB, NBF, One, G2(1,1,LIOcBB), NBF, &
     &         CMOB, NBF, Zero, G1(1,NOcA+1,LIOcBB), NBF)
         END DO
      END DO
!
      END SUBROUTINE
