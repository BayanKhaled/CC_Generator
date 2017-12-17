      SUBROUTINE CC_Trn4_CDBA(G1, G2)
!
      USE CC_Module, ONLY : NVr, NBF, CMOA, CMOB, NVrA, NVrB, NVr, NOcA, NOcB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th virtual transformation (CD|BA)
!                                       ^
!       Integrals are stored as G2(A,D,BC) (B < C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NVr,*)
      REAL(8), INTENT(OUT) :: G2(NVr,NVr,*)
!
      INTEGER :: LVrA, IVrA, LIVrAA, LVrB, LIVrBA, IVrB, LIVrBB, LOff
!
      CALL CCLib_DClear(G2, NVr*NVr*(NVr*(NVr-1))/2)
!
      DO LVrA = 1, NVrA
         LOff = ((LVrA - 1) * (LVrA - 2)) / 2
         DO IVrA = 1, LVrA - 1
            LIVrAA = LOff + IVrA
            CALL DGEMM('T', 'N', NVrA, NVrA, NBF, One, CMOA(1,NOcA+1), NBF, &
     &         G1(1,1,LIVrAA), NBF, Zero, G2(1,1,LIVrAA), NVr)
         END DO
      END DO
!
      DO LVrB = 1, NVrB
         LOff = ((NVrA + LVrB - 1) * (NVrA + LVrB - 2)) / 2
         DO IVrA = 1, NVrA
            LIVrBA = LOff + IVrA
            CALL DGEMM('T', 'N', NVrA, NVrB, NBF, One, CMOA(1,NOcA+1), NBF, &
     &         G1(1,NVrA+1,LIVrBA), NBF, Zero, G2(1,NVrA+1,LIVrBA), NVr)
         END DO
         DO IVrB = 1, LVrB - 1
            LIVrBB = LOff + (NVrA + IVrB)
            CALL DGEMM('T', 'N', NVrB, NVrB, NBF, One, CMOB(1,NOcB+1), NBF, &
     &         G1(1,NVrA+1,LIVrBB), NBF, Zero, G2(NVrA+1,NVrA+1,LIVrBB), NVr)
         END DO
      END DO
!
      END SUBROUTINE
