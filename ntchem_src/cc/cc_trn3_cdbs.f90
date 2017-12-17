      SUBROUTINE CC_Trn3_CDBs(G1, G2)
!
      USE CC_Module, ONLY : NVr, NBF, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (CD|Bs)
!                                  ^
!       Integrals are stored as G1(s,D,BC) (B < C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NVr,*)
!
      INTEGER :: LVrA, IVrA, LIVrAA, LVrB, LIVrBA, IVrB, LIVrBB, LOff
!
      CALL CCLib_DClear(G1, NBF*NVr*(NVr*(NVr-1))/2)
!
      DO LVrA = 2, NVrA
         LOff = ((LVrA - 1) * (LVrA - 2)) / 2
         DO IVrA = 1, LVrA - 1
            LIVrAA = LOff + IVrA
            CALL DGEMM('N', 'N', NBF, NVrA, NBF, One, G2(1,1,LIVrAA), NBF, &
     &         CMOA(1,NOcA+1), NBF, Zero, G1(1,1,LIVrAA), NBF)
         END DO   ! IVrA
      END DO   ! LVrA
!
      DO LVrB = 1, NVrB
         LOff = ((NVrA + LVrB - 1) * (NVrA + LVrB - 2)) / 2
         DO IVrA = 1, NVrA
            LIVrBA = LOff + IVrA
            CALL DGEMM('N', 'N', NBF, NVrB, NBF, One, G2(1,1,LIVrBA), NBF, &
     &         CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,LIVrBA), NBF)
         END DO
         DO IVrB = 1, LVrB - 1
            LIVrBB = LOff + (NVrA + IVrB)
            CALL DGEMM('N', 'N', NBF, NVrB, NBF, One, G2(1,1,LIVrBB), NBF, &
     &         CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,LIVrBB), NBF)
         END DO
      END DO
!
      END SUBROUTINE
