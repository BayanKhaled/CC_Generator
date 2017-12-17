      SUBROUTINE CC_Trn3_CBIs(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr, NBF, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (CB|Is)
!                                    ^
!       Integrals are stored as G1(I*s,B,C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NOc*NBF,NBF,*)
      REAL(8), INTENT(OUT) :: G1(NOc*NBF,NVr,*)
!
      INTEGER :: IVrA, IVrB
!
      CALL CCLib_DClear(G1, NOc*NBF*NVr*NVr)
!
      DO IVrA = 1, NVrA
         CALL DGEMM('N', 'N', (NOc*NBF), NVrA, NBF, One, G2(1,1,IVrA), (NOc*NBF), &
     &      CMOA(1,NOcA+1), NBF, Zero, G1(1,1,IVrA), (NOc*NBF))
         CALL DGEMM('N', 'N', (NOc*NBF), NVrB, NBF, One, G2(1,1,IVrA), (NOc*NBF), &
     &      CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,IVrA), (NOc*NBF))
      END DO
!
      DO IVrB = 1, NVrB
         CALL DGEMM('N', 'N', (NOc*NBF), NVrA, NBF, One, G2(1,1,NVrA+IVrB), (NOc*NBF), &
     &      CMOA(1,NOcA+1), NBF, Zero, G1(1,1,NVrA+IVrB), (NOc*NBF))
         CALL DGEMM('N', 'N', (NOc*NBF), NVrB, NBF, One, G2(1,1,NVrA+IVrB), (NOc*NBF), &
     &      CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,NVrA+IVrB), (NOc*NBF))
      END DO
!
      END SUBROUTINE
