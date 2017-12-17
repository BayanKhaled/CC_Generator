      SUBROUTINE CC_Trn3_IqBC(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NBF, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (Iq|BC)
!                                       ^
!       Integrals are stored as G1(q,C,B,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,NVr,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NVr,NVr,*)
!
      INTEGER :: IOc, JVrA, JVrB, JVrBX
!
      CALL CCLib_DClear(G1, NVr*NBF*NVr*NOc)
!
      DO IOc = 1, NOc
         DO JVrA = 1, NVrA
            CALL DGEMM('T', 'N', NBF, NVrA, NBF, One, G2(1,1,JVrA,IOc), NBF, &
     &         CMOA(1,NOcA+1), NBF, Zero, G1(1,1,JVrA,IOc), NBF)
         END DO
         DO JVrB = 1, NVrB
            JVrBX = NVrA + JVrB
            CALL DGEMM('T', 'N', NBF, NVrB, NBF, One, G2(1,1,JVrBX,IOc), NBF, &
     &         CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,JVrBX,IOc), NBF)
         END DO
      END DO
!
      END SUBROUTINE
