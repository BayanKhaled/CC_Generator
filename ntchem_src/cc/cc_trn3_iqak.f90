      SUBROUTINE CC_Trn3_IqAK(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr, NBF, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (Iq|AK)
!                                       ^
!       Integrals are stored as G1(q,K,A,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,NVr,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NOc,NVr,*)
!
      INTEGER :: IOc, IVrA, IVrB, IVrBX
!
      CALL CCLib_DClear(G1, NBF*NOc*NVr*NOc)
!
      DO IOc = 1, NOc
         DO IVrA = 1, NVrA
            CALL DGEMM('T', 'N', NBF, NOcA, NBF, One, G2(1,1,IVrA,IOc), NBF, &
     &         CMOA, NBF, Zero, G1(1,1,IVrA,IOc), NBF)
         END DO
         DO IVrB = 1, NVrB
            IVrBX = NVrA + IVrB
            CALL DGEMM('T', 'N', NBF, NOcB, NBF, One, G2(1,1,IVrBX,IOc), NBF, &
     &         CMOB, NBF, Zero, G1(1,NOcA+1,IVrBX,IOc), NBF)
         END DO
      END DO
!
      END SUBROUTINE
