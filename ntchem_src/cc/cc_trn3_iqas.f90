      SUBROUTINE CC_Trn3_IQAs(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NMO, CMOA, CMOB, NMOA, NMOB, NVrA, NVrB, NOcA, NOcB, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd general transformation (IQ|As)
!                                    ^
!       Integrals are stored as G1(s,Q,A,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,NVr,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NMO,NVr,*)
!
      INTEGER :: IOcA, IOcB, IOcBX, IVrA, IVrB, IVrBX
!
      CALL CCLib_DClear(G1, NMO*NBF*NVr*NOc)
!
      DO IOcA = 1, NOcA
         DO IVrA = 1, NVrA
            CALL DGEMM('N', 'N', NBF, NMOA, NBF, One, G2(1,1,IVrA,IOcA), NBF, &
     &         CMOA, NBF, Zero, G1(1,1,IVrA,IOcA), NBF)
         END DO
         DO IVrB = 1, NVrB
            IVrBX = NVrA + IVrB
            CALL DGEMM('N', 'N', NBF, NMOA, NBF, One, G2(1,1,IVrBX,IOcA), NBF, &
     &         CMOA, NBF, Zero, G1(1,1,IVrBX,IOcA), NBF)
         END DO
      END DO
!
      DO IOcB = 1, NOcB
         IOcBX = NOcA + IOcB
         DO IVrA = 1, NVrA
            CALL DGEMM('N', 'N', NBF, NMOB, NBF, One, G2(1,1,IVrA,IOcBX), NBF, &
     &         CMOB, NBF, Zero, G1(1,NMOA+1,IVrA,IOcBX), NBF)
         END DO
         DO IVrB = 1, NVrB
            IVrBX = NVrA + IVrB
            CALL DGEMM('N', 'N', NBF, NMOB, NBF, One, G2(1,1,IVrBX,IOcBX), NBF, &
     &         CMOB, NBF, Zero, G1(1,NMOA+1,IVrBX,IOcBX), NBF)
         END DO
      END DO
!
      END SUBROUTINE
