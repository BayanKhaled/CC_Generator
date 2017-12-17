      SUBROUTINE CC_Trn4_IQAP(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NMO, CMOA, CMOB, NMOA, NMOB, NVrA, NVrB, NOcA, NOcB, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th general transformation (IQ|AP)
!                                       ^
!       Integrals are stored as G2(P,Q,A,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NMO,NVr,*)
      REAL(8), INTENT(OUT) :: G2(NMO,NMO,NVr,*)
!
      INTEGER :: IOcA, IOcB, IOcBX, IVrA, IVrB, IVrBX
!
      CALL CCLib_DClear(G2, NMO*NMO*NVr*NOc)
!
      DO IOcA = 1, NOcA
         DO IVrA = 1, NVrA
            CALL DGEMM('T', 'N', NMOA, NMOA, NBF, One, CMOA, NBF, &
     &         G1(1,1,IVrA,IOcA), NBF, Zero, G2(1,1,IVrA,IOcA), NMO)
         END DO
         DO IVrB = 1, NVrB
            IVrBX = NVrA + IVrB
            CALL DGEMM('T', 'N', NMOB, NMOA, NBF, One, CMOB, NBF, &
     &         G1(1,1,IVrBX,IOcA), NBF, Zero, G2(NMOA+1,1,IVrBX,IOcA), NMO)
         END DO
      END DO
!
      DO IOcB = 1, NOcB
         IOcBX = NOcA + IOcB
         DO IVrA = 1, NVrA
            CALL DGEMM('T', 'N', NMOA, NMOB, NBF, One, CMOA, NBF, &
     &         G1(1,NMOA+1,IVrA,IOcBX), NBF, Zero, G2(1,NMOA+1,IVrA,IOcBX), NMO)
         END DO
         DO IVrB = 1, NVrB
            IVrBX = NVrA + IVrB
            CALL DGEMM('T', 'N', NMOB, NMOB, NBF, One, CMOB, NBF, &
     &         G1(1,NMOA+1,IVrBX,IOcBX), NBF, Zero, G2(NMOA+1,NMOA+1,IVrBX,IOcBX), NMO)
         END DO
      END DO
!
      END SUBROUTINE
