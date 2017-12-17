      SUBROUTINE CC_Trn4_IAJB(G1, G2)
!
      USE CC_Module, ONLY : CMO, CMOA, CMOB, NOc, NOcA, NOcB, NVrA, NVrB, NVr, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th virtual transformation (IA|JB)
!                                       ^
!       Integrals are stored as G2(B,A,JI) (J < I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NVr,*)
      REAL(8), INTENT(OUT) :: G2(NVr,NVr,*)
!
      INTEGER :: IOcA, IOff, JOcA, IJOcAA, IOcB, IOcBX, IJOcBA, JOcB, JOcBX, IJOcBB
!
      CALL CCLib_DClear(G2, NVr*NVr*(NOc*(NOc-1))/2)
!
      DO IOcA = 1, NOcA
         IOff = ((IOcA - 1) * (IOcA - 2)) / 2
         DO JOcA = 1, IOcA - 1
            IJOcAA = IOff + JOcA
            CALL DGEMM('T', 'N', NVrA, NVrA, NBF, One, CMOA(1,NOcA+1), NBF, &
     &         G1(1,1,IJOcAA), NBF, Zero, G2(1,1,IJOcAA), NVr)
         END DO 
      END DO
!
      DO IOcB = 1, NOcB
         IOcBX = NOcA + IOcB
         IOff = ((IOcBX - 1) * (IOcBX - 2)) / 2
         DO JOcA = 1, NOcA
            IJOcBA = IOff + JOcA
            CALL DGEMM('T', 'N', NVrA, NVrB, NBF, One, CMOA(1,NOcA+1), NBF, &
     &         G1(1,NVrA+1,IJOcBA), NBF, Zero, G2(1,NVrA+1,IJOcBA), NVr)
         END DO
         DO JOcB = 1, IOcB - 1
            JOcBX = NOcA + JOcB
            IJOcBB = IOff + JOcBX
            CALL DGEMM('T', 'N', NVrB, NVrB, NBF, One, CMOB(1,NOcB+1), NBF, &
     &         G1(1,NVrA+1,IJOcBB), NBF, Zero, G2(NVrA+1,NVrA+1,IJOcBB), NVr)
         END DO
      END DO
!
      END SUBROUTINE
