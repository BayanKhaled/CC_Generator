      SUBROUTINE CC_Trn3_IAJs(G1, G2)
!
      USE CC_Module, ONLY : CMO, CMOA, CMOB, NOc, NOcA, NOcB, NVrA, NVrB, NVr, NBF
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (IA|Js)
!                                    ^
!       Integrals are stored as G1(s,A,JI) (J < I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NVr,*)
!
      INTEGER :: IOcA, IOff, JOcA, IJOcAA, IOcB, IJOcBA, JOcB, JOcBX, IJOcBB
!
      CALL CCLib_DClear(G1, NBF*NVr*(NOc*(NOc-1))/2)
!
      DO IOcA = 2, NOcA
         IOff = ((IOcA - 1) * (IOcA - 2)) / 2
         DO JOcA = 1, IOcA - 1
            IJOcAA = IOff + JOcA
            CALL DGEMM('N', 'N', NBF, NVrA, NBF, One, G2(1,1,IJOcAA), NBF, &
     &         CMOA(1,NOcA+1), NBF, Zero, G1(1,1,IJOcAA), NBF)
         END DO
      END DO
!
      DO IOcB = 1, NOcB
         IOff = ((NOcA + IOcB - 1) * (NOcA + IOcB - 2)) / 2
         DO JOcA = 1, NOcA
            IJOcBA = IOff + JOcA
            CALL DGEMM('N', 'N', NBF, NVrB, NBF, One,G2(1,1,IJOcBA), NBF, &
     &         CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,IJOcBA), NBF)
         END DO
         DO JOcB = 1, IOcB - 1
            JOcBX = NOcA + JOcB
            IJOcBB = IOff + JOcBX
            CALL DGEMM('N', 'N', NBF, NVrB, NBF, One, G2(1,1,IJOcBB), NBF, &
     &         CMOB(1,NOcB+1), NBF, Zero, G1(1,NVrA+1,IJOcBB), NBF)
         END DO
      END DO
!
      END SUBROUTINE
