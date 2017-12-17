      SUBROUTINE CC_Trn3_PQRs(G1, G2)
!
      USE CC_Module, ONLY : NBF, CMO, NMO, NOcA, NVrA, NOc
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 3rd virtual transformation (PR|Qs)
!                                    ^
!       Integrals are stored as G1(s,R,Q,P)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NBF,NBF,NMO,*)
      REAL(8), INTENT(OUT) :: G1(NBF,NMO,NMO,*)
!
      INTEGER :: LMO, IMO, KMO, LSpin, KSpin
!
      CALL CCLib_DClear(G1, NBF*NMO*NMO*NMO)
!
      DO LMO = 1, NMO
         DO IMO = 1, NMO
            CALL DGEMM('N', 'N', NBF, NMO, NBF, One, G2(1,1,IMO,LMO), NBF, &
     &         CMO, NBF, Zero, G1(1,1,IMO,LMO), NBF)
         END DO
      END DO
!
      DO LMO = 1, NMO
         IF ((LMO <= NOcA) .OR. ((LMO > NOc) .AND. (LMO <= (NOc + NVrA)))) THEN
            LSpin = 1
         ELSE
            LSpin = -1
         END IF
         DO IMO = 1, NMO
            DO KMO = 1, NMO
               IF ((KMO <= NOcA) .OR. ((KMO > NOc) .AND. (KMO <= (NOc + NVrA)))) THEN
                  KSpin = 1
               ELSE
                  KSpin = -1
               END IF
               IF (LSpin /= KSpin) THEN
                  CALL CCLib_DClear(G1(1,KMO,IMO,LMO), NBF)
               END IF
            END DO
         END DO
      END DO
!
      END SUBROUTINE
