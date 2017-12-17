      SUBROUTINE CC_Trn4_PQRS(G1, G2)
!
      USE CC_Module, ONLY : NMO, NBF, CMO, NOc, NOcA, NOcB, NVrA
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o 4th virtual transformation (PQ|RS)
!                                       ^
!       Integrals are stored as G2(S,Q,R,P)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NBF,NMO,NMO,*)
      REAL(8), INTENT(OUT) :: G2(NMO,NMO,NMO,*)
!
      INTEGER :: LMO, IMO, JMO, KMO
      INTEGER :: ISpin, JSpin, KSpin, LSpin
!
      CALL CCLib_DClear(G2, NMO*NMO*NMO*NMO)
!
      DO LMO = 1, NMO
         DO IMO = 1, NMO
            CALL DGEMM('T', 'N', NMO, NMO, NBF, One, CMO, NBF, &
     &         G1(1,1,IMO,LMO), NBF, Zero, G2(1,1,IMO,LMO), NMO)
         END DO
      END DO
!
!     o Spin integration
!
      DO LMO = 1, NMO
         IF ((LMO <= NOcA) .OR. ((LMO > NOc) .AND. (LMO <= (NOc + NVrA)))) THEN
            LSpin = 1
         ELSE
            LSpin = -1
         END IF
         DO JMO = 1, NMO
            IF ((JMO <= NOcA) .OR. ((JMO > NOc) .AND. (JMO <= (NOc + NVrA)))) THEN
               JSpin = 1
            ELSE
               JSpin = -1
            END IF
            DO IMO = 1, NMO
               IF ((IMO <= NOcA) .OR. ((IMO > NOc) .AND. (IMO <= (NOc + NVrA)))) THEN
                  ISpin = 1
               ELSE
                  ISpin = -1
               END IF
               DO KMO = 1, NMO
                  IF ((KMO <= NOcA) .OR. ((KMO > NOc) .AND. (KMO <= (NOc + NVrA)))) THEN
                     KSpin = 1
                  ELSE
                     KSpin = -1
                  END IF
!
                  IF ((LSpin /= JSpin) .OR. (KSpin /= ISpin)) THEN
                     G2(KMO,JMO,IMO,LMO) = Zero
                  END IF
!
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
