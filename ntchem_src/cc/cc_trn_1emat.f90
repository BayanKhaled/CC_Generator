      SUBROUTINE CC_Trn_1eMat(G1, G2)
!
      USE CC_Module, ONLY : NBF, CMO, NMO, NOcA, NOcB, NVrA, NOc
      USE CC_Constant_Module, ONLY : Zero, One
!
!     Transform a 1-e matrix
!     Integrals are stored as G1(Q,P)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(*)
      REAL(8), INTENT(OUT) :: G1(NMO,NMO)
!
      REAL(8), PARAMETER :: Small = 1.0D-12
      INTEGER :: Ip, Iq, Ipq, ISpinP, ISpinQ, IpX, IqX
      INTEGER :: NCoreA
      REAL(8) :: Wrk1(NBF,NBF), Wrk2(NMO,NMO), Dum
!
!     o Form full AO matrix
!
      Ipq = 0
      DO Ip = 1, NBF
         DO Iq = 1, Ip
            Ipq = Ipq + 1
            Wrk1(Iq,Ip) = G2(Ipq)
            Wrk1(Ip,Iq) = G2(Ipq)
         END DO
      END DO
!
!     o 1st transformation
!
      CALL DGEMM('T', 'N', NMO, NBF, NBF, One, CMO, NBF, Wrk1, NBF, Zero, Wrk2, NMO)
!
!     o 2nd transformation
!
      CALL DGEMM('N', 'N', NMO, NMO, NBF, One, Wrk2, NMO, CMO, NBF, Zero, G1, NMO)
!
!     o Eliminate alpha-beta and beta-alpha blOcks
!
      DO Ip = 1, NMO
         IF ((Ip <= NOcA) .OR. ((Ip > NOc) .AND. (Ip <= (NOc + NVrA)))) THEN
            ISpinP = 1
         ELSE
            ISpinP = -1
         END IF
         DO Iq = 1, NMO
            IF ((Iq <= NOcA) .OR. ((Iq > NOc) .AND. (Iq <= (NOc + NVrA)))) THEN
               ISpinQ = 1
            ELSE
               ISpinQ = -1
            END IF
            IF (ISpinP /= ISpinQ) THEN
               G1(Iq,Ip) = Zero
               G1(Ip,Iq) = Zero
            END IF
         END DO
      END DO
!
      NCoreA = 0   ! # of alpha core orbitals
      DO Ip = 1, NMO
         IF (Ip <= NCoreA) CYCLE
         IF ((Ip <= NOcA) .OR. ((Ip > NOc) .AND. (Ip <= (NOc + NVrA)))) THEN
            IpX = Ip - NOcB - NCoreA
            IF (IpX <= 0) IpX = Ip - NCoreA
            DO Iq = 1, Ip
               IF (Iq <= NCoreA) CYCLE
               IF ((Iq <= NOcA) .OR. ((Iq > NOc) .AND. (Iq <= (NOc + NVrA)))) THEN
                  IqX = Iq - NOcB - NCoreA
                  IF (IqX <= 0) IqX = Iq - NCoreA
                  Dum = G1(Iq,Ip)
                  IF (ABS(Dum) > Small) THEN
                     WRITE(*, '(" Hcore: ", E22.15, 2I3)') Dum, IpX, IqX
                  END IF
               END IF
            END DO
         END IF
      END DO
!
      END SUBROUTINE
