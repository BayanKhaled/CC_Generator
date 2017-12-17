      SUBROUTINE CC_AntiSym_LKIJ(G1, G2)
!
      USE CC_Module, ONLY : NOc
!
!     o Anti-symmetrize <JK|IL> integrals
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1((NOc*(NOc-1))/2,*)
      REAL(8), INTENT(IN) :: G2(NOc,NOc,*)
!
      INTEGER :: IOc, JOc, KOc, LOc, IOff, KOff, IJOc, KLOc
!
      CALL CCLib_DClear(G1, (NOc*(NOc-1))/2*(NOc*(NOc-1))/2)
!
!$OMP PARALLEL PRIVATE(IOc, IOff, JOc, IJOc, KOc, KOff, LOc, KLOc)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IOc = NOc, 2, -1
         IOff = ((IOc - 1) * (IOc - 2)) / 2
         DO JOc = 1, IOc - 1
            IJOc = IOff + JOc
            DO KOc = 2, NOc
               KOff = ((KOc - 1) * (KOc - 2)) / 2
               DO LOc = 1, KOc - 1
                  KLOc = KOff + LOc
                  G1(KLOc,IJOc) = G2(LOc,KOc,IJOc) - G2(KOc,LOc,IJOc)
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
