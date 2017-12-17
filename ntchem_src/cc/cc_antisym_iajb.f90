      SUBROUTINE CC_AntiSym_IAJB(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr
!
!     o Anti-symmetrize (IA|JB) integrals (A <--> B)
!       Integrals are stored as G1(BA,JI) (B<A, J<I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(((NVr*(NVr-1))/2),*)
      REAL(8), INTENT(IN) :: G2(NVr,NVr,*)
!
      INTEGER :: IJOc, KLVr, IOc, JOc, KVr, LVr, IOff, KOff
!
      CALL CCLib_DClear(G1, (NVr*(NVr-1))/2*(NOc*(NOc-1))/2)
!
!$OMP PARALLEL PRIVATE(IOc, IOff, JOc, IJOc, KVr, KOff, LVr, KLVr)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IOc = NOc, 2, -1
         IOff = ((IOc - 1) * (IOc - 2)) / 2
         DO JOc = 1, IOc - 1
            IJOc = IOff + JOc
            DO KVr = 2, NVr
               KOff = ((KVr - 1) * (KVr - 2)) / 2
               DO LVr = 1, KVr - 1
                  KLVr = KOff + LVr
                  G1(KLVr,IJOc) = G2(LVr,KVr,IJOc) - G2(KVr,LVr,IJOc)
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
