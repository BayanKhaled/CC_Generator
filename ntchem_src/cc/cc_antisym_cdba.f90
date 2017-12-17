      SUBROUTINE CC_AntiSym_CDBA(G1, G2)
!
!     o Anti-symmetrize <AD|BC> integrals
!
      USE CC_Module, ONLY : NVr, NVrA, NVrB
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(((NVr*(NVr-1))/2),*)
      REAL(8), INTENT(IN) :: G2(NVr,NVr,*)
!
      INTEGER :: IVr, JVr, KVr, LVr, IOff, KOff, IJVr, KLVr
!
      CALL CCLib_DClear(G1, (NVr*(NVr-1))/2*(NVr*(NVr-1))/2)
!
!$OMP PARALLEL PRIVATE(IVr, IOff, JVr, IJVr, KVr, KOff, LVr, KLVr)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IVr = NVr, 2, -1
         IOff = ((IVr - 1) * (IVr - 2)) / 2
         DO JVr = 1, IVr - 1
            IJVr = IOff + JVr
            DO KVr = 2, NVr
               KOff = ((KVr - 1) * (KVr - 2)) / 2
               DO LVr = 1, KVr - 1
                  KLVr = KOff + LVr
                  G1(KLVr,IJVr) = G2(LVr,KVr,IJVr) - G2(KVr,LVr,IJVr)
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
