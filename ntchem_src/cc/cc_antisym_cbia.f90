      SUBROUTINE CC_AntiSym_CBIA(G1, G2)
!
!     o Anti-symmetrize <CI|BA> = [CB|IA] integrals (A <--> B)
!
      USE CC_Module, ONLY : NVr, NVrA, NVrB, NOc, NOcA, NOcB
      USE CC_Constant_Module, ONLY : Zero
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G2(NVr,NVr,NOc,*)
      REAL(8), INTENT(OUT) :: G1(((NVr*(NVr-1))/2),NOc,*)
!
      INTEGER :: IVr, JVr, KVr, IOc, JKVr
      REAL(8) :: Dum1, Dum2
!
      CALL CCLib_DClear(G1, ((NVr*(NVr-1))/2*NOc*NVr))
!
!$OMP PARALLEL PRIVATE(IVr, IOc, JVr, KVr, JKVr, Dum1, Dum2)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IVr = 1, NVr
         DO IOc = 1, NOc
            DO JVr = 2, NVr
               DO KVr = 1, (JVr - 1)
                  JKVr = ((JVr - 1) * (JVr - 2)) / 2 + KVr
                  Dum1 = G2(JVr,KVr,IOc,IVr)
                  Dum2 = G2(KVr,JVr,IOc,IVr)
                  G1(JKVr,IOc,IVr) = Dum1 - Dum2
               END DO
            END DO
         END DO
      END DO
!$OMP END DO 
!$OMP END PARALLEL
!
      END SUBROUTINE
