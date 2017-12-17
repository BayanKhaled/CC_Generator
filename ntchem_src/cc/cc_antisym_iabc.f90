      SUBROUTINE CC_AntiSym_IABC(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NBF, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o Anti-symmetrize (IA|BC) integrals (A <--> C)
!      Integrals are stored as G1(B,A<C,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(NVr,((NVr*(NVr-1))/2),*)
      REAL(8), INTENT(IN) :: G2(NVr,NVr,NVr,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-12
      INTEGER :: IOc, IVr, KVr, JVr, KOff, IKv
      REAL(8) :: Dum1, Dum2
!
      CALL CCLib_DClear(G1, (NVr*(NVr*(NVr-1))/2*NOc))
!
!$OMP PARALLEL PRIVATE(IOc, JVr, KVr, KOff, IVr, Dum1, Dum2, IKv)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IOc = 1, NOc
         DO JVr = 1, NVr
            DO KVr = 2, NVr
               KOff = ((KVr - 1) * (KVr - 2)) / 2
               DO IVr = 1, KVr - 1
                  Dum1 = G2(IVr,KVr,JVr,IOc)
                  Dum2 = G2(KVr,IVr,JVr,IOc)
                  IF (ABS(Dum1 - Dum2) <= Small) CYCLE
                  IKv = KOff + IVr
                  G1(JVr,IKv,IOc) = Dum1 - Dum2
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
