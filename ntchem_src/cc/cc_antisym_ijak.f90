      SUBROUTINE CC_AntiSym_IJAK(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc
!
!     o Anti-symmetrize (IJ|AK) integrals (J <--> K)
!       Integrals are stored as G1(A,K<J,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(NVr,((NOc*(NOc-1))/2),*)
      REAL(8), INTENT(IN) :: G2(NOc,NOc,NVr,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-12
      INTEGER :: IOc, IVr, JOc, KOc, JKOc, JOff
      REAL(8) :: Dum1, Dum2
!
      CALL CCLib_DClear(G1, NVr*(NOc*(NOc-1))/2*NOc)
!
!$OMP PARALLEL PRIVATE(IOc, IVr, JOc, JOff, KOc, Dum1, Dum2, JKOc)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IOc = 1, NOc
         DO IVr = 1, NVr
            DO JOc = 2, NOc
               JOff = ((JOc - 1) * (JOc - 2)) / 2
               DO KOc = 1, JOc - 1
                  Dum1 = G2(JOc,KOc,IVr,IOc)
                  Dum2 = G2(KOc,JOc,IVr,IOc)
                  IF (ABS(Dum1 - Dum2) <= Small) CYCLE
                  JKOc = JOff + KOc
                  G1(IVr,JKOc,IOc) = Dum1 - Dum2
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
