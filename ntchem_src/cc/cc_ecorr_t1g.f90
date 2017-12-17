      SUBROUTINE CC_ECorr_T1G(G, T1, ECorr)
!
      USE CC_Module, ONLY : NVr, NOc
      USE CC_Constant_Module, ONLY : Zero, Half, One
!
!     o CC correlation energy: Two-electron integral * T1 contribution
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G((NVr*(NVr-1))/2,*), T1(NVr,*)
      REAL(8), INTENT(OUT) :: ECorr
!
      REAL(8), PARAMETER :: Small = 1.0D-12
      INTEGER :: IOc, JOc, KVr, LVr, IJOc, KLVr
      REAL(8) :: Dum, DumG, DumT, Fac1, Fac2
!
      Dum = Zero
!
      DO JOc = 1, NOc
         DO IOc = 1, NOc
            IF (IOc > JOc) THEN
               IJOc = ((IOc - 1) * (IOc - 2)) / 2 + JOc
               Fac1 = (-One)
            ElSE IF (IOc < JOc) THEN
               IJOc = ((JOc - 1) * (JOc - 2)) / 2 + IOc
               Fac1 = One
            ELSE
               CYCLE
            END IF
!
            DO LVr = 1, NVr
               DO KVr = 1, NVr
                  IF (KVr > LVr) THEN
                     KLVr = ((KVr - 1) * (KVr - 2)) / 2 + LVr
                     Fac2 = -Fac1
                  ElSE IF (KVr < LVr) THEN
                     KLVr = ((LVr - 1) * (LVr - 2)) / 2 + KVr
                     Fac2 = Fac1
                  ELSE
                     CYCLE
                  END IF
!
                  DumG = G(KLVr,IJOc)
                  IF (ABS(DumG) < Small) CYCLE
                  DumT = T1(KVr,IOc) * T1(LVr,JOc)
                  Dum = Dum + Fac2 * DumG * DumT
!
               END DO
            END DO
         END DO
      END DO
!
      ECorr = ECorr + Dum * Half
!
      END SUBROUTINE
