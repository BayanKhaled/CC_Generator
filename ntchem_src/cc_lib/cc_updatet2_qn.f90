      SUBROUTINE CC_UpdateT2_QN(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene
      USE CC_Constant_Module, ONLY : Zero
!
!     o Update T2 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, J01, J02
      REAL(8) :: EFac
!
      JCnt = 0
      EFac = Zero
      DO J01 = 2, NOc
         DO J02 = 1, J01 - 1
            JCnt = JCnt + 1
            ICnt = 0
            DO I01 = 2, NVr
               DO I02 = 1, I01 - 1
                  EFac = Ene(J01) + Ene(J02) - Ene(NOc+I01) - Ene(NOc+I02)
                  ICnt = ICnt + 1
                  W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
                  T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
