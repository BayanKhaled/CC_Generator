      SUBROUTINE CC_UpdateT1_QN(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene
      USE CC_Constant_Module, ONLY : Zero
!
!     o Update T1 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr,NOc), T(NVr,NOc)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, J01
      REAL(8) :: EFac
!
      JCnt = 0
      EFac = Zero
      DO J01 = 1, NOc
         JCnt = JCnt + 1
         ICnt = 0
         DO I01 = 1, NVr
            EFac = Ene(J01) - Ene(NOc+I01)
            ICnt = ICnt + 1
            W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
            T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
         END DO
      END DO
!
      END SUBROUTINE
