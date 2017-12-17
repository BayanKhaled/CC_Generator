      SUBROUTINE SOCC_UpdateT2_QN(W, T)
!
!     o Update T2 amplitude (No DIIS ver.)
!
      USE CC_Module, ONLY : NOc, NVr, EneSO
      USE CC_Constant_Module, ONLY : Zero
!
      IMPLICIT NONE
!
      COMPLEX(8), INTENT(INOUT) :: W(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
      COMPLEX(8), INTENT(INOUT) :: T(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
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
!
            ICnt = 0
            DO I01 = 2, NVr
               DO I02 = 1, I01 - 1
                  EFac = EneSO(J01) + EneSO(J02) - EneSO(NOc+I01) - EneSO(NOc+I02)
                  ICnt = ICnt + 1
                  W(ICnt,JCnt) = W(ICnt,JCnt) / DCMPLX(EFac)
                  T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
!
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
