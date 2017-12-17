      SUBROUTINE CC_UpdateT3_QN(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene
!
!     o Update T3 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)/6,*)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)/6,*)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, J01, J02, J03
      REAL(8) :: EFac, EFacO
!
      JCnt = 0
      DO J01 = 3, NOc
         DO J02 = 2, J01 - 1
            DO J03 = 1, J02 - 1
               EFacO = Ene(J01) + Ene(J02) + Ene(J03)
               JCnt = JCnt + 1
!
               ICnt = 0
               DO I01 = 3, NVr
                  DO I02 = 2, I01 - 1
                     DO I03 = 1, I02 - 1
                        EFac = EFacO - Ene(NOc+I01) - Ene(NOc+I02) - Ene(NOc+I03)
                        ICnt = ICnt + 1
                        W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
                        T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
                     END DO
                  END DO
               END DO
!
            END DO
         END DO
      END DO
!
      END SUBROUTINE
