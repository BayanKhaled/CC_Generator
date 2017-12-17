      SUBROUTINE CC_UpdateT4_QN(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene
!
!     o Update T4 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, I04, J01, J02, J03, J04
      REAL(8) :: EFac,EFacO
!
      JCnt = 0
      DO J01 = 4, NOc
         DO J02 = 3, J01 - 1
            DO J03 = 2, J02 - 1
               DO J04 = 1, J03 - 1
                  EFacO = Ene(J01) + Ene(J02) + Ene(J03) + Ene(J04)
                  JCnt = JCnt + 1
!
                  ICnt = 0
                  DO I01 = 4, NVr
                     DO I02 = 3, I01 - 1
                        DO I03 = 2, I02 - 1
                           DO I04 = 1, I03 - 1
                              EFac = EFacO - Ene(NOc+I01) - Ene(NOc+I02) - Ene(NOc+I03) - Ene(NOc+I04)
                              ICnt = ICnt + 1
                              W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
                              T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
                           END DO
                        END DO
                     END DO
                  END DO
!
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
