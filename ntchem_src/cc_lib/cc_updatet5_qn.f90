      SUBROUTINE CC_UpdateT5_QN(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene
!
!     o Update T5 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)*(NVr-3)*(NVr-4)/120,*)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)*(NVr-3)*(NVr-4)/120,*)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, I04, I05, J01, J02, J03, J04, J05
      REAL(8) :: EFac, EFacO
!
      JCnt = 0
      DO J01 = 5, NOc
         DO J02 = 4, J01 - 1
            DO J03 = 3, J02 - 1
               DO J04 = 2, J03 - 1
                  DO J05 = 1, J04 - 1
                     EFacO = Ene(J01) + Ene(J02) + Ene(J03) + Ene(J04) + Ene(J05)
                     JCnt = JCnt + 1
!
                     ICnt = 0
                     DO I01 = 5, NVr
                        DO I02 = 4, I01 - 1
                           DO I03 = 3, I02 - 1
                              DO I04 = 2, I03 - 1
                                 DO I05 = 1, I04 - 1
                                    EFac = EFacO - Ene(NOc+I01) - Ene(NOc+I02) - Ene(NOc+I03) - Ene(NOc+I04) - Ene(NOc+I05)
                                    ICnt = ICnt + 1
                                    W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
                                    T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
                                 END DO
                              END DO
                           END DO
                        END DO
                     END DO
!
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
