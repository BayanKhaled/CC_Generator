      SUBROUTINE SOCC_UpdateT4_QN(W, T)
!
!     o Update T4 amplitude (No DIIS ver.)
!
      USE CC_Module, ONLY : NOc, NVr, EneSO
!
      IMPLICIT NONE
!
      COMPLEX(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
      COMPLEX(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
!
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, I04, J01, J02, J03, J04
      REAL(8) :: EFac, EFacO
!
      JCnt = 0
      DO J01 = 4, NOc
         DO J02 = 3, J01 - 1
            DO J03 = 2, J02 - 1
               DO J04 = 1, J03 - 1
                  EFacO = EneSO(J01) + EneSO(J02) + EneSO(J03) + EneSO(J04)
                  JCnt = JCnt + 1
                  ICnt = 0
                  DO I01 = 4, NVr
                     DO I02 = 3, I01 - 1
                        DO I03 = 2, I02 - 1
                           DO I04 = 1, I03 - 1
                              EFac = EFacO - EneSO(NOc+I01) - EneSO(NOc+I02) - EneSO(NOc+I03) - EneSO(NOc+I04)
                              ICnt = ICnt + 1
                              W(ICnt,JCnt) = W(ICnt,JCnt) / DCMPLX(EFac)
                              T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
                           END DO
                        END DO
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      END SUBROUTINE
