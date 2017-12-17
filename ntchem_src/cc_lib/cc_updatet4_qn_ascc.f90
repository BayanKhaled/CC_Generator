      SUBROUTINE CC_UpdateT4_QN_ASCC(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene, NOcA, NOcB, NVrA, NVrB, NOccModelA, NVirModelA, NOccModelB, NVirModelB
!
!     o Update T4 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)*(NVr-3)/24,*)
!
!ASCC_Bgn
      REAL(8), PARAMETER :: Zero = 0.0D+00
      LOGICAL :: Active, ActiveI01, ActiveJ01, ActiveI02, ActiveJ02, ActiveI03, ActiveJ03, ActiveI04, ActiveJ04
!ASCC_End
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, I04, J01, J02, J03, J04
      REAL(8) :: EFac,EFacO
!
      JCnt = 0
      DO J01 = 4, NOc
!ASCC_Bgn
         ActiveJ01 = (((J01 <= NOcA) .AND. ((NOcA - J01 + 1) <= NOccModelA)) .OR. &
     &      ((J01 > NOcA) .AND. ((NOcB - (J01 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
         DO J02 = 3, J01 - 1
!ASCC_Bgn
            ActiveJ02 = (((J02 <= NOcA) .AND. ((NOcA - J02 + 1) <= NOccModelA)) .OR. &
     &         ((J02 > NOcA) .AND. ((NOcB - (J02 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
            DO J03 = 2, J02 - 1
!ASCC_Bgn
               ActiveJ03 = (((J03 <= NOcA) .AND. ((NOcA - J03 + 1) <= NOccModelA)) .OR. &
     &            ((J03 > NOcA) .AND. ((NOcB - (J03 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
               DO J04 = 1, J03 - 1
!ASCC_Bgn
                  ActiveJ04 = (((J04 <= NOcA) .AND. ((NOcA - J04 + 1) <= NOccModelA)) .OR. &
     &               ((J04 > NOcA) .AND. ((NOcB - (J04 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
                  EFacO = Ene(J01) + Ene(J02) + Ene(J03) + Ene(J04)
                  JCnt = JCnt + 1
!
                  ICnt = 0
                  DO I01 = 4, NVr
!ASCC_Bgn
                     ActiveI01 = (((I01 <= NVrA) .AND. (I01 <= NVirModelA)) .OR. &
     &                  ((I01 > NVrA) .AND. ((I01 - NVrA) <= NVirModelB)))
!ASCC_End
                     DO I02 = 3, I01 - 1
!ASCC_Bgn
                        ActiveI02 = (((I02 <= NVrA) .AND. (I02 <= NVirModelA)) .OR. &
     &                     ((I02 > NVrA) .AND. ((I02 - NVrA) <= NVirModelB)))
!ASCC_End
                        DO I03 = 2, I02 - 1
!ASCC_Bgn
                           ActiveI03 = (((I03 <= NVrA) .AND. (I03 <= NVirModelA)) .OR. &
     &                        ((I03 > NVrA) .AND. ((I03 - NVrA) <= NVirModelB)))
!ASCC_End
                           DO I04 = 1, I03 - 1
!ASCC_Bgn
                              ActiveI04 = (((I04 <= NVrA) .AND. (I04 <= NVirModelA)) .OR. &
     &                           ((I04 > NVrA)  .AND. ((I04 - NVrA) <= NVirModelB)))
                              Active = (ActiveI01 .AND. ActiveJ01 .AND. ActiveI02 .AND. ActiveJ02 .AND. &
     &                           ActiveI03 .AND. ActiveJ03 .AND. ActiveI04 .AND. ActiveJ04)
!ASCC_End
                              EFac = EFacO - Ene(NOc+I01) - Ene(NOc+I02) - Ene(NOc+I03) - Ene(NOc+I04)
                              ICnt = ICnt + 1
!ASCC_Bgn
                              IF (.NOT. Active) W(ICnt,JCnt) = Zero
!ASCC_End
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
