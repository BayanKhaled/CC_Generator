      SUBROUTINE CC_UpdateT3_QN_ASCC(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene, NOcA, NOcB, NVrA, NVrB, NOccModelA, NVirModelA, NOccModelB, NVirModelB
!
!     o Update T3 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)*(NVr-2)/6,*)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)*(NVr-2)/6,*)
!
!ASCC_Bgn
      REAL(8), PARAMETER :: Zero = 0.0D+00
      LOGICAL :: Active, ActiveI01, ActiveJ01, ActiveI02, ActiveJ02, ActiveI03, ActiveJ03
!ASCC_End
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, I03, J01, J02, J03
      REAL(8) :: EFac, EFacO
!
      JCnt = 0
      DO J01 = 3, NOc
!ASCC_Bgn
         ActiveJ01 = (((J01 <= NOcA) .AND. ((NOcA - J01 + 1) <= NOccModelA)) .OR. &
     &      ((J01 > NOcA) .AND. ((NOcB - (J01 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
         DO J02 = 2, J01 - 1
!ASCC_Bgn
            ActiveJ02 = (((J02 <= NOcA) .AND. ((NOcA - J02 + 1) <= NOccModelA)) .OR. &
     &         ((J02 > NOcA) .AND. ((NOcB - (J02 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
            DO J03 = 1, J02 - 1
!ASCC_Bgn
               ActiveJ03 = (((J03 <= NOcA) .AND. ((NOcA - J03 + 1) <= NOccModelA)) .OR. &
     &            ((J03 > NOcA) .AND. ((NOcB - (J03 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
               EFacO = Ene(J01) + Ene(J02) + Ene(J03)
               JCnt = JCnt + 1
!
               ICnt = 0
               DO I01 = 3, NVr
!ASCC_Bgn
                  ActiveI01 = (((I01 <= NVrA) .AND. (I01 <= NVirModelA)) .OR. &
     &               ((I01 > NVrA) .AND. ((I01 - NVrA) <= NVirModelB)))
!ASCC_End
                  DO I02 = 2, I01 - 1
!ASCC_Bgn
                     ActiveI02 = (((I02 <= NVrA) .AND. (I02 <= NVirModelA)) .OR. &
     &                  ((I02 > NVrA) .AND. ((I02 - NVrA) <= NVirModelB)))
!ASCC_End
                     DO I03 = 1, I02 - 1
!ASCC_Bgn
                        ActiveI03 = (((I03 <= NVrA) .AND. (I03 <= NVirModelA)) .OR. &
     &                     ((I03 > NVrA) .AND. ((I03 - NVrA) <= NVirModelB)))
                        Active = (ActiveI01 .AND. ActiveJ01 .AND. ActiveI02 .AND. ActiveJ02 .AND. &
     &                     ActiveI03 .AND. ActiveJ03)
!ASCC_End
                        EFac = EFacO - Ene(NOc+I01) - Ene(NOc+I02) - Ene(NOc+I03)
                        ICnt = ICnt + 1
!ASCC_Bgn
                        IF (.NOT. Active) W(ICnt,JCnt) = Zero
!ASCC_End
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
