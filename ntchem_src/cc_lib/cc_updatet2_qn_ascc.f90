      SUBROUTINE CC_UpdateT2_QN_ASCC(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene, NOcA, NOcB, NVrA, NVrB, NOccModelA, NVirModelA, NOccModelB, NVirModelB
      USE CC_Constant_Module, ONLY : Zero
!
!     o Update T2 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
      REAL(8), INTENT(INOUT) :: T(NVr*(NVr-1)/2,NOc*(NOc-1)/2)
!
!ASCC_Bgn
      LOGICAL :: Active, ActiveI01, ActiveJ01, ActiveI02, ActiveJ02
!ASCC_End
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, I02, J01, J02
      REAL(8) :: EFac
!
      JCnt = 0
      EFac = Zero
      DO J01 = 2, NOc
!ASCC_Bgn
         ActiveJ01 = (((J01 <= NOcA) .AND. ((NOcA - J01 + 1) <= NOccModelA)) .OR. &
     &      ((J01 > NOcA) .AND. ((NOcB - (J01 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
         DO J02 = 1, J01 - 1
!ASCC_Bgn
            ActiveJ02 = (((J02 <= NOcA) .AND. ((NOcA - J02 + 1) <= NOccModelA)) .OR. &
     &         ((J02 > NOcA) .AND. ((NOcB - (J02 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
            JCnt = JCnt + 1
            ICnt = 0
            DO I01 = 2, NVr
!ASCC_Bgn
               ActiveI01 = (((I01 <= NVrA) .AND. (I01 <= NVirModelA)) .OR. &
     &            ((I01 > NVrA) .AND. ((I01 - NVrA) <= NVirModelB)))
!ASCC_End
               DO I02 = 1, I01 - 1
!ASCC_Bgn
                  ActiveI02 = (((I02 <= NVrA) .AND. (I02 <= NVirModelA)) .OR. &
     &               ((I02 > NVrA) .AND. ((I02 - NVrA) <= NVirModelB)))
                  Active = (ActiveI01 .AND. ActiveJ01 .AND. ActiveI02 .AND. ActiveJ02)
!ASCC_End
                  EFac = Ene(J01) + Ene(J02) - Ene(NOc+I01) - Ene(NOc+I02)
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
      END SUBROUTINE
