      SUBROUTINE CC_UpdateT1_QN_ASCC(W, T)
!
      USE CC_Module, ONLY : NOc, NVr, Ene, NOcA, NOcB, NVrA, NVrB, NOccModelA, NVirModelA, NOccModelB, NVirModelB
      USE CC_Constant_Module, ONLY : Zero
!
!     o Update T1 amplitude (No DIIS ver.)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(INOUT) :: W(NVr,NOc), T(NVr,NOc)
!
!ASCC_Bgn
      LOGICAL :: Active, ActiveI01, ActiveJ01
!ASCC_End
      INTEGER :: ICnt, JCnt
      INTEGER :: I01, J01
      REAL(8) :: EFac
!
      JCnt = 0
      EFac = Zero
      DO J01 = 1, NOc
!ASCC_Bgn
         ActiveJ01 = (((J01 <= NOcA) .AND. ((NOcA - J01 + 1) <= NOccModelA)) .OR. &
     &      ((J01 > NOcA) .AND. ((NOcB - (J01 - NOcA) + 1) <= NOccModelB)))
!ASCC_End
         JCnt = JCnt + 1
         ICnt = 0
         DO I01 = 1, NVr
!ASCC_Bgn
            ActiveI01 = (((I01 <= NVrA) .AND. (I01 <= NVirModelA)) .OR. &
     &         ((I01 > NVrA) .AND. ((I01 - NVrA) <= NVirModelB)))
            Active = (ActiveI01 .AND. ActiveJ01)
!ASCC_End
            EFac = Ene(J01) - Ene(NOc+I01)
            ICnt = ICnt + 1
!ASCC_Bgn
            IF (.NOT. Active) W(ICnt,JCnt) = Zero
!ASCC_End
            W(ICnt,JCnt) = W(ICnt,JCnt) / EFac
            T(ICnt,JCnt) = T(ICnt,JCnt) + W(ICnt,JCnt)
         END DO
      END DO
!
      END SUBROUTINE
