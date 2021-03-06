      SUBROUTINE  CC_ReArrange_B01J00D02L00X_AutoGen  &
      &   (V, V_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
!
!     o This is an automatically generated program
!
!     Re-arranges the array in the form (D,L,AI) to the form (C,K,B,J,AI)
!     nB, nJ, ...; # of occupations
!     mB, mJ, ...: # of all possible strings
!
      USE CC_Module, ONLY : NOc, NVr, IArcWgtOcc, IArcWgtVir
!
      IMPLICIT NONE
!
      INTEGER :: nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK
      REAL(8):: V(mD,mL,mAI), V_(mC,mK,mB,mJ,mAI)
!
      LOGICAL :: COIN
      INTEGER :: NStringB(nB)
      INTEGER :: NStringC(nD-nB)
      INTEGER :: NStringCB(nD)
!
      INTEGER :: nC, nK, jB
      INTEGER :: IAddJ
      INTEGER :: IAddB
      INTEGER :: IAddK
      INTEGER :: IAddC
      INTEGER :: IAddKJ
      INTEGER :: IAddCB
      INTEGER :: ISgnKJ
      INTEGER :: ISgnCB
      INTEGER :: iAI
      INTEGER :: iB00, IAddB00
      INTEGER :: iB01, IAddB01
      INTEGER :: iC00, IAddC00
      INTEGER :: iC01, IAddC01
      CALL CCLib_DClear(V_, mC*mK*mB*mJ*mAI)
      nK = nL - nJ
      nC = nD - nB
      DO iAI = 1, mAI

            IAddJ = 1

            iB00 = 0
            IAddB00 = 1
            DO iB01 = iB00 + 1, NVr - nB +  1
               NStringB( 1) = iB01
               IAddB01 = IAddB00 + IArcWgtVir(iB01,  1, nB)
               IAddB = IAddB01
                  IAddK = 1
                  ISgnKJ = 1
                  IAddKJ = 1

                  iC00 = 0
                  IAddC00 = 1
                  Lp_iC01: DO iC01 = iC00 + 1, NVr - nC +  1
                     NStringC( 1) = iC01
                     IF (iC01 == iB01) CYCLE
                     IAddC01 = IAddC00 + IArcWgtVir(iC01,  1, nC)
                     IAddC = IAddC01
!
!     --- Get sign and Address of string C.B ---
!
                     COIN = .FALSE.
                     CALL CCLib_MergeStrings(NStringC, NStringB, NStringCB, nC, nB, ISgnCB, COIN)
                     IF (COIN) CYCLE
                     CALL CCLib_StringAddress(NStringCB, nC+nB, "Vir", IAddCB)
!
!     --- Rearrange the intermediate ---
!
                     V_(IAddC,IAddK,IAddB,IAddJ,iAI) = V(IAddCB,IAddKJ,iAI) * DBLE(ISgnCB * ISgnKJ)
                  END DO Lp_iC01 ! Free internal particles
            END DO    ! Fixed internal particles
      END DO    ! External indices
!
      END SUBROUTINE
