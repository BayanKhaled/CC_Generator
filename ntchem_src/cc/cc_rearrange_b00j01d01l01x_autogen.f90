      SUBROUTINE  CC_ReArrange_B00J01D01L01X_AutoGen  &
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
      INTEGER :: NStringJ(nJ)
      INTEGER :: NStringC(nD-nB)
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
      INTEGER :: iJ00, IAddJ00
      INTEGER :: iJ01, IAddJ01
      INTEGER :: iC00, IAddC00
      INTEGER :: iC01, IAddC01
      CALL CCLib_DClear(V_, mC*mK*mB*mJ*mAI)
      nK = nL - nJ
      nC = nD - nB
      DO iAI = 1, mAI

         iJ00 = 0
         IAddJ00 = 1
         DO iJ01 = iJ00 + 1, NOc - nJ +  1
            NStringJ( 1) = iJ01
            IAddJ01 = IAddJ00 + IArcWgtOcc(iJ01,  1, nJ)
            IAddJ = IAddJ01
               IAddB = 1
!
!     --- Get sign and Address of string KJ ---
!
                  IAddK = 1
                  ISgnKJ = 1
                  IAddKJ = IAddJ

                  iC00 = 0
                  IAddC00 = 1
                  Lp_iC01: DO iC01 = iC00 + 1, NVr - nC +  1
                     NStringC( 1) = iC01
                     IAddC01 = IAddC00 + IArcWgtVir(iC01,  1, nC)
                     IAddC = IAddC01
                     ISgnCB = 1
                     IAddCB = IAddC
!
!     --- Rearrange the intermediate ---
!
                     V_(IAddC,IAddK,IAddB,IAddJ,iAI) = V(IAddCB,IAddKJ,iAI) * DBLE(ISgnCB * ISgnKJ)
                  END DO Lp_iC01 ! Free internal particles
         END DO    ! Fixed internal holes
      END DO    ! External indices
!
      END SUBROUTINE
