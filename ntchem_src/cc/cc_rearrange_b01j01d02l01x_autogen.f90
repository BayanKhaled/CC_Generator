      SUBROUTINE  CC_ReArrange_B01J01D02L01X_AutoGen  &
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
      INTEGER :: NStringJ(nJ)
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
      INTEGER :: iJ00, IAddJ00
      INTEGER :: iJ01, IAddJ01
      INTEGER :: iB00, IAddB00
      INTEGER :: iB01, IAddB01
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

            iB00 = 0
            IAddB00 = 1
            DO iB01 = iB00 + 1, NVr - nB +  1
               NStringB( 1) = iB01
               IAddB01 = IAddB00 + IArcWgtVir(iB01,  1, nB)
               IAddB = IAddB01
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
         END DO    ! Fixed internal holes
      END DO    ! External indices
!
      END SUBROUTINE
