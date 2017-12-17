      SUBROUTINE  CC_ReArrange_B00J01D02L02N_AutoGen  &
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
      REAL(8):: V(mD,mL), V_(mC,mK,mB,mJ)
!
      LOGICAL :: COIN
      INTEGER :: NStringJ(nJ)
      INTEGER :: NStringC(nD-nB)
      INTEGER :: NStringK(nL-nJ)
      INTEGER :: NStringKJ(nL)
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
      INTEGER :: iJ00, IAddJ00
      INTEGER :: iJ01, IAddJ01
      INTEGER :: iK00, IAddK00
      INTEGER :: iK01, IAddK01
      INTEGER :: iC00, IAddC00
      INTEGER :: iC01, IAddC01
      INTEGER :: iC02, IAddC02
      CALL CCLib_DClear(V_, mC*mK*mB*mJ)
      nK = nL - nJ
      nC = nD - nB
         iJ00 = 0
         IAddJ00 = 1
         DO iJ01 = iJ00 + 1, NOc - nJ +  1
            NStringJ( 1) = iJ01
            IAddJ01 = IAddJ00 + IArcWgtOcc(iJ01,  1, nJ)
            IAddJ = IAddJ01
               IAddB = 1

               iK00 = 0
               IAddK00 = 1
               Lp_iK01: DO   iK01 = iK00 + 1, NOc - nK +  1
                  NStringK( 1) = iK01
                  IF (iK01 == iJ01) CYCLE
                  IAddK01 = IAddK00 + IArcWgtOcc(iK01,  1, nK)
                  IAddK = IAddK01
!
!     --- Get sign and Address of string K.J ---
!
                  COIN = .FALSE.
                  CALL CCLib_MergeStrings(NStringK, NStringJ,NStringKJ, nK, nJ, ISgnKJ, COIN)
                  IF (COIN) CYCLE
                  CALL CCLib_StringAddress(NStringKJ, nK+nJ, "Occ", IAddKJ)

                  iC00 = 0
                  IAddC00 = 1
                  Lp_iC01: DO iC01 = iC00 + 1, NVr - nC +  1
                     NStringC( 1) = iC01
                     IAddC01 = IAddC00 + IArcWgtVir(iC01,  1, nC)
                  Lp_iC02: DO iC02 = iC01 + 1, NVr - nC +  2
                     NStringC( 2) = iC02
                     IAddC02 = IAddC01 + IArcWgtVir(iC02,  2, nC)
                     IAddC = IAddC02
                     ISgnCB = 1
                     IAddCB = IAddC
!
!     --- Rearrange the intermediate ---
!
                     V_(IAddC,IAddK,IAddB,IAddJ) = V(IAddCB,IAddKJ) * DBLE(ISgnCB * ISgnKJ)
                  END DO Lp_iC02 ! Free internal particles
                  END DO Lp_iC01
               END DO Lp_iK01  ! Free internal holes
         END DO    ! Fixed internal holes
!
      END SUBROUTINE
