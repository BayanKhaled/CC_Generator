      SUBROUTINE  CC_ReArrange_B04J02D04L04N_AutoGen  &
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
      INTEGER :: NStringB(nB)
      INTEGER :: NStringJ(nJ)
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
      INTEGER :: iJ02, IAddJ02
      INTEGER :: iB00, IAddB00
      INTEGER :: iB01, IAddB01
      INTEGER :: iB02, IAddB02
      INTEGER :: iB03, IAddB03
      INTEGER :: iB04, IAddB04
      INTEGER :: iK00, IAddK00
      INTEGER :: iK01, IAddK01
      INTEGER :: iK02, IAddK02
      CALL CCLib_DClear(V_, mC*mK*mB*mJ)
      nK = nL - nJ
      nC = nD - nB
         iJ00 = 0
         IAddJ00 = 1
         DO iJ01 = iJ00 + 1, NOc - nJ +  1
            NStringJ( 1) = iJ01
            IAddJ01 = IAddJ00 + IArcWgtOcc(iJ01,  1, nJ)
         DO iJ02 = iJ01 + 1, NOc - nJ +  2
            NStringJ( 2) = iJ02
            IAddJ02 = IAddJ01 + IArcWgtOcc(iJ02,  2, nJ)
            IAddJ = IAddJ02

            iB00 = 0
            IAddB00 = 1
            DO iB01 = iB00 + 1, NVr - nB +  1
               NStringB( 1) = iB01
               IAddB01 = IAddB00 + IArcWgtVir(iB01,  1, nB)
            DO iB02 = iB01 + 1, NVr - nB +  2
               NStringB( 2) = iB02
               IAddB02 = IAddB01 + IArcWgtVir(iB02,  2, nB)
            DO iB03 = iB02 + 1, NVr - nB +  3
               NStringB( 3) = iB03
               IAddB03 = IAddB02 + IArcWgtVir(iB03,  3, nB)
            DO iB04 = iB03 + 1, NVr - nB +  4
               NStringB( 4) = iB04
               IAddB04 = IAddB03 + IArcWgtVir(iB04,  4, nB)
               IAddB = IAddB04

               iK00 = 0
               IAddK00 = 1
               Lp_iK01: DO   iK01 = iK00 + 1, NOc - nK +  1
                  NStringK( 1) = iK01
                  IF (iK01 == iJ01) CYCLE
                  IF (iK01 == iJ02) CYCLE
                  IAddK01 = IAddK00 + IArcWgtOcc(iK01,  1, nK)
               Lp_iK02: DO   iK02 = iK01 + 1, NOc - nK +  2
                  NStringK( 2) = iK02
                  IF (iK02 == iJ01) CYCLE
                  IF (iK02 == iJ02) CYCLE
                  IAddK02 = IAddK01 + IArcWgtOcc(iK02,  2, nK)
                  IAddK = IAddK02
!
!     --- Get sign and Address of string K.J ---
!
                  COIN = .FALSE.
                  CALL CCLib_MergeStrings(NStringK, NStringJ,NStringKJ, nK, nJ, ISgnKJ, COIN)
                  IF (COIN) CYCLE
                  CALL CCLib_StringAddress(NStringKJ, nK+nJ, "Occ", IAddKJ)
!
!     --- Get sign and Address of string B ---
!
                     IAddC = 1
                     ISgnCB = 1
                     IAddCB = IAddB
!
!     --- Rearrange the intermediate ---
!
                     V_(IAddC,IAddK,IAddB,IAddJ) = V(IAddCB,IAddKJ) * DBLE(ISgnCB * ISgnKJ)
               END DO Lp_iK02  ! Free internal holes
               END DO Lp_iK01
            END DO    ! Fixed internal particles
            END DO
            END DO
            END DO
         END DO    ! Fixed internal holes
         END DO
!
      END SUBROUTINE
