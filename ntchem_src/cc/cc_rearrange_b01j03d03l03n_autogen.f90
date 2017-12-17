      SUBROUTINE  CC_ReArrange_B01J03D03L03N_AutoGen  &
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
      INTEGER :: iJ00, IAddJ00
      INTEGER :: iJ01, IAddJ01
      INTEGER :: iJ02, IAddJ02
      INTEGER :: iJ03, IAddJ03
      INTEGER :: iB00, IAddB00
      INTEGER :: iB01, IAddB01
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
         DO iJ02 = iJ01 + 1, NOc - nJ +  2
            NStringJ( 2) = iJ02
            IAddJ02 = IAddJ01 + IArcWgtOcc(iJ02,  2, nJ)
         DO iJ03 = iJ02 + 1, NOc - nJ +  3
            NStringJ( 3) = iJ03
            IAddJ03 = IAddJ02 + IArcWgtOcc(iJ03,  3, nJ)
            IAddJ = IAddJ03

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
                  Lp_iC02: DO iC02 = iC01 + 1, NVr - nC +  2
                     NStringC( 2) = iC02
                     IF (iC02 == iB01) CYCLE
                     IAddC02 = IAddC01 + IArcWgtVir(iC02,  2, nC)
                     IAddC = IAddC02
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
                     V_(IAddC,IAddK,IAddB,IAddJ) = V(IAddCB,IAddKJ) * DBLE(ISgnCB * ISgnKJ)
                  END DO Lp_iC02 ! Free internal particles
                  END DO Lp_iC01
            END DO    ! Fixed internal particles
         END DO    ! Fixed internal holes
         END DO
         END DO
!
      END SUBROUTINE
