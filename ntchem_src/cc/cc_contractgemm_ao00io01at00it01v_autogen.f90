      SUBROUTINE  CC_ContractGEMM_Ao00Io01At00It01V_AutoGen &
      &   (V, T, W, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
!
!     o This is an automatically generated program
!
!     Contract intermediate V(CK,BJ,Ao,Io) to T(BJ,At,It)
!     to generate a new intermediate W(C,K,An,In)
!
      USE CC_Module, ONLY : NOc, NVr, IArcWgtOcc, IArcWgtVir
      USE CC_Constant_Module, ONLY : One, Zero
!
      IMPLICIT NONE
!
      INTEGER :: nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn
      REAL(8):: V(mCK,mBJ,mAo,mIo), T(mBJ,mAt,mIt), W(mCK,mAn,mIn)
!
      LOGICAL:: COIN
      REAL(8) :: Alpha
      INTEGER:: NStringAt(nAt), NStringIt(nIt), NStringAo(nAo), NStringIo(nIo), NStringAn(nAo+nAt), NStringIn(nIo+nIt)

      REAL(8), ALLOCATABLE :: U(:,:,:)
      INTEGER :: IAddIo
      INTEGER :: IAddAo
      INTEGER :: IAddIt
      INTEGER :: IAddAt
      INTEGER :: IAddIn
      INTEGER :: IAddAn
      INTEGER :: ISgnIn
      INTEGER :: ISgnAn
      INTEGER :: iIo00, IAddIo00
      INTEGER :: iIt00, IAddIt00
!
      ALLOCATE(U(mCK,mAt,mIt))
!
      DO iIo00 =  1, NOc   ! loop over external hole(s) in V
         NStringIo( 1) = iIo00
         IAddIo00 = 1 + IArcWgtOcc(iIo00,  1, nIo)
         IAddIo = IAddIo00
!
            IAddAo = 1
!
            CALL DGEMM("N", "N", mCK, (mAt*mIt), mBJ, One, V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)
!
            DO iIt00 =  1, NOc    ! loop over external hole(s) in V
               IF (iIt00 == iIo00) CYCLE
               NStringIt( 1) = iIt00
               IAddIt00 = 1 + IArcWgtOcc(iIt00,  1, nIt)
               IAddIt = IAddIt00
               COIN = .FALSE.
               CALL CCLib_MergeStrings(NStringIo, NStringIt, NStringIn, nIo, nIt, ISgnIn, COIN)
               IF (COIN) CYCLE
               CALL CCLib_StringAddress(NStringIn, nIo+nIt, "Occ", IAddIn)
!
               IAddAt = 1
               ISgnAn = 1
               IAddAn = 1
!
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)
!
            END DO  ! T ext. holes
!
      END DO  ! V ext. holes
      DEALLOCATE(U)
!
      END SUBROUTINE
