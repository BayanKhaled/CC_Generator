      SUBROUTINE  CC_ContractGEMM_Ao00Io00At02It02S_AutoGen &
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
      REAL(8):: V(mBJ,mAo,mIo), T(mBJ,mAt,mIt), W(mAn,mIn)
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
      INTEGER :: IAddAtIt
      INTEGER :: iIt00, IAddIt00
      INTEGER :: iIt01, IAddIt01
      INTEGER :: iAt00, IAddAt00
      INTEGER :: iAt01, IAddAt01
!
      ALLOCATE(U(mAt*mIt,mAo,mIo))
!
      CALL DGEMM("T", "N", (mAt*mIt), (mAo*mIo), mBJ, One, T, mBJ, V, mBJ, Zero, U, (mAt*mIt))
         IAddIo = 1
!
            IAddAo = 1
!
            DO iIt00 =  2, NOc    ! loop over external hole(s) in V
               NStringIt( 2) = iIt00
               IAddIt00 = 1 + IArcWgtOcc(iIt00,  2, nIt)
            DO  iIt01 =  1, iIt00 - 1
               NStringIt( 1) =  iIt01
               IAddIt01 = IAddIt00 + IArcWgtOcc(iIt01,  1, nIt)
               IAddIt = IAddIt01
               ISgnIn = 1
               IAddIn = IAddIt
!
            DO iAt00 =  2, NVr   ! loop over external particle(s) in V
               NStringAt( 2) = iAt00
               IAddAt00 = 1 + IArcWgtVir(iAt00,  2, nAt)
            DO  iAt01 =  1, iAt00 - 1
               NStringAt( 1) =  iAt01
               IAddAt01 = IAddAt00 + IArcWgtVir(iAt01,  1, nAt)
               IAddAt = IAddAt01
               ISgnAn = 1
               IAddAn = IAddAt
!
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  IAddAtIt = mAt * (IAddIt - 1) + IAddAt
                  W(IAddAn,IAddIn) = W(IAddAn,IAddIn) + Alpha * U(IAddAtIt,IAddAo,IAddIo)
!
               END DO  ! T ext. particles
               END DO
!
            END DO  ! T ext. holes
            END DO
      DEALLOCATE(U)
!
      END SUBROUTINE
