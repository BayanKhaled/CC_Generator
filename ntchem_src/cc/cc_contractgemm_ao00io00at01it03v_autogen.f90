      SUBROUTINE  CC_ContractGEMM_Ao00Io00At01It03V_AutoGen &
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
      INTEGER :: iIt00, IAddIt00
      INTEGER :: iIt01, IAddIt01
      INTEGER :: iIt02, IAddIt02
      INTEGER :: iAt00, IAddAt00
!
      ALLOCATE(U(mCK,mAt,mIt))
         IAddIo = 1
!
            IAddAo = 1
!
            CALL DGEMM("N", "N", mCK, (mAt*mIt), mBJ, One, V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)
!
            DO iIt00 =  3, NOc    ! loop over external hole(s) in V
               NStringIt( 3) = iIt00
               IAddIt00 = 1 + IArcWgtOcc(iIt00,  3, nIt)
            DO  iIt01 =  2, iIt00 - 1
               NStringIt( 2) =  iIt01
               IAddIt01 = IAddIt00 + IArcWgtOcc(iIt01,  2, nIt)
            DO  iIt02 =  1, iIt01 - 1
               NStringIt( 1) =  iIt02
               IAddIt02 = IAddIt01 + IArcWgtOcc(iIt02,  1, nIt)
               IAddIt = IAddIt02
               ISgnIn = 1
               IAddIn = IAddIt
!
            DO iAt00 =  1, NVr   ! loop over external particle(s) in V
               NStringAt( 1) = iAt00
               IAddAt00 = 1 + IArcWgtVir(iAt00,  1, nAt)
               IAddAt = IAddAt00
               ISgnAn = 1
               IAddAn = IAddAt
!
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)
!
               END DO  ! T ext. particles
!
            END DO  ! T ext. holes
            END DO
            END DO
      DEALLOCATE(U)
!
      END SUBROUTINE
