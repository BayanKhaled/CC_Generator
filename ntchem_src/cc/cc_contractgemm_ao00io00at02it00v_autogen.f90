      SUBROUTINE  CC_ContractGEMM_Ao00Io00At02It00V_AutoGen &
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
      INTEGER :: iAt00, IAddAt00
      INTEGER :: iAt01, IAddAt01
      ALLOCATE(U(mCK,mAt,mIt))
         IAddIo = 1
!
            IAddAo = 1
!
            CALL DGEMM("N", "N", mCK, (mAt*mIt), mBJ, One, V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)
!
            IAddIt = 1
            ISgnIn = 1
            IAddIn = 1
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
                  CALL DAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)
!
               END DO  ! T ext. particles
               END DO
      DEALLOCATE(U)
!
      END SUBROUTINE
