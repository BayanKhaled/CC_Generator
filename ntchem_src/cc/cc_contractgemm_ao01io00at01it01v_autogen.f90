      SUBROUTINE  CC_ContractGEMM_Ao01Io00At01It01V_AutoGen &
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
      INTEGER :: iAo00, IAddAo00
      INTEGER :: iIt00, IAddIt00
      INTEGER :: iAt00, IAddAt00
!
      ALLOCATE(U(mCK,mAt,mIt))
         IAddIo = 1
!
         DO iAo00 =  1, NVr   ! loop over external particle(s) in V
            NStringAo( 1) = iAo00
            IAddAo00 = 1 + IArcWgtVir(iAo00,  1, nAo)
            IAddAo = IAddAo00
!
            CALL DGEMM("N", "N", mCK, (mAt*mIt), mBJ, One, V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)
!
            DO iIt00 =  1, NOc    ! loop over external hole(s) in V
               NStringIt( 1) = iIt00
               IAddIt00 = 1 + IArcWgtOcc(iIt00,  1, nIt)
               IAddIt = IAddIt00
               ISgnIn = 1
               IAddIn = IAddIt
!
            DO iAt00 =  1, NVr   ! loop over external particle(s) in V
               IF (iAt00 == iAo00) CYCLE
               NStringAt( 1) = iAt00
               IAddAt00 = 1 + IArcWgtVir(iAt00,  1, nAt)
               IAddAt = IAddAt00
               COIN = .FALSE.
               CALL CCLib_MergeStrings(NStringAo, NStringAt, NStringAn, nAo, nAt, ISgnAn, COIN)
               IF (COIN) CYCLE
               CALL CCLib_StringAddress(NStringAn, nAo+nAt, "Vir", IAddAn)
!
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)
!
               END DO  ! T ext. particles
!
            END DO  ! T ext. holes
!
         END DO  ! V ext. particles
      DEALLOCATE(U)
!
      END SUBROUTINE
