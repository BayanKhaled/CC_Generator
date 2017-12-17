      SUBROUTINE  CC_ContractGEMM_Ao01Io03At01It00V_AutoGen &
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
      INTEGER :: iIo01, IAddIo01
      INTEGER :: iIo02, IAddIo02
      INTEGER :: iAo00, IAddAo00
      INTEGER :: iAt00, IAddAt00
!
      ALLOCATE(U(mCK,mAt,mIt))
!
      DO iIo00 =  3, NOc   ! loop over external hole(s) in V
         NStringIo( 3) = iIo00
         IAddIo00 = 1 + IArcWgtOcc(iIo00,  3, nIo)
      DO  iIo01 =  2, iIo00 - 1
         NStringIo( 2) =  iIo01
         IAddIo01 = IAddIo00 + IArcWgtOcc(iIo01,  2, nIo)
      DO  iIo02 =  1, iIo01 - 1
         NStringIo( 1) =  iIo02
         IAddIo02 = IAddIo01 + IArcWgtOcc(iIo02,  1, nIo)
         IAddIo = IAddIo02
!
         DO iAo00 =  1, NVr   ! loop over external particle(s) in V
            NStringAo( 1) = iAo00
            IAddAo00 = 1 + IArcWgtVir(iAo00,  1, nAo)
            IAddAo = IAddAo00
!
            CALL DGEMM("N", "N", mCK, (mAt*mIt), mBJ, One, V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)
!
            IAddIt = 1
            ISgnIn = 1
            IAddIn = IAddIo
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
         END DO  ! V ext. particles
!
      END DO  ! V ext. holes
      END DO
      END DO
      DEALLOCATE(U)
!
      END SUBROUTINE
