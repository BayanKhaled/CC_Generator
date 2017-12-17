      SUBROUTINE  CC_ContractGEMM_Ao02Io03At01It00S_AutoGen &
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
      INTEGER :: iIo00, IAddIo00
      INTEGER :: iIo01, IAddIo01
      INTEGER :: iIo02, IAddIo02
      INTEGER :: iAo00, IAddAo00
      INTEGER :: iAo01, IAddAo01
      INTEGER :: iAt00, IAddAt00
!
      ALLOCATE(U(mAt*mIt,mAo,mIo))
!
      CALL DGEMM("T", "N", (mAt*mIt), (mAo*mIo), mBJ, One, T, mBJ, V, mBJ, Zero, U, (mAt*mIt))
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
         DO iAo00 =  2, NVr   ! loop over external particle(s) in V
            NStringAo( 2) = iAo00
            IAddAo00 = 1 + IArcWgtVir(iAo00,  2, nAo)
         DO  iAo01 =  1, iAo00 - 1
            NStringAo( 1) =  iAo01
            IAddAo01 = IAddAo00 + IArcWgtVir(iAo01,  1, nAo)
            IAddAo = IAddAo01
!
            IAddIt = 1
            ISgnIn = 1
            IAddIn = IAddIo
!
            DO iAt00 =  1, NVr   ! loop over external particle(s) in V
               IF (iAt00 == iAo00) CYCLE
               IF (iAt00 == iAo01) CYCLE
               NStringAt( 1) = iAt00
               IAddAt00 = 1 + IArcWgtVir(iAt00,  1, nAt)
               IAddAt = IAddAt00
               COIN = .FALSE.
               CALL CCLib_MergeStrings(NStringAo, NStringAt, NStringAn, nAo, nAt, ISgnAn, COIN)
               IF (COIN) CYCLE
               CALL CCLib_StringAddress(NStringAn, nAo+nAt, "Vir", IAddAn)
!
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  IAddAtIt = mAt * (IAddIt - 1) + IAddAt
                  W(IAddAn,IAddIn) = W(IAddAn,IAddIn) + Alpha * U(IAddAtIt,IAddAo,IAddIo)
!
               END DO  ! T ext. particles
!
         END DO  ! V ext. particles
         END DO
!
      END DO  ! V ext. holes
      END DO
      END DO
      DEALLOCATE(U)
!
      END SUBROUTINE
