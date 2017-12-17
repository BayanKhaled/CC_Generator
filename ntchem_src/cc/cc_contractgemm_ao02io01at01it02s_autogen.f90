      SUBROUTINE  CC_ContractGEMM_Ao02Io01At01It02S_AutoGen &
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
      INTEGER :: iAo00, IAddAo00
      INTEGER :: iAo01, IAddAo01
      INTEGER :: iIt00, IAddIt00
      INTEGER :: iIt01, IAddIt01
      INTEGER :: iAt00, IAddAt00
!
      ALLOCATE(U(mAt*mIt,mAo,mIo))
!
      CALL DGEMM("T", "N", (mAt*mIt), (mAo*mIo), mBJ, One, T, mBJ, V, mBJ, Zero, U, (mAt*mIt))
!
      DO iIo00 =  1, NOc   ! loop over external hole(s) in V
         NStringIo( 1) = iIo00
         IAddIo00 = 1 + IArcWgtOcc(iIo00,  1, nIo)
         IAddIo = IAddIo00
!
         DO iAo00 =  2, NVr   ! loop over external particle(s) in V
            NStringAo( 2) = iAo00
            IAddAo00 = 1 + IArcWgtVir(iAo00,  2, nAo)
         DO  iAo01 =  1, iAo00 - 1
            NStringAo( 1) =  iAo01
            IAddAo01 = IAddAo00 + IArcWgtVir(iAo01,  1, nAo)
            IAddAo = IAddAo01
!
            DO iIt00 =  2, NOc    ! loop over external hole(s) in V
               IF (iIt00 == iIo00) CYCLE
               NStringIt( 2) = iIt00
               IAddIt00 = 1 + IArcWgtOcc(iIt00,  2, nIt)
            DO  iIt01 =  1, iIt00 - 1
               IF (iIt01 == iIo00) CYCLE
               NStringIt( 1) =  iIt01
               IAddIt01 = IAddIt00 + IArcWgtOcc(iIt01,  1, nIt)
               IAddIt = IAddIt01
               COIN = .FALSE.
               CALL CCLib_MergeStrings(NStringIo, NStringIt, NStringIn, nIo, nIt, ISgnIn, COIN)
               IF (COIN) CYCLE
               CALL CCLib_StringAddress(NStringIn, nIo+nIt, "Occ", IAddIn)
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
            END DO  ! T ext. holes
            END DO
!
         END DO  ! V ext. particles
         END DO
!
      END DO  ! V ext. holes
      DEALLOCATE(U)
!
      END SUBROUTINE
