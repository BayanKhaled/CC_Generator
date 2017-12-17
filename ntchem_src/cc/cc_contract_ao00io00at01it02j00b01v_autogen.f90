      SUBROUTINE  CC_Contract_Ao00Io00At01It02J00B01V_AutoGen &
      &   (V, T, W, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
!
!     o This is an automatically generated program
!
!     Contract intermediate V(C,K,B,J,Ao,Io) to T(D,L)
!     to generate a new intermediate W(C,K,An,In)
!
      USE CC_Module, ONLY : NOc, NVr
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
!
      LOGICAL:: COIN
      REAL(8):: V(mCK,mB*mJ,mAo,mIo), T(mD,mL), W(mCK,mAn,mIn), F(mB,mJ), Alpha
      INTEGER:: NstringB(nB), NstringJ(nJ), NstringD(nB+nAt), NstringL(nJ+nIt)
      INTEGER:: NStringAt(nAt), NStringIt(nIt), NStringAo(nAo), NStringIo(nIo), NStringAn(nAo+nAt), NStringIn(nIo+nIt)

      REAL(8), PARAMETER :: Zero = 0.0D+00, One = 1.0D+00


      DO iIt00 =  2, NOc   ! loop over external hole(s) in T-vertex
         NStringIt( 2) = iIt00
      DO  iIt01 =  1, iIt00 - 1
         NStringIt( 1) =  iIt01


         DO iAt00 =  1, NVr   ! loop over external particle(s) in T-vertex
            NStringAt( 1) = iAt00

            CALL CCLib_DClear(F, mB*mJ)
               IAddJ = 1

               CALL CCLib_StringAddress(NStringIt, nIt, "Occ", IAddIt)
               ISgnL = 1
               IAddL = IAddIt


               DO iB00 =  1, NVr   ! loop over fixed particle(s)
                  NStringB( 1) = iB00
                  COIN = .FALSE.
                  CALL CCLib_MergeStrings(NStringB, NStringAt, NStringD, nB, nAt, ISgnD, COIN)
                  IF (COIN) CYCLE
                  CALL CCLib_StringAddress(NStringD, nB+nAt, "Vir", IAddD)
                  CALL CCLib_StringAddress(NStringB, nB, "Vir", IAddB)
                  F(IAddB,IAddJ) = DBLE(ISgnL * ISgnD) * T(IAddD,IAddL)


               END DO  ! Fixed particles
               IAddIo = 1
               CALL CCLib_StringAddress(NStringIt, nIt, "Occ", IAddIt)
               ISgnIn = 1
               IAddIn = IAddIt
                  IAddAo = 1
                  CALL CCLib_StringAddress(NStringAt, nAt, "Vir", IAddAt)
                  ISgnAn = 1
                  IAddAn = IAddAt
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DGEMV("N", mCK, mB*mJ, Alpha, V(1,1,IAddAo,IAddIo), mCK, F, 1, One, W(1,IAddAn,IAddIn), 1)


         END DO  ! T ext. holes
         END DO


      END DO  ! T ext. particles
!
      END SUBROUTINE
