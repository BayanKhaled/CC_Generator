      SUBROUTINE  CC_Contract_Ao00Io00At01It00J01B00V_AutoGen &
      &   (V, T, W, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
!
!     o This is an automatically generated program
!
!     Contract intermediate V(C,K,B,J,Ao,Io) to T(D,L)
!     to generate a new intermediate W(C,K,An,In)
!
      USE CC_Module, ONLY : NOc, NVr
      USE CC_Constant_Module, ONLY : One
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
!
      LOGICAL:: COIN
      REAL(8):: V(mCK,mB*mJ,mAo,mIo), T(mD,mL), W(mCK,mAn,mIn), F(mB,mJ), Alpha
      INTEGER:: NStringB(nB), NStringJ(nJ), NStringD(nB+nAt), NStringL(nJ+nIt)
      INTEGER:: NStringAt(nAt), NStringIt(nIt), NStringAo(nAo), NStringIo(nIo), NStringAn(nAo+nAt), NStringIn(nIo+nIt)



         DO iAt00 =  1, NVr   ! loop over external particle(s) in T-vertex
            NStringAt( 1) = iAt00

            CALL CCLib_DClear(F, mB*mJ)


            DO iJ00 =  1, NOc    ! loop over fixed hole(s)
               NStringJ( 1) = iJ00
               CALL CCLib_StringAddress(NStringJ, nJ, "Occ", IAddJ)
               ISgnL = 1
               IAddL = IAddJ
                  IAddB = 1

                  CALL CCLib_StringAddress(NStringAt, nAt, "Vir", IAddAt)
                  ISgnD = 1
                  IAddD = IAddAt
                  F(IAddB,IAddJ) = DBLE(ISgnL * ISgnD) * T(IAddD,IAddL)


            END DO  ! Fixed holes
               IAddIo = 1
               ISgnIn = 1
               IAddIn = 1
                  IAddAo = 1
                  CALL CCLib_StringAddress(NStringAt, nAt, "Vir", IAddAt)
                  ISgnAn = 1
                  IAddAn = IAddAt
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DGEMV("N", mCK, mB*mJ, Alpha, V(1,1,IAddAo,IAddIo), mCK, F, 1, One, W(1,IAddAn,IAddIn), 1)


      END DO  ! T ext. particles
!
      END SUBROUTINE
