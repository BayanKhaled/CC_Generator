      SUBROUTINE  CC_Contract_Ao01Io02At01It00J02B01S_AutoGen &
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


         DO iAt00 =  1, NVr   ! loop over external particle(s) in T-vertex
            NStringAt( 1) = iAt00

            CALL CCLib_DClear(F, mB*mJ)


            DO iJ00 =  2, NOc    ! loop over fixed hole(s)
               NStringJ( 2) = iJ00
            DO  iJ01 =  1,iJ00 - 1
               NStringJ( 1) =  iJ01
               CALL CCLib_StringAddress(NStringJ, nJ, "Occ", IAddJ)
               ISgnL = 1
               IAddL = IAddJ


               DO iB00 =  1, NVr   ! loop over fixed particle(s)
                  NStringB( 1) = iB00
                  COIN = .FALSE.
                  CALL CCLib_MergeStrings(NStringB, NStringAt, NStringD, nB, nAt, ISgnD, COIN)
                  IF (COIN) CYCLE
                  CALL CCLib_StringAddress(NStringD, nB+nAt, "Vir", IAddD)
                  CALL CCLib_StringAddress(NStringB, nB, "Vir", IAddB)
                  F(IAddB,IAddJ) = DBLE(ISgnL * ISgnD) * T(IAddD,IAddL)


               END DO  ! Fixed particles


            END DO  ! Fixed holes
            END DO


            DO iIo00 =  2, NOc    ! loop over external hole(s) in V
               NStringIo( 2) = iIo00
            DO  iIo01 =  1, iIo00 - 1
               NStringIo( 1) =  iIo01
               CALL CCLib_StringAddress(NStringIo, nIo,"Occ", IAddIo)
               ISgnIn = 1
               IAddIn = IAddIo


               DO iAo00 =  1, NVr   ! loop over external particle(s) in V
                  NStringAo( 1) = iAo00
                  COIN = .FALSE.
                  CALL CCLib_MergeStrings(NStringAo, NStringAt, NStringAn, nAo, nAt, ISgnAn, COIN)
                  IF (COIN) CYCLE
                  CALL CCLib_StringAddress(NStringAn, nAo+nAt, "Vir", IAddAn)
                  CALL CCLib_StringAddress(NStringAo, nAo, "Vir", IAddAo)
                  Alpha = DBLE(ISgnAn * ISgnIn)
                  CALL DGEMV("N", mCK, mB*mJ, Alpha, V(1,1,IAddAo,IAddIo), mCK, F, 1, One, W(1,IAddAn,IAddIn), 1)


               END DO  ! V ext. particles


            END DO  ! V ext. holes
            END DO


      END DO  ! T ext. particles
!
      END SUBROUTINE
