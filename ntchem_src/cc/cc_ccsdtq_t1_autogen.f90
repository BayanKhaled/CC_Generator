      SUBROUTINE CC_CCSDTQ_T1_AutoGen
!
!     o This is an automatically generated program
!
      USE CC_Module, ONLY : NMax, NOc, NVr
      USE CC_Constant_Module, ONLY : One, Half
!
      IMPLICIT NONE
!
      REAL(8), ALLOCATABLE :: V2(:), V3(:), V4(:), V3_(:), V4_(:)
      REAL(8), ALLOCATABLE :: V1(:), T(:), T_(:), G(:), G_(:)
      REAL(8) :: WgtFac
      INTEGER :: NumDig
      INTEGER :: NPwr
      INTEGER :: LevelT
      INTEGER :: LevelTOcc
      INTEGER :: LevelTVir
      INTEGER :: NSizeT
      INTEGER :: NSizeT_
      INTEGER :: NSizeG
      INTEGER :: NSizeG_
      INTEGER :: NSizeV1
      INTEGER :: NSizeV2
      INTEGER :: NSizeV3
      INTEGER :: NSizeV3_
      INTEGER :: NSizeV4
      INTEGER :: NSizeV4_
      INTEGER :: nA
      INTEGER :: nI
      INTEGER :: nB
      INTEGER :: nJ
      INTEGER :: nC
      INTEGER :: nK
      INTEGER :: nD
      INTEGER :: nL
      INTEGER :: mAI
      INTEGER :: mB
      INTEGER :: mJ
      INTEGER :: mD
      INTEGER :: mL
      INTEGER :: mC
      INTEGER :: mK
      INTEGER :: nAo
      INTEGER :: nIo
      INTEGER :: nAt
      INTEGER :: nIt
      INTEGER :: nAn
      INTEGER :: nIn
      INTEGER :: mBJ
      INTEGER :: mCK
      INTEGER :: mAo
      INTEGER :: mIo
      INTEGER :: mAn
      INTEGER :: mIn
      INTEGER :: mAt
      INTEGER :: mIt
      INTEGER :: Mwp(  15)
      INTEGER :: CCLib_NComb
!
!     o Weight and phase factors
!
      Mwp(   1)=    1   !  0 0 0/ 0 0 0/ 0 0 0/ 0 0 0 12
      Mwp(   2)=   -1   !  1 1 0/ 0 0 0/ 0 0 0/ 0 0 0  2
      Mwp(   3)=   -1   !  1 1 0/ 1 1 1/ 0 0 0/ 0 0 0  3
      Mwp(   4)=    1   !  1 1 0/ 1 2 1/ 0 0 0/ 0 0 0  8
      Mwp(   5)=    1   !  1 1 0/ 2 3 2/ 0 0 0/ 0 0 0 11
      Mwp(   6)=    1   !  1 1 1/ 0 0 0/ 0 0 0/ 0 0 0  1
      Mwp(   7)=   -1   !  1 1 1/ 1 1 0/ 1 2 1/ 0 0 0 11
      Mwp(   8)=   -1   !  1 1 1/ 1 2 1/ 0 0 0/ 0 0 0  7
      Mwp(   9)=   -1   !  1 2 1/ 0 0 0/ 0 0 0/ 0 0 0  6
      Mwp(  10)=    1   !  2 2 1/ 0 0 0/ 0 0 0/ 0 0 0  3
      Mwp(  11)=    1   !  2 2 1/ 1 2 1/ 0 0 0/ 0 0 0 11
      Mwp(  12)=   -1   !  2 3 1/ 0 0 0/ 0 0 0/ 0 0 0  8
      Mwp(  13)=   -1   !  2 3 1/ 1 1 1/ 0 0 0/ 0 0 0 11
      Mwp(  14)=    1   !  2 3 2/ 0 0 0/ 0 0 0/ 0 0 0  7
      Mwp(  15)=    1   !  3 4 2/ 0 0 0/ 0 0 0/ 0 0 0 11
!
!     ----- CC( 4)-T( 1) equation -----
!
!
!     o Prepare arc weight arrays
!
      CALL CCLib_InitArcWgt

!----- A new T1 vertex   0 found.
!----- A new T1 vertex 110 found.
!----- A new T1 vertex 111 found.
!----- A new T1 vertex 121 found.
!----- A new T1 vertex 221 found.
!----- A new T1 vertex 231 found.
!----- A new T1 vertex 232 found.
!----- A new T1 vertex 342 found.

!----- Initialize V1 of size CCLib_NComb(NOc,  1)*CCLib_NComb(NVr,  1)                                                           
      NSizeV1 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1)
      ALLOCATE( V1(NSizeV1))
      CALL CCLib_DClear( V1, NSizeV1)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
      NumDig = 0

!
!     --- Processing T1(  0) ---
!
      NSizeG = NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   1
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  12)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
!----- V1 += G(          12) with Mwp = 1
      CALL DAXPY( NSizeV1, One, G, 1, V1, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(110) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   1)           
      NSizeV2 = CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!----- A new T2 vertex 111 found.
!----- A new T2 vertex 121 found.
!----- A new T2 vertex 232 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NOc * NOc
!
      NumDig = NumDig + 1  ! NumDig =   2
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   2)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           2) with Mwp =-1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(111) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   3
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   3)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(           3) with Mwp =-1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   1
      nJ =   0
      nC =   0
      nK =   1
      nD =   1
      nL =   1
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B01J00D01L01N_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  0
      nC =  0
      nK =  1
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
!----- V2 += T2(111)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  0
      nK  =  1
      nAn =  0
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At00It01V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(121) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   1)           
      NSizeV3 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NOc, 2) * NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   4
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   8)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(           8) with Mwp = 1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   1
      nB =   1
      nJ =   1
      nC =   0
      nK =   1
      nD =   1
      nL =   2
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B01J01D01L02X_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  0
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
!----- V2 += T2(121)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  1
      nAt =  0
      nIt =  0
      nB  =  1
      nJ  =  1
      nC  =  0
      nK  =  1
      nAn =  0
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io01At00It00V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(232) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   5
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(          11) with Mwp = 1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   2
      nJ =   1
      nC =   0
      nK =   1
      nD =   2
      nL =   2
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B02J01D02L02N_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   2
      LevelTOcc =   2
      LevelTVir =   2
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  2
      nJ =  1
      nC =  0
      nK =  1
      nD =  2
      nL =  2
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B00J01D02L02N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
!----- V2 += T2(232)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  2
      nJ  =  1
      nC  =  0
      nK  =  1
      nAn =  0
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At00It01V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(110)*V2
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  0
      nJ =  1
      nC =  1
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B01J00D01L01N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  1
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io01At01It00S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(111) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   0) * CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   0)           
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!----- A new T2 vertex 110 found.
!----- A new T2 vertex 121 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NVr * NVr
!
      NumDig = NumDig + 1  ! NumDig =   6
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   1)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           1) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(110) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex 121 found.
!
!     --- Processing T3(121) ---
!
!----- Initialize V4 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV4 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V4(NSizeV4))
      CALL CCLib_DClear( V4, NSizeV4)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
!----- A new T4 vertex   0 found.
!
!     --- Processing T4(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   7
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
!----- V4 += G(          11) with Mwp =-1
      CALL DAXPY( NSizeV4, One, G, 1, V4, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
!----- Re-arrange V4 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   1
      nJ =   1
      nC =   1
      nK =   1
      nD =   2
      nL =   2
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV4_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V4_(NSizeV4_))
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
      CALL CC_ReArrange_B01J01D02L02N_AutoGen( &
      &   V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V4)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  0
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  0
      nB  =  1
      nJ  =  1
      nC  =  1
      nK  =  1
      nAn =  0
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At00It00V_AutoGen( &
      &   V4_, T_, V3, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V4_)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   0
      nJ =   1
      nC =   1
      nK =   0
      nD =   1
      nL =   1
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B00J01D01L01N_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  0
      nJ =  1
      nC =  1
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B01J00D01L01N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
!----- V2 += T2(110)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  1
      nK  =  0
      nAn =  1
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At01It00V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(121) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * NOc * NVr
!
      NumDig = NumDig + 1  ! NumDig =   8
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   7)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(           7) with Mwp =-1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   1
      nI =   0
      nB =   1
      nJ =   1
      nC =   1
      nK =   0
      nD =   2
      nL =   1
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B01J01D02L01X_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  0
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
!----- V2 += T2(121)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  1
      nIo =  0
      nAt =  0
      nIt =  0
      nB  =  1
      nJ  =  1
      nC  =  1
      nK  =  0
      nAn =  1
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao01Io00At00It00V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(111)*V2
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  0
      nC =  0
      nK =  1
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  1
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao01Io00At00It01S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(121) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   1)           
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NVr * NOc * NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   9
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   6)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           6) with Mwp =-1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(121)*V2
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  0
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  1
      nIo =  1
      nAt =  0
      nIt =  0
      nB  =  1
      nJ  =  1
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao01Io01At00It00S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(221) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!----- A new T2 vertex 121 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =  10
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   3)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           3) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(121) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =  11
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(          11) with Mwp = 1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   1
      nJ =   1
      nC =   1
      nK =   1
      nD =   2
      nL =   2
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B01J01D02L02N_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  0
      nK =  0
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
!----- V2 += T2(121)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  0
      nB  =  1
      nJ  =  1
      nC  =  1
      nK  =  1
      nAn =  0
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At00It00V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(221)*V2
      LevelT =   2
      LevelTOcc =   2
      LevelTVir =   2
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  1
      nC =  1
      nK =  1
      nD =  2
      nL =  2
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B01J01D02L02N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  1
      nIt =  1
      nB  =  1
      nJ  =  1
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At01It01S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(231) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   1)           
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!----- A new T2 vertex 111 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = CCLib_NComb(NOc, 2) * NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =  12
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   8)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           8) with Mwp =-1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(111) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV3 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =  13
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(          11) with Mwp =-1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!----- Re-arrange V3 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   0
      nI =   0
      nB =   1
      nJ =   0
      nC =   1
      nK =   2
      nD =   2
      nL =   2
      mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      NSizeV3_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V3_(NSizeV3_))
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      CALL CC_ReArrange_B01J00D02L02N_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)
!
      LevelT =   1
      LevelTOcc =   1
      LevelTVir =   1
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  0
      nC =  0
      nK =  1
      nD =  1
      nL =  1
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL DCOPY(NSizeT, T, 1, T_, 1)
      DEALLOCATE( T)
!----- V2 += T2(111)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  1
      nK  =  2
      nAn =  0
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At00It01V_AutoGen( &
      &   V3_, T_, V2, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(231)*V2
      LevelT =   2
      LevelTOcc =   2
      LevelTVir =   2
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  1
      nJ =  2
      nC =  1
      nK =  0
      nD =  2
      nL =  2
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B01J00D02L02N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  1
      nAt =  1
      nIt =  0
      nB  =  1
      nJ  =  2
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io01At01It00S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(232) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   1) * CCLib_NComb(NVr,   1)*CCLib_NComb(NOc,   0)           
      NSizeV2 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * NOc * NVr
!
      NumDig = NumDig + 1  ! NumDig =  14
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   7)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(           7) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(232)*V2
      LevelT =   2
      LevelTOcc =   2
      LevelTVir =   2
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  2
      nJ =  1
      nC =  0
      nK =  1
      nD =  2
      nL =  2
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B00J01D02L02N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  1
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  2
      nJ  =  1
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao01Io00At00It01S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(342) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,   2)*CCLib_NComb(NOc,   2) * CCLib_NComb(NVr,   0)*CCLib_NComb(NOc,   0)           
      NSizeV2 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  0)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- A new T2 vertex   0 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =  15
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(          11) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(342)*V2
      LevelT =   3
      LevelTOcc =   3
      LevelTVir =   3
      NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nB =  2
      nJ =  2
      nC =  1
      nK =  1
      nD =  3
      nL =  3
      nA =  0
      nI =  0
      mB = CCLib_NComb(NVr, nB)
      mJ = CCLib_NComb(NOc, nJ)
      mC = CCLib_NComb(NVr, nC)
      mK = CCLib_NComb(NOc, nK)
      mD = CCLib_NComb(NVr, nD)
      mL = CCLib_NComb(NOc, nL)
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      NSizeT_ = CCLib_NComb(NOc, nJ) * CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nK) * CCLib_NComb(NVr, nC)
      ALLOCATE( T_(NSizeT_))
      CALL CC_ReArrange_B01J01D03L03N_AutoGen( &
      &   T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)
      DEALLOCATE( T)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      nAo =  0
      nIo =  0
      nAt =  1
      nIt =  1
      nB  =  2
      nJ  =  2
      nC  =  0
      nK  =  0
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAt = CCLib_NComb(NVr, nAt)
      mIt = CCLib_NComb(NOc, nIt)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_ContractGEMM_Ao00Io00At01It01S_AutoGen( &
      &   V2, T_, V1, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)
      DEALLOCATE( T_)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)

!-----    15 diagrams have been processed
!
!     ----- End of residue calculation -----
!
      END SUBROUTINE
