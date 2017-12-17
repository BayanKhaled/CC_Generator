      SUBROUTINE CC_CC2_T2_AutoGen
!
!     o This is an automatically generated program
!
      USE CC_Module, ONLY : NMax, NOc, NVr
      USE CC_Constant_Module, ONLY : One, Half
!
      IMPLICIT NONE
!
      REAL(8), ALLOCATABLE :: V2(:), V3(:), V4(:), V3_(:), V4_(:)
      REAL(8), ALLOCATABLE :: V1(:), T(:), G(:), G_(:)
      REAL(8) :: WgtFac
      INTEGER :: NumDig
      INTEGER :: NPwr
      INTEGER :: LevelT
      INTEGER :: NSizeT
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
      INTEGER :: Mwp(  13)
      INTEGER :: CCLib_NComb
!
!     o Weight and phase factors
!
      Mwp(   1)=    1
      Mwp(   2)=   -1
      Mwp(   3)=   -2
      Mwp(   4)=    1
      Mwp(   5)=    1
      Mwp(   6)=    2
      Mwp(   7)=   -2
      Mwp(   8)=   -2
      Mwp(   9)=    3
      Mwp(  10)=   -1
      Mwp(  11)=   -1
      Mwp(  12)=    1
      Mwp(  13)=   -1
!
!     ----- CC( 2)-T( 2) equation -----
!
!
!     o Prepare arc weight arrays
!
      CALL CCLib_InitArcWgt

!----- A new T1 vertex   0 found.
!----- A new T1 vertex 110 found.
!----- A new T1 vertex 111 found.
!----- A new T1 vertex 210 found.
!----- A new T1 vertex 211 found.

!----- Initialize V1 of size CCLib_NComb(NOc,  2)*CCLib_NComb(NVr,  2)                                                                                                                                                               
      NSizeV1 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  2)
      ALLOCATE( V1(NSizeV1))
      CALL CCLib_DClear( V1, NSizeV1)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
      NumDig = 0

!
!     --- Processing T1(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   1
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  13)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
!----- V1 += G( 13) with Mwp = 1
      CALL DAXPY( NSizeV1, One, G, 1, V1, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(110) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  2)                                                                                                                   
      NSizeV2 = CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  2)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
 !----- A new T2 vertex   0 found.
 !----- A new T2 vertex 110 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NOc * NVr * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   2
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  10)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G( 10) with Mwp =-1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(110) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,                 2) * CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  2)                                                                                                    
      NSizeV3 = CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  2)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
 !----- A new T3 vertex   0 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NOc, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   3
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   5)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(  5) with Mwp =-2
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
      nI =   2
      nB =   0
      nJ =   1
      nC =   0
      nK =   1
      nD =   0
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
      CALL CC_ReArrange_B00J01D00L02X_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)


!----- V2 += T2(110)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  2
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  0
      nK  =  1
      nD  =  1
      nL  =  1
      nAn =  1
      nIn =  2
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io02At01It00J01B00V_AutoGen( &
      &   V3_, T, V2, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(110)*V2
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  1
      nIo =  2
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  0
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  2
      nIn =  2
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao01Io02At01It00J01B00S_AutoGen( &
      &   V2, T, V1, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(111) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  2)*CCLib_NComb(NOc,  1)                                                                                                                   
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  1)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
 !----- A new T2 vertex   0 found.
 !----- A new T2 vertex 110 found.
 !----- A new T2 vertex 111 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NVr * CCLib_NComb(NVr, 2) * NOc
!
      NumDig = NumDig + 1  ! NumDig =   4
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   9)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(  9) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(110) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,                 1) * CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  1)                                                                                                    
      NSizeV3 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  1)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
 !----- A new T3 vertex   0 found.
 !----- A new T3 vertex 110 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = NVr * NOc * NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   5
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   6)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(  6) with Mwp = 1
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!
!     --- Processing T3(110) ---
!
!----- Initialize V4 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  1)                                                                                                                   
      NSizeV4 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  2) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1)
      ALLOCATE( V4(NSizeV4))
      CALL CCLib_DClear( V4, NSizeV4)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
 !----- A new T4 vertex   0 found.
!
!     --- Processing T4(  0) ---
!
      NSizeG = CCLib_NComb(NOc, 2) * NVr * NOc
!
      NumDig = NumDig + 1  ! NumDig =   6
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   8)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
!----- V4 += G(  8) with Mwp = 2
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
      nI =   1
      nB =   0
      nJ =   1
      nC =   1
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
      NSizeV4_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V4_(NSizeV4_))
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
      CALL CC_ReArrange_B00J01D01L02X_AutoGen( &
      &   V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V4)


!----- V3 += T3(110)*V4
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  1
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  1
      nK  =  1
      nD  =  1
      nL  =  1
      nAn =  1
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io01At01It00J01B00V_AutoGen( &
      &   V4_, T, V3, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
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
      nA =   1
      nI =   1
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
      CALL CC_ReArrange_B00J01D01L01X_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)


!----- V2 += T2(110)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  1
      nIo =  1
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  1
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  2
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao01Io01At01It00J01B00V_AutoGen( &
      &   V3_, T, V2, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(111) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,  2)*CCLib_NComb(NOc,                 0) * CCLib_NComb(NVr,  2)*CCLib_NComb(NOc,  0)                                                                                                    
      NSizeV3 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  0)
      ALLOCATE( V3(NSizeV3))
      CALL CCLib_DClear( V3, NSizeV3)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
 !----- A new T3 vertex   0 found.
 !----- A new T3 vertex 110 found.
!
!     --- Processing T3(  0) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NVr, 2)
!
      NumDig = NumDig + 1  ! NumDig =   7
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   4)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(  4) with Mwp =-2
      CALL DAXPY( NSizeV3, One, G, 1, V3, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV3, V3, 'V3')
      DEALLOCATE( V3)
!
!     --- Processing T3(110) ---
!
!----- Initialize V4 of size CCLib_NComb(NVr,  2)*CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  0)                                                                                                                   
      NSizeV4 = CCLib_NComb(NVr,  2) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0)
      ALLOCATE( V4(NSizeV4))
      CALL CCLib_DClear( V4, NSizeV4)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
 !----- A new T4 vertex   0 found.
 !----- A new T4 vertex 110 found.
!
!     --- Processing T4(  0) ---
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
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
!----- V4 += G(  7) with Mwp =-2
      CALL DAXPY( NSizeV4, One, G, 1, V4, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
!
!     --- Processing T4(110) ---
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
!
      NumDig = NumDig + 1  ! NumDig =   9
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,  11)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
!-----Define G_ size
      nA =   0
      nI =   0
      nB =   0
      nJ =   1
      nC =   2
      nK =   1
      mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)
      mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      NSizeG_ = mAI * mBJ * mCK
      ALLOCATE( G_(NSizeG_))
!----- Re-arrange G array
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
      nC =   2
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
      CALL CC_ReArrange_B00J01D02L02N_AutoGen( &
      &   G, G_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( G)
!----- V4 += T4(110)*I5
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  0
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  2
      nK  =  1
      nD  =  1
      nL  =  1
      nAn =  1
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io00At01It00J01B00V_AutoGen( &
      &   G_, T, V4, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( G_)
      CALL CC_WriteV( NSizeV4, V4, 'V4')
      DEALLOCATE( V4)
!----- Re-arrange V4 array
!
!     nA, nI ... # of ext. lines of intermediate
!     nB, nJ ... Summation indices
!     nC, nK ... Free int. indices
!     nD, nL ... nB+nC and nJ+nK
!
      nA =   1
      nI =   0
      nB =   0
      nJ =   1
      nC =   2
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
      NSizeV4_ = mAI * mJ * mB * mK * mC
      ALLOCATE(V4_(NSizeV4_))
      ALLOCATE( V4(NSizeV4))
      CALL CC_ReadV( NSizeV4, V4, 'V4')
      CALL CC_ReArrange_B00J01D02L01X_AutoGen( &
      &   V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V4)


!----- V3 += T3(110)*V4
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  1
      nIo =  0
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  2
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  2
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao01Io00At01It00J01B00V_AutoGen( &
      &   V4_, T, V3, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
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
      nA =   2
      nI =   0
      nB =   1
      nJ =   0
      nC =   1
      nK =   0
      nD =   2
      nL =   0
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
      CALL CC_ReArrange_B01J00D02L00X_AutoGen( &
      &   V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)
      DEALLOCATE( V3)


!----- V2 += T2(111)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  2
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  1
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  2
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao02Io00At00It01J00B01V_AutoGen( &
      &   V3_, T, V2, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(111)*V2
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  2
      nIo =  1
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  0
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  2
      nIn =  2
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao02Io01At00It01J00B01S_AutoGen( &
      &   V2, T, V1, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(210) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  1)                                                                                                                   
      NSizeV2 = CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1) * CCLib_NComb(NVr,  0) * CCLib_NComb(NOc,  1)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
 !----- A new T2 vertex   0 found.
 !----- A new T2 vertex 111 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NOc * NOc
!
      NumDig = NumDig + 1  ! NumDig =  10
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   2)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(  2) with Mwp =-1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(111) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,                 1) * CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  0)                                                                                                    
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
      NumDig = NumDig + 1  ! NumDig =  11
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   3)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(  3) with Mwp =-1
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


!----- V2 += T2(111)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  0
      nAt =  0
      nIt =  1
      nB  =  1
      nJ  =  0
      nC  =  0
      nK  =  1
      nD  =  1
      nL  =  1
      nAn =  0
      nIn =  1
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io00At00It01J00B01V_AutoGen( &
      &   V3_, T, V2, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(210)*V2
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   2
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  1
      nAt =  2
      nIt =  1
      nB  =  0
      nJ  =  1
      nC  =  0
      nK  =  0
      nD  =  2
      nL  =  2
      nAn =  2
      nIn =  2
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io01At02It01J01B00S_AutoGen( &
      &   V2, T, V1, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)
!
!     --- Processing T1(211) ---
!
!----- Initialize V2 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,  0)                                                                                                                   
      NSizeV2 = CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0) * CCLib_NComb(NVr,  1) * CCLib_NComb(NOc,  0)
      ALLOCATE( V2(NSizeV2))
      CALL CCLib_DClear( V2, NSizeV2)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
 !----- A new T2 vertex   0 found.
 !----- A new T2 vertex 110 found.
!
!     --- Processing T2(  0) ---
!
      NSizeG = NVr * NVr
!
      NumDig = NumDig + 1  ! NumDig =  12
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   1)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
!----- V2 += G(  1) with Mwp = 1
      CALL DAXPY( NSizeV2, One, G, 1, V2, 1)
      DEALLOCATE( G)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!
!     --- Processing T2(110) ---
!
!----- Initialize V3 of size CCLib_NComb(NVr,  1)*CCLib_NComb(NOc,                 1) * CCLib_NComb(NVr,  0)*CCLib_NComb(NOc,  0)                                                                                                    
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
      NumDig = NumDig + 1  ! NumDig =  13
!
      ALLOCATE(G(NSizeG))
      CALL CC_ReadMOint(G, NSizeG,   3)
      NPwr = IABS(Mwp(NumDig)) - 1
      WgtFac = Half ** NPwr
      IF (Mwp(NumDig) < 0) WgtFac = -WgtFac
      CALL DSCAL(NSizeG, WgtFac, G, 1)
      ALLOCATE( V3(NSizeV3))
      CALL CC_ReadV( NSizeV3, V3, 'V3')
!----- V3 += G(  3) with Mwp =-1
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


!----- V2 += T2(110)*V3
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   1
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  0
      nIo =  0
      nAt =  1
      nIt =  0
      nB  =  0
      nJ  =  1
      nC  =  1
      nK  =  0
      nD  =  1
      nL  =  1
      nAn =  1
      nIn =  0
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao00Io00At01It00J01B00V_AutoGen( &
      &   V3_, T, V2, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V3_)
      CALL CC_WriteV( NSizeV2, V2, 'V2')
      DEALLOCATE( V2)
!----- V1 += T1(211)*V2
      ALLOCATE( V1(NSizeV1))
      CALL CC_ReadV( NSizeV1, V1, 'V1')
      ALLOCATE( V2(NSizeV2))
      CALL CC_ReadV( NSizeV2, V2, 'V2')
      LevelT =   2
      NSizeT = CCLib_NComb(NOc, LevelT) * CCLib_NComb(NVr, LevelT)
      ALLOCATE( T(NSizeT))
      CALL CC_ReadAmp(NSizeT,  T, 'T', LevelT)
      nAo =  1
      nIo =  0
      nAt =  1
      nIt =  2
      nB  =  1
      nJ  =  0
      nC  =  0
      nK  =  0
      nD  =  2
      nL  =  2
      nAn =  2
      nIn =  2
      mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)
      mB  = CCLib_NComb(NVr, nB)
      mJ  = CCLib_NComb(NOc, nJ)
      mD  = CCLib_NComb(NVr, nD)
      mL  = CCLib_NComb(NOc, nL)
      mAo = CCLib_NComb(NVr, nAo)
      mIo = CCLib_NComb(NOc, nIo)
      mAn = CCLib_NComb(NVr, nAn)
      mIn = CCLib_NComb(NOc, nIn)
      CALL CC_Contract_Ao01Io00At01It02J00B01S_AutoGen( &
      &   V2, T, V1, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)
      DEALLOCATE( T)
      DEALLOCATE( V2)
      CALL CC_WriteV( NSizeV1, V1, 'V1')
      DEALLOCATE( V1)

!-----    13 diagrams have been processed
!
!     ----- End of residue calculation -----
!
      END SUBROUTINE
