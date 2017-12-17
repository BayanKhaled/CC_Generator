      SUBROUTINE CC_SetVar
!
      USE CC_Module, ONLY : NBF, UHF, NMO, NMOA, NMOB, NOcA, NOcB, NVrA, NVrB, &
     &   NFrzOA, NFrzOB, NFrzVA, NFrzVB, NFrzO, NFrzV, NOc, NVr, &
     &   NoT1, CC2, NMax, Name, WfnName, EneRef, Guess, ThrEne, ThrGrd, MaxDIIS, &
!ActiveCC_Bgn
     &   ASCC, NOccModelA, NVirModelA, NOccModelB, NVirModelB, &
     &   NFrzOAM, NFrzVAM, NFrzOBM, NFrzVBM
!ActiveCC_End
!
!     o Set CC variables
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: IO = 90
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: A2
      LOGICAL :: FileExists
      LOGICAL :: ROHF
      INTEGER :: NOcA0, NOcB0, NVrA0, NVrB0, NMO0, NRank
!
!     o Read SCF Info.
!
      FBuf = TRIM(Name)//".SCF_Info"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED')
      REWIND(IO)
      READ(IO, *) NBF, NMO0, NOcA0, NOcB0, UHF, ROHF
      CLOSE(IO)
!
      FBuf = TRIM(Name)//".TotEne"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='FORMATTED')
      REWIND(IO)
      READ(IO, '(F25.12)') EneRef
      CLOSE(IO)
!
      IF (ROHF) THEN
         WRITE(*, '("NYI: CC with ROHF orbitals")')
         STOP
      END IF
!
      NMOA = NMO0
      NMOB = NMO0
      NVrA0 = NMOA - NOcA0
      NVrB0 = NMOB - NOcB0
      NOcA = NOcA0 - NFrzOA
      NOcB = NOcB0 - NFrzOB
      NVrA = NVrA0 - NFrzVA
      NVrB = NVrB0 - NFrzVB
      NMOA = NOcA + NVrA
      NMOB = NOcB + NVrB
      NMO = NMOA + NMOB
      NOc = NOcA + NOcB
      NVr = NVrA + NVrB
      NFrzO = NFrzOA + NFrzOB
      NFrzV = NFrzVA + NFrzVB
!
!FrozenCore      NMOA = NMO
!FrozenCore      NMOB = NMO
!FrozenCore      NMO  = NMOA + NMOB
!FrozenCore      NVrA = NMOA - NOcA
!FrozenCore      NVrB = NMOB - NOcB
!FrozenCore      NOc  = NOcA + NOcB
!FrozenCore      NVr  = NVrA + NVrB
!
!     o Check if amplitude files exist (if Guess = READ)
!
      IF (TRIM(Guess) == 'READ') THEN
!
         DO NRank = 1, NMax
            CALL CCLib_Int2Char(NRank, A2, '0')
            FBuf = TRIM(Name)//".T"//A2
            INQUIRE(FILE=TRIM(FBuf), EXIST=FileExists)
            IF (.NOT. FileExists) THEN
               WRITE(*, '(" T", I0, " amplitude file not found.")') NRank
               Guess = 'INIT'
            END IF
         END DO
!
      END IF
!
!     o Punch out input variables
!
      WRITE(*, '(" ... CC input variables ...", /, &
     &   5X, "NBF =", I5, /, &
     &   5X, "NMO =", I5, X, "(NMOA =", I5, X, "NMOB =", I5, ")", /, &
     &   5X, "NOc =", I5, X, "(NOcA =", I5, X, "NOcB =", I5, ")", /, &
     &   5X, "NVr =", I5, X, "(NVrA =", I5, X, "NVrB =", I5, ")", /, &
     &   5X, "NFrzOA =", I5, /, &
     &   5X, "NFrzOB =", I5, /, &
     &   5X, "NFrzVA =", I5, /, &
     &   5X, "NFrzVB =", I5, /, &
     &   5X, "ThrGrd =", F20.12, /, &
     &   5X, "ThrEne =", F20.12, /, &
     &   5X, "MaxDIIS   =", I5, /, &
     &   5X, "Level of truncation: ", I2, /, &
     &   5X, "Reference Energy:", F20.12)') &
     &   NBF, NMO, NMOA, NMOB, NOc, NOcA, NOcB, NVr, NVrA, NVrB, NFrzOA, NFrzOB, NFrzVA, NFrzVB, &
     &   ThrGrd, ThrEne, MaxDIIS, NMax, EneRef
!
      WRITE(*, *) 'o NoT1 = ', NoT1
!
      SELECT CASE (NMax)
         CASE (1)
            WfnName = 'CCS'
         CASE (2)
            IF (CC2) THEN
               WfnName = 'CC2'
            ELSE IF (NoT1) THEN
               WfnName = 'CCD'
            ELSE
               WfnName = 'CCSD'
            END IF
         CASE (3)
            WfnName = 'CCSDT'
         CASE (4)
            WfnName = 'CCSDTQ'
         CASE DEFAULT
            WRITE(*, '("NYI: CCSDTQ5 or higher")')
            STOP
      END SELECT
!
      WRITE(*, '(/, " ... Wavefunction type is", $)')
      WRITE(*, *) TRIM(WfnName)
!ActiveCC_Bgn
      IF (ASCC == 'HM') THEN
         WRITE(*, '(" Turning on active-space hybrid CC: High-Model calculation")')
         NFrzOAM = (NOcA + NFrzOA) - NOccModelA
         NFrzOBM = (NOcB + NFrzOB) - NOccModelB
         NFrzVAM = (NVrA + NFrzVA) - NVirModelA
         NFrzVBM = (NVrA + NFrzVB) - NVirModelB
         WRITE(*, '("   NOcc in model space: ", I2, " Alpha / ", I2, " Beta")') NOccModelA, NOccModelB
         WRITE(*, '("   NVir in model space: ", I2, " Alpha / ", I2, " Beta")') NVirModelA, NVirModelB
      ELSE IF (ASCC == 'LR') THEN
         WRITE(*, '(" Turning on active-space hybrid CC: Low-Real calculation")')
         NOccModelA = NOcA
         NOccModelB = NOcB
         NVirModelA = NVrA
         NVirModelB = NVrB
         NFrzOAM = (NOcA + NFrzOA) - NOccModelA
         NFrzOBM = (NOcB + NFrzOB) - NOccModelB
         NFrzVAM = (NVrA + NFrzVA) - NVirModelA
         NFrzVBM = (NVrA + NFrzVB) - NVirModelB
         WRITE(*, '("   NOcc in model space: ", I2, " Alpha / ", I2, " Beta")') NOccModelA, NOccModelB
         WRITE(*, '("   NVir in model space: ", I2, " Alpha / ", I2, " Beta")') NVirModelA, NVirModelB
      ELSE
         NOccModelA = NOcA
         NOccModelB = NOcB
         NVirModelA = NVrA
         NVirModelB = NVrB
         NFrzOAM = (NOcA + NFrzOA) - NOccModelA
         NFrzOBM = (NOcB + NFrzOB) - NOccModelB
         NFrzVAM = (NVrA + NFrzVA) - NVirModelA
         NFrzVBM = (NVrA + NFrzVB) - NVirModelB
      END IF
!ActiveCC_End
!
      END SUBROUTINE
