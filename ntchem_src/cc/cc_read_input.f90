      SUBROUTINE CC_Read_Input
!
      USE CC_Module, ONLY : Name, NMax, NBF, NMOA, NMOB, NOcA, NOcB, NFrzOA, NFrzOB, NFrzVA, NFrzVB, &
     &   MaxIter, UHF, DoDIIS, MaxDIIS, T1Trn, Bruck, NoT1, CC2, CalcMP, Guess, ThrEne, ThrGrd, &
     &   DoTran, IPrint, &
!ActiveCC_Bgn
     &   ASCC, NOccModelA, NVirModelA, NOccModelB, NVirModelB
!ActiveCC_End
!
!     o Read the CC input
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: IO1 = 5
      CHARACTER(LEN=4) :: Symm
      INTEGER :: NCorePerIO, NCorePerScaLAPACK
!
      NAMELIST /Control/ Name, Symm, NCorePerIO, NCorePerScaLAPACK
      NAMELIST /CC/ NMax, NBF, NMOA, NMOB, NFrzOA, NFrzOB, NFrzVA, NFrzVB, NOcA, NOcB, &
     &   MaxIter, UHF, MaxDIIS, T1Trn, Bruck, NoT1, CC2, CalcMP, Guess, ThrEne, ThrGrd, &
     &   DoTran, IPrint, &
!ActiveCC_Bgn
     &   ASCC, NOccModelA, NVirModelA, NOccModelB, NVirModelB
!ActiveCC_End
!
!     o Open INPUT file
!
      OPEN(UNIT=IO1, FILE='INPUT', STATUS='OLD', ACCESS='SEQUENTIAL', ERR=100)
      GO TO 200
  100 CONTINUE
      STOP 'Error: in opening INPUT file'
  200 CONTINUE
!
!     o Initialize
!
      Name = 'ntchem'
      Guess = 'INIT'   ! 'INIT' or 'READ'
      UHF = .FALSE.
      T1Trn = .FALSE.
      Bruck = .FALSE.
      NoT1 = .FALSE.
      CC2 = .FALSE.
      CalcMP = .FALSE.
      DoTran = .TRUE.
!ActiveCC_Bgn
      ASCC = ''   ! 'HM' or 'LR'
      NOccModelA = 0
      NVirModelA = 0
      NOccModelB = 0
      NVirModelB = 0
!ActiveCC_End
      NMax = 2
      NBF = 0
      NMOA = 0
      NMOB = 0
      NOcA = 0
      NOcB = 0
      NFrzOA = 0
      NFrzOB = 0
      NFrzVA = 0
      NFrzVB = 0
      MaxDIIS = 6   ! No DIIS acceleration if MaxDIIS = 1
      MaxIter = 100
      ThrGrd = 1.0D-07
      ThrEne = 1.0D-07
      IPrint = 0
!
      REWIND(IO1)
      READ(IO1, Control, END=7000, ERR=7000)
      GO TO 7010
 7000 CONTINUE
 7010 CONTINUE
!
      REWIND(IO1)
      READ(IO1, CC, END=9000, ERR=9000)
      GO TO 9010
 9000 STOP 'Error: Check NAMELIST CC'
 9010 CONTINUE
!
      CLOSE(IO1)
!
      CALL Util_TransChar(Guess, 4)
!
!     o Check consistency
!
      IF (CC2 .AND. (NMax /= 2)) THEN
         WRITE(*, '("... NMax must be 2 for CC2")')
         GO TO 9000
      END IF
      DoDIIS = (MaxDIIS > 1)
!
      END SUBROUTINE
