      SUBROUTINE CC_Write_Info
!
      USE CC_Module, ONLY : Name, NMax, NoT1, CC2, WfnName, EneRef, ECorr, &
     &   NFrzOA, NFrzOB, NFrzVA, NFrzVB
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: IO = 99
      CHARACTER(LEN=255) :: FBuf
!
!     o Write CC information
!
      FBuf = TRIM(Name)//".CC_Info"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', ACCESS='SEQUENTIAL', FORM='FORMATTED')
      REWIND(IO)
      WRITE(IO, *) NMax, NoT1, CC2, WfnName, EneRef, ECorr
      WRITE(IO, *) NFrzOA, NFrzOB, NFrzVA, NFrzVB
      CLOSE(IO)
!
      END SUBROUTINE
