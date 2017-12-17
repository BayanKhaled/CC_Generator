      SUBROUTINE SOCC_ReadAmp(NSize, T, AmpName, Levl)
!
      USE CC_Module, ONLY : Name
!
!     Read T amplitudes from disc
!     Levl ... Level of T excitation (1, 2, ...)
!
      IMPLICIT NONE
!
      CHARACTER(*), INTENT(IN) :: AmpName
      INTEGER, INTENT(IN) :: NSize, Levl
      COMPLEX(8), INTENT(OUT) :: T(*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      INTEGER :: N
!
      CALL CCLib_Int2Char(Levl, c1, "0")
!YA012012      WRITE(*, '(" ..... Loading ", $)')
!YA012012      WRITE(*, *) TRIM(AmpName)//TRIM(c1)//" amplitude"
!
      FBuf = TRIM(Name)//"."//TRIM(AmpName)//TRIM(c1)
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ERR=9000, FORM='UNFORMATTED')
      GO TO 9100
 9000 STOP 'Error: in opening amplitude file in -SOCC_ReadAmp-'
 9100 CONTINUE
      REWIND(IO1)
      READ(IO1) (T(N), N = 1, NSize)
      CLOSE(IO1)
!
      END SUBROUTINE
