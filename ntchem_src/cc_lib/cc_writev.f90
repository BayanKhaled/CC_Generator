      SUBROUTINE CC_WriteV(NSize, V, Typ)
!
      USE CC_Module, ONLY : Name
!
!     o Write intermediates to disc
!
      IMPLICIT NONE
!
      CHARACTER(*), INTENT(IN) :: Typ
      INTEGER, INTENT(IN) :: NSize
      REAL(8), INTENT(IN) :: V(*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FBuf
      INTEGER :: N
!
      FBuf = TRIM(Name)//"."//TRIM(Typ)
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', ERR=9000, FORM='UNFORMATTED')
      GO TO 9100
 9000 STOP 'Error: in opening intermediate file in -CC_WriteV-'
 9100 CONTINUE
      REWIND(IO1)
      WRITE(IO1) (V(N), N = 1, NSize)
      CLOSE(IO1)
!
      END SUBROUTINE
