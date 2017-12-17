      SUBROUTINE SOCC_ReadV(NSize, V, Typ)
!
      USE CC_Module, ONLY : Name
!
!     Read intermediates from disc
!
      IMPLICIT NONE
!
      CHARACTER(*), INTENT(IN) :: Typ
      INTEGER, INTENT(IN) :: NSize
      COMPLEX(8), INTENT(OUT) :: V(*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FBuf
      INTEGER :: N
!
      FBuf = TRIM(Name)//"."//TRIM(Typ)
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ERR=9000, FORM='UNFORMATTED')
      GO TO 9100
 9000 STOP 'Error: in opening intermediate file in -SOCC_ReadV-'
 9100 CONTINUE
      REWIND(IO1)
      READ(IO1) (V(N), N = 1, NSize)
      CLOSE(IO1)
!
      END SUBROUTINE
