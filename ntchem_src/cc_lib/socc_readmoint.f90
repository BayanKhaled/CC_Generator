      SUBROUTINE SOCC_ReadMOint(G, NSize, M13)
!
      USE CC_Module, ONLY : Name
!
!     o Read MO integrals from disc
!       M13 is the type of MO integrals
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NSize, M13
      COMPLEX(8), INTENT(OUT) :: G(*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      INTEGER :: N
!
      CALL CCLib_Int2Char(M13, c1, "0")
      FBuf = TRIM(Name)//".INT"//TRIM(c1)
!
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ERR=9000, FORM='UNFORMATTED')
      GO TO 9100
 9000 STOP 'MO integral file not found in -SOCC_ReadMOint-.'
 9100 CONTINUE
      REWIND(IO1)
      READ(IO1) (G(N), N = 1, NSize)
      CLOSE(IO1)
!
      END SUBROUTINE
