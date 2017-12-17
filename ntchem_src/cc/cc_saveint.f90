      SUBROUTINE CC_SaveInt(G, NSize, ID)
!
      USE CC_Module, ONLY : Name
!
!     o Save MO integrals
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NSize, ID
      REAL(8), INTENT(IN) :: G(*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=2) :: c1
      CHARACTER(LEN=255) :: FName
      INTEGER :: N
!
      CALL CCLib_Int2Char(ID, c1, "0")
!
      FName = TRIM(Name)//".INT"//c1
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G(N), N = 1, NSize)
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type ", I2, " integrals saved on disk.")') ID
!
      END SUBROUTINE
