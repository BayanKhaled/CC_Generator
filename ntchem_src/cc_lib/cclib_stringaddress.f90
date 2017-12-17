      SUBROUTINE CCLib_StringAddress(NStr, NOcc, Space, IAdd)
!
!     o Address of string NStr of the length NOcc
!
      USE CC_Module, ONLY : NMax, Noc, Nvr, IArcWgtOcc, IArcWgtVir
      IMPLICIT NONE
!
      CHARACTER(LEN=3) :: Space
      INTEGER :: NStr(NOcc)
      INTEGER :: NOcc, IAdd
!
      INTEGER :: IOcc
!
      IAdd = 1
      IF (Space == "Occ") THEN
         DO IOcc = 1, NOcc
            IAdd = IAdd + IArcWgtOcc(NStr(IOcc),IOcc,NOcc)
         END DO
      ELSE IF (Space == "Vir") THEN
         DO IOcc = 1, NOcc
            IAdd = IAdd + IArcWgtVir(NStr(IOcc),IOcc,NOcc)
         END DO
      ELSE
         WRITE(*, *) "Illegal value of -Space- in CCLIB_StringAddress."
         WRITE(*, *) "STOP."
         STOP
      END IF
!
      END SUBROUTINE
