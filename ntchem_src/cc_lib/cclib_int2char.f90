      SUBROUTINE CCLib_Int2Char(I, Cout, A)
!
!     Convert a 2-digit INTEGER to character*2
!
      IMPLICIT NONE
!
      CHARACTER(LEN=1):: A
      CHARACTER(LEN=2):: Cout
      INTEGER :: J, I
!
      Cout = '  '
      WRITE(Cout, '(I2)') I
      DO J = 1, 2
         IF (Cout(J:J) == ' ') Cout(J:J) = A
      END DO
!
      END SUBROUTINE
