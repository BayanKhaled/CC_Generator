      SUBROUTINE CCLib_DClear(A, N)
!
!     Zero-clear the first N elements of array A
!     Double precision ver.
!
      USE CC_Constant_Module, ONLY : Zero
!
      IMPLICIT NONE
!
      REAL(8) :: A(*)
      INTEGER :: N
!
      CALL DCOPY(N, Zero, 0, A, 1)
!
      END SUBROUTINE
