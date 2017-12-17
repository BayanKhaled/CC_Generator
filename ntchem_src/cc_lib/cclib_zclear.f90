      SUBROUTINE CCLib_ZClear(A, N)
!
!     o Zero-clear the first N elements of array A
!       Double complex ver.
!
      USE CC_Constant_Module, ONLY : ZeroC
!
      IMPLICIT NONE
!
      INTEGER :: N
      COMPLEX(8) :: A(*)
!
      CALL ZCOPY(N, ZeroC, 0, A, 1)
!
      END SUBROUTINE
