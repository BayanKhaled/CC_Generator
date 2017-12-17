      SUBROUTINE CCLib_NormVec_D(N, A)
!     --------------------------------------
!     Normalize REAL(8) vector A(1:N)
!     --------------------------------------
      IMPLICIT NONE
!
      REAL(8) :: A(*)
      INTEGER :: N
!
      REAL(8) :: FacNorm
      REAL(8) :: DNRM2
!
      FacNorm = 1.0D+00 / DNRM2(N, A, 1)
      CALL DSCAL(N, FacNorm, A, 1)
!
      END SUBROUTINE
