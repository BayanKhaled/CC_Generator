      SUBROUTINE CCLib_NormVec_Z(N, A)
!     --------------------------------------
!     Normalize COMPLEX(8) vector A(1:N)
!     --------------------------------------
      IMPLICIT NONE
!
      COMPLEX(8) :: A(*)
      INTEGER :: N
!
      COMPLEX(8) :: FacNorm
      REAL(8) :: DZNRM2
!
      FacNorm = DCMPLX(1.0D+00 / DZNRM2(N, A, 1))
      CALL ZSCAL(N, FacNorm, A, 1)
!
      END SUBROUTINE
