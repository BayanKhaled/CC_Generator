      SUBROUTINE CCLib_Clock(CTime, WTime)
!
!     o CPU and WALL Time
!
      IMPLICIT NONE
!
      REAL(8) :: CTime, WTime
!
      INTEGER :: NCnt, NCntRate, NCntMax
!
      CALL SYSTEM_CLOCK(NCnt, NCntRate, NCntMax)
      WTime = DBLE(NCnt) / DBLE(NCntRate)
      CALL CPU_TIME(CTime)
!
      END SUBROUTINE
