      SUBROUTINE CCLib_InitArcWgt
!
!     Make arrays containing arc weights
!
      USE CC_Module, ONLY : Noc, Nvr, NMax, IArcWgtOcc, IArcWgtVir
!
      IMPLICIT NONE
!
      INTEGER :: I
!
      DO I = 1, MAX(2, NMax)
         CALL CCLib_MakeArcWgt(Noc, I, IArcWgtOcc(1,1,I))
         CALL CCLib_MakeArcWgt(Nvr, I, IArcWgtVir(1,1,I))
      END DO
!
      END SUBROUTINE
