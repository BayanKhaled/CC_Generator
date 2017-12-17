      SUBROUTINE CC_ArcWgt_Allocate
!
      USE CC_Module, ONLY : NMax, NOc, NVr, IArcWgtOcc, IArcWgtVir
!
      IMPLICIT NONE
!
      ALLOCATE(IArcWgtOcc(NOc,MAX(2, NMax),MAX(2, NMax)))
      ALLOCATE(IArcWgtVir(NVr,MAX(2, NMax),MAX(2, NMax)))
!
      END SUBROUTINE
