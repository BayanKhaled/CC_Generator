      SUBROUTINE CC_ArcWgt_Deallocate
!
      USE CC_Module, ONLY : IArcWgtOcc, IArcWgtVir
!
      IMPLICIT NONE
!
      DEALLOCATE(IArcWgtOcc)
      DEALLOCATE(IArcWgtVir)
!
      END SUBROUTINE
