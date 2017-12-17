      SUBROUTINE CC_ConvSCF_Deallocate
!
      USE CC_Module, ONLY : ERIBuf, Lp, Lq, Lr, Ls
!
      IMPLICIT NONE
!
      DEALLOCATE(ERIBuf)
      DEALLOCATE(Lp)
      DEALLOCATE(Lq)
      DEALLOCATE(Lr)
      DEALLOCATE(Ls)
!
      END SUBROUTINE
