      SUBROUTINE CC_ConvSCF_Allocate
!
      USE CC_Module, ONLY : ERIBuf, Lp, Lq, Lr, Ls, MaxBuf
!
      IMPLICIT NONE
!
      ALLOCATE(ERIBuf(MaxBuf))
      ALLOCATE(Lp(MaxBuf))
      ALLOCATE(Lq(MaxBuf))
      ALLOCATE(Lr(MaxBuf))
      ALLOCATE(Ls(MaxBuf))
!
      END SUBROUTINE
