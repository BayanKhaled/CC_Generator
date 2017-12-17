      SUBROUTINE CCLib_MakeArcWgt(NOrb, NOcc, IArc)
!
!     o Make arrays containing arc weights
!
      IMPLICIT NONE
!
      INTEGER :: NOrb, NOcc, IArc(NOrb,NOcc)
!
      INTEGER :: IVer(0:NOrb,0:NOcc)   ! vertex weight
      INTEGER :: K, M
!
!     o Initialize
!
      DO K = 0, NOcc
         DO M = 0, NOrb
            IVer(M,K) = 0
         END DO
      END DO
      DO M = 0, NOrb - NOcc
         IVer(M,0) = 1
      END DO
!
!     o Vertex weights
!
      DO M = 1, NOcc
         DO K = 0 + M, NOrb - (NOcc - M)
            IVer(K,M) = IVer(K-1,M) + IVer(K-1,M-1)
         END DO
      END DO
!
!     o Arc weights
!
      DO M = 1, NOcc
         DO K = 1, NOrb
            IArc(K,M) = IVer(K-1,M)
         END DO
      END DO
!
      END SUBROUTINE
