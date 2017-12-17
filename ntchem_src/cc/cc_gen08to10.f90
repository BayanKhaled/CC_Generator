      SUBROUTINE CC_Gen08to10(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr
!
!     o Generate type-10 anti-symmetrized integrals <AJ||IK>
!       by transposing the type-08 integrals
!       Integrals are stored as G2(J,A,IK)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1(NVr*(NOc*(NOc-1))/2,*)
      REAL(8), INTENT(OUT) :: G2(NOc,*)
!
      INTEGER :: IOc, Jvoo
!
      DO IOc = 1, NOc
         DO Jvoo = 1, NVr * (NOc * (NOc - 1)) / 2
            G2(IOc,Jvoo) = G1(Jvoo,IOc)
         END DO
      END DO
!
      END SUBROUTINE
