      SUBROUTINE CC_AntiSym_PQRS(G1, G2)
!
      USE CC_Module, ONLY : NMO
!
!     o Anti-symmetrize (PQ|RS) integrals
!       Integrals are saved into G1(S,Q,R,P) = <PR||QS> (S <-> Q)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(NMO,NMO,NMO,*)
      REAL(8), INTENT(IN) :: G2(NMO,NMO,NMO,*)
!
      INTEGER :: I, J, K, L
!
      CALL CCLib_DClear(G1, NMO*NMO*NMO*NMO)
!
!$OMP PARALLEL PRIVATE(I, J, K, L)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO I = 1, NMO
         DO J = 1, NMO
            IF (I == J) CYCLE
            DO K = 1, NMO
               DO L = 1, NMO
                  G1(L,K,J,I) = G2(L,K,J,I) - G2(K,L,J,I)
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
