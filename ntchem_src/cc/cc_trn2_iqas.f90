      SUBROUTINE CC_Trn2_IqAs(G1, G2)
!
      USE CC_Module, ONLY : NVr, NOc, NBF, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB
!
!     o 2nd virtual transformation (Iq|As)
!                                      ^
!       Integrals are stored as G2(s,q,A,I)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1((NBF*(NBF+1))/2,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NBF,NBF,NVr,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: Iq, Ir, Is, Irs, IOcA, KVrA, KVrB, KVrBX
      REAL(8) :: Dum
!
      CALL CCLib_DClear(G2, NBF*NBF*NVr*NOc)
!
!$OMP PARALLEL PRIVATE(IOcA, Iq, Irs, Ir, Is, Dum, KVrA, KVrB, KVrBX)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IOcA = 1, NOc   ! Loop involves alpha and beta Occ. MOs
         DO Iq = 1, NBF
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1)/ 2) + Is   ! OMP
                  Dum = G1(Irs, Iq, IOcA)
                  IF (ABS(Dum) < Small) CYCLE
                  DO KVrA = 1, NVrA
                     G2(Is,Iq,KVrA,IOcA) = G2(Is,Iq,KVrA,IOcA) + CMOA(Ir,NOcA+KVrA) * Dum
                     G2(Ir,Iq,KVrA,IOcA) = G2(Ir,Iq,KVrA,IOcA) + CMOA(Is,NOcA+KVrA) * Dum
                  END DO
                  DO KVrB = 1, NVrB
                     KVrBX = NVrA + KVrB
                     G2(Is,Iq,KVrBX,IOcA) = G2(Is,Iq,KVrBX,IOcA) + CMOB(Ir,NOcB+KVrB) * Dum
                     G2(Ir,Iq,KVrBX,IOcA) = G2(Ir,Iq,KVrBX,IOcA) + CMOB(Is,NOcB+KVrB) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
