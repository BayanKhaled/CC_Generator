      SUBROUTINE CC_Trn2_CqIs(G1, G2)
!
      USE CC_Module, ONLY : NOc, NVr, CMOA, CMOB, NVrA, NVrB, NOcA, NOcB, NBF
!
!     o 2nd Occupied transformation (Cq|Is)
!                                     ^
!       Integrals are stored as (I,s,q,C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1((NBF*(NBF+1))/2,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NOc,NBF,NBF,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: Iq, Ir, Is, IVr, KOcA, KOcB, Irs
      REAL(8) :: Dum
!
      CALL CCLib_DClear(G2, NBF*NBF*NOc*NVr)
!
!$OMP PARALLEL PRIVATE(IVr, Iq, Irs, Ir, Is, Dum, KOcA, KOcB)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO IVr = 1, NVr
         DO Iq = 1, NBF
            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
                  Irs = Irs + 1
                  Dum = G1(Irs,Iq,IVr)
                  IF (ABS(Dum) < Small) CYCLE
                  DO KOcA = 1, NOcA
                     G2(KOcA,Ir,Iq,IVr) = G2(KOcA,Ir,Iq,IVr) + CMOA(Is,KOcA) * Dum
                     G2(KOcA,Is,Iq,IVr) = G2(KOcA,Is,Iq,IVr) + CMOA(Ir,KOcA) * Dum
                  END DO
                  DO KOcB = 1, NOcB
                     G2(NOcA+KOcB,Ir,Iq,IVr) = G2(NOcA+KOcB,Ir,Iq,IVr) + CMOB(Is,KOcB) * Dum
                     G2(NOcA+KOcB,Is,Iq,IVr) = G2(NOcA+KOcB,Is,Iq,IVr) + CMOB(Ir,KOcB) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
