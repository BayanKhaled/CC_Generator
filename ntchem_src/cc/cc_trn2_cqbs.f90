      SUBROUTINE CC_Trn2_CqBs(G1, G2)
!
      USE CC_Module, ONLY : NVr, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB, NBF
!
!     o 2nd virtual transformation (Cq|Bs)
!                                      ^
!       Integrals are stored as (s,q,BC) (B < C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1((NBF*(NBF+1))/2,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NBF,NBF,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: Iq, Ir, Is, Irs, LVrA, LVrB, IVrA, IVrB, LIVrAA, LIVrBB, LIVrBA
      REAL(8) :: Dum
!
      CALL CCLib_DClear(G2, NBF*NBF*(NVr*(NVr-1))/2)
!
!$OMP PARALLEL PRIVATE(LVrA, Iq, Irs, Ir, Is, Dum, IVrA, LIVrAA)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO LVrA = NVrA, 2, -1   ! alpha vir.
         DO Iq = 1, NBF   ! AO loop
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1) / 2) + Is   ! OMP
                  Dum = G1(Irs,Iq,LVrA)
                  IF (ABS(Dum) < Small) CYCLE
                  DO IVrA = 1, LVrA - 1
                     LIVrAA = ((LVrA - 1) * (LVrA - 2)) / 2 + IVrA
                     G2(Is,Iq,LIVrAA) = G2(Is,Iq,LIVrAA) + CMOA(Ir,NOcA+IVrA) * Dum
                     G2(Ir,Iq,LIVrAA) = G2(Ir,Iq,LIVrAA) + CMOA(Is,NOcA+IVrA) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
!$OMP PARALLEL PRIVATE(LVrB, Iq, Irs, Ir, Is, Dum, IVrA, LIVrBA, IVrB, LIVrBB)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO LVrB = NVrB, 1, -1
         DO Iq = 1, NBF
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1) / 2) + Is   ! OMP
                  Dum = G1(Irs,Iq,NVrA+LVrB)
                  IF (ABS(Dum) < Small) CYCLE
                  DO IVrA = 1, NVrA  ! alpha Occupied
                     LIVrBA = ((NVrA + LVrB - 1) * (NVrA + LVrB - 2)) / 2 + IVrA
                     G2(Is,Iq,LIVrBA) = G2(Is,Iq,LIVrBA) + CMOA(Ir,NOcA+IVrA) * Dum
                     G2(Ir,Iq,LIVrBA) = G2(Ir,Iq,LIVrBA) + CMOA(Is,NOcA+IVrA) * Dum
                  END DO
                  DO IVrB = 1, LVrB - 1  ! beta Occupied
                     LIVrBB = ((NVrA + LVrB - 1) * (NVrA + LVrB - 2)) / 2 + (NVrA + IVrB)
                     G2(Is,Iq,LIVrBB) = G2(Is,Iq,LIVrBB) + CMOB(Ir,NOcB+IVrB) * Dum
                     G2(Ir,Iq,LIVrBB) = G2(Ir,Iq,LIVrBB) + CMOB(Is,NOcB+IVrB) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO 
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
