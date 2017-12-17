      SUBROUTINE CC_Trn2_LqIs(G1, G2)
!
      USE CC_Module, ONLY : CMOA, CMOB, NOc, NOcA, NOcB, NBF
!
!     o 2nd Occupied transformation (Lq|Is)
!                                       ^
!       Integrals are stored as G2(s,q,IL) (I < L)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1((NBF*(NBF+1))/2,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NBF,NBF,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: Iq, Ir, Is, IOcA, IOcB, Irs, LOcA, LOcB, LIOcAA, LIOcBA, LIOcBB
      REAL(8) :: Dum
!
      CALL CCLib_DClear(G2, NBF*NBF*(NOc*(NOc-1))/2)
!
!$OMP PARALLEL PRIVATE(LOcA, Iq, Irs, Ir, Is, Dum, IOcA, LIOcAA)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO LOcA = NOcA, 2, -1   ! alpha Occupied
         DO Iq = 1, NBF   ! AO loop
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1) / 2) + Is   ! OMP
                  Dum = G1(Irs,Iq,LOcA)
                  IF (ABS(Dum) < Small) CYCLE
                  DO IOcA = 1, LOcA - 1  ! alpha Occupied
                     LIOcAA = ((LOcA - 1) * (LOcA - 2)) / 2 + IOcA
                     G2(Is,Iq,LIOcAA) = G2(Is,Iq,LIOcAA) + CMOA(Ir,IOcA) * Dum
                     G2(Ir,Iq,LIOcAA) = G2(Ir,Iq,LIOcAA) + CMOA(Is,IOcA) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
!$OMP PARALLEL PRIVATE(LOcB, Iq, Irs, Ir, Is, Dum, IOcA, LIOcBA, IOcB, LIOcBB)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO LOcB = NOcB, 1, -1   ! beta Occupied
         DO Iq = 1, NBF
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1) / 2) + Is   ! OMP
                  Dum = G1(Irs,Iq,NOcA+LOcB)
                  IF (ABS(Dum) < Small) CYCLE
                  DO IOcA = 1, NOcA   ! alpha Occupied
                     LIOcBA = ((NOcA + LOcB - 1) * (NOcA + LOcB - 2)) / 2 + IOcA
                     G2(Is,Iq,LIOcBA) = G2(Is,Iq,LIOcBA) + CMOA(Ir,IOcA) * Dum
                     G2(Ir,Iq,LIOcBA) = G2(Ir,Iq,LIOcBA) + CMOA(Is,IOcA) * Dum
                  END DO
                  DO IOcB = 1, LOcB - 1   ! beta Occupied
                     LIOcBB = ((NOcA + LOcB - 1) * (NOcA + LOcB - 2)) / 2 + (NOcA + IOcB)
                     G2(Is,Iq,LIOcBB) = G2(Is,Iq,LIOcBB) + CMOB(Ir,IOcB) * Dum
                     G2(Ir,Iq,LIOcBB) = G2(Ir,Iq,LIOcBB) + CMOB(Is,IOcB) * Dum
                  END DO 
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
