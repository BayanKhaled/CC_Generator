      SUBROUTINE CC_Trn2_PqRs(G1, G2)
!
      USE CC_Module, ONLY : CMO, NMO, NBF
      USE CC_Constant_Module, ONLY : Zero
!
!     o 2nd occupied transformation (Pq|Rs)
!                                       ^
!       Integrals are stored as G2(s,q,R,P)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(IN) :: G1((NBF*(NBF+1))/2,NBF,*)
      REAL(8), INTENT(OUT) :: G2(NBF,NBF,NMO,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: Iq, Ir, Is, Irs, IMO, LMO
      REAL(8) :: Dum
!
      CALL CCLib_DClear(G2, NBF*NBF*NMO*NMO)
!
!$OMP PARALLEL PRIVATE(LMO, Iq, Irs, Ir, Is, Dum, IMO)
!$OMP DO SCHEDULE(DYNAMIC, 1)
      DO LMO = 1, NMO   ! MO loop
         DO Iq = 1, NBF   ! AO loop
!OMP            Irs = 0
            DO Ir = 1, NBF
               DO Is = 1, Ir
!OMP                  Irs = Irs + 1
                  Irs = (Ir * (Ir - 1) / 2) + Is   ! OMP
                  Dum = G1(Irs,Iq,LMO)
                  IF (ABS(Dum) < Small) CYCLE
                  DO IMO = 1, NMO
                     G2(Is,Iq,IMO,LMO) = G2(Is,Iq,IMO,LMO) + CMO(Ir,IMO) * Dum
                     G2(Ir,Iq,IMO,LMO) = G2(Ir,Iq,IMO,LMO) + CMO(Is,IMO) * Dum
                  END DO
               END DO
            END DO
         END DO
      END DO
!$OMP END DO
!$OMP END PARALLEL
!
      END SUBROUTINE
