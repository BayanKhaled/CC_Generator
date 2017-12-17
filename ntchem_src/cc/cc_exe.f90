      SUBROUTINE CC_Exe
!
      USE CC_Module, ONLY: NMax, MaxIter, ECorr, NoT1, EneRef, Guess, DoDIIS
      USE CC_Constant_Module, ONLY : Zero
!
!     o Solve CC amplitude equations
!
!       WrkT and WrkW are of size NOc!/(NOc-NMax)!*NVr!/(NVr-NMax)!
!       WrkG can store the largest MO integral type
!
      IMPLICIT NONE
!
      LOGICAL :: Conv
      INTEGER :: NRank, Iter
      REAL(8) :: ECorr0, DECorr, RMSGrd
!
      ECorr = Zero
!
!     o Initialize T amplitude (MP2)
!
      IF (TRIM(Guess) == 'INIT') THEN
         CALL CC_InitWrkT   ! initialize the T amplitudes and write to file
      END IF
!
!     o Start CC iteration
!
      Conv = .FALSE.
      DO Iter = 1, MaxIter
!
!        o Initial Correlation energy
!
         ECorr0 = ECorr
         CALL CC_ECorr(ECorr)
         DECorr = ECorr - ECorr0
!
!        o Calculate residues
!
         DO NRank = 1, NMax
            SELECT CASE (NMax)
               CASE (1)
                  CALL CC_CalcResidueS
               CASE (2)
                  CALL CC_CalcResidueD(NRank)
               CASE (3)
                  CALL CC_CalcResidueT(NRank)
               CASE (4)
                  CALL CC_CalcResidueQ(NRank)
               CASE DEFAULT
                  STOP 'NYI: CC_CalcResidue?'
            END SELECT
         END DO
!
!        o Check convergence
!
         CALL CC_CheckConv(DECorr, Conv, RMSGrd)
!ActiveCC_Bgn
         Conv = (Conv .AND. (Iter > 2))
!ActiveCC_End
!
         WRITE(*, '(" CC_Iter. ", I4, 3F20.12)') Iter, ECorr, DECorr, RMSGrd
!
         IF (Conv) THEN
            WRITE(*, '(" +++++ CC wavefunction converged +++++", /)')
            WRITE(*, '(" o CC correlation energy =", F20.12)') ECorr
            WRITE(*, '(" o Reference energy      =", F20.12)') EneRef
            WRITE(*, '(" o CC total energy       =", F20.12,/)') EneRef + ECorr
            DO NRank = 1, NMax
               IF (NoT1 .AND. (NRank == 1)) CYCLE
               CALL CC_UpdateT(NRank)
            END DO
            CALL CC_ECorr(ECorr)
            EXIT
         ELSE
            IF (Iter == MaxIter) THEN
               WRITE(*, '(" +++++ CC wavefunction did not converge +++++")')
               EXIT
            END IF
         END IF
!
!        o Update T amplitudes
!
         DO NRank = 1, NMax
            IF (NoT1 .AND. (NRank == 1)) CYCLE
            CALL CC_UpdateT(NRank)
         END DO
!
!        o DIIS extrapolation of next T amplitudes
!
         IF (DoDIIS) THEN
            CALL CC_UpdateT_DIIS(Iter)
         END IF
!
      END DO   ! Iter
!
      END SUBROUTINE
