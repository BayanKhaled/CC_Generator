      SUBROUTINE CC_T2init(T)
!
      USE CC_Module, ONLY : NMO, NOc, NVr, NOcA, NOcB, NVrA, NVrB, UHF, Ene
      USE CC_Constant_Module, ONLY : Zero
!
!     o Initialize T2 amplitude (MP2)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: T((NVr*(NVr-1))/2,*)
!
      REAL(8), PARAMETER :: Small = 1.0D-11
      INTEGER :: NSizeG1, NSizeG2, JOc, IOc, KVr, LVr, JOff, JIOc, LOff, LKVr, CCLib_NComb
      REAL(8) :: EJ, EIJ, ELIJ, EKLIJ, Dum, EMP2
      REAL(8), ALLOCATABLE :: G(:,:)
!
!     o Allocate MO integral array
!
      NSizeG1 = CCLib_NComb(NVr, 2)
      NSizeG2 = CCLib_NComb(NOc, 2)
      ALLOCATE(G(NSizeG1,NSizeG2))
!
!     o Load type-13 MO integrals
!
      CALL CC_ReadMOint(G, NSizeG1*NSizeG2, 13)
!
!     o Initialize T2 amplitude
!
      EMP2 = Zero
      DO JOc = 2, NOc
         EJ = Ene(JOc)
         JOff = ((JOc - 1) * (JOc - 2)) / 2
         DO IOc = 1, JOc - 1
            EIJ = EJ + Ene(IOc)
            JIOc = JOff + IOc
            DO LVr = 2, NVr
               ELIJ =  EIJ - Ene(NOc+LVr)
               LOff = ((LVr - 1) * (LVr - 2)) / 2
               DO KVr = 1, LVr - 1
                  LKVr = LOff + KVr
                  Dum = G(LKVr,JIOc)
                  IF (ABS(Dum) > Small) THEN
                     EKLIJ = ELIJ - Ene(NOc+KVr)
                     T(LKVr,JIOc) = Dum / EKLIJ
                     EMP2 = EMP2 + Dum * T(LKVr,JIOc)
                  END IF
               END DO
            END DO 
         END DO
      END DO
!
      WRITE(*, '(" ..... MP2 correlation energy =", F20.12)') EMP2
!
      DEALLOCATE(G)
!
      END SUBROUTINE
