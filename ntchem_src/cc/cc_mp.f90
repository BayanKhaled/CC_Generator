      SUBROUTINE CC_MP
!
      USE CC_Module, ONLY : NMO, NOc, NVr, Ene
      USE CC_Constant_Module, ONLY : Zero, Four, Eight
!
!     o MP energies (Assuming canonical MO)
!
      IMPLICIT NONE
!
      REAL(8), PARAMETER :: Small = 1.0D-12
      INTEGER :: IOc, JOc, KOc, LOc, MOc,IVr, JVr, KVr, LVr, MVr
      REAL(8) :: EMP, EMP1, EMP2, EMP3, EMP4, Efac1, Efac2, Efac3, DumA, DumB, DumC, DumD
      REAL(8), ALLOCATABLE :: G2(:,:), G4(:,:,:,:)
!
      ALLOCATE(G4(NMO,NMO,NMO,NMO))
!
      CALL CC_ReadMOint(G4, NMO*NMO*NMO*NMO, 0)
!
!     o MP2
!
      EMP = Zero
!
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            Efac1 = Ene(IOc) + Ene(JOc)
            DO KVr = 1, NVr
               DO LVr = 1, NVr
                  Efac2 = Ene(NOc+KVr) + Ene(NOc+LVr)
                  DumA = G4(NOc+LVr,NOc+KVr,JOc,IOc)
                  DumB = G4(JOc,IOc,NOc+LVr,NOc+KVr)
                  EMP = EMP + DumA * DumB / (Efac1 - Efac2)
               END DO
            END DO
         END DO
      END DO
!
      EMP = EMP / Four
!
      WRITE(*, '(" ... E(MP2) =", F16.10)') EMP
!
!     o MP3
!
!     o (VV|VV) term
!
      EMP1 = Zero
      DO IVr = 1, NVr
         DO JVr = 1, NVr
            DO KVr = 1, NVr
               DO LVr = 1, NVr
                  DumA = G4(NOc+LVr,NOc+KVr,NOc+JVr,NOc+IVr)
                  IF (ABS(DumA) < Small) CYCLE
                  DO IOc = 1, NOc
                     DO JOc = 1, NOc
                        IF ((IVr == JVr) .OR. (KVr == LVr) .OR. (IOc == JOc)) CYCLE
                        DumB = G4(NOc+JVr,NOc+IVr,JOc,IOc)
                        DumC = G4(JOc,IOc,NOc+LVr,NOc+KVr)
                        Efac1 = Ene(IOc) + Ene(JOc) - Ene(NOc+IVr) - Ene(NOc+JVr)
                        Efac2 = Ene(IOc) + Ene(JOc) - Ene(NOc+KVr) - Ene(NOc+LVr)
                        EMP1 = EMP1 + DumA * DumB * DumC / (Efac1 * Efac2)
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP1 = EMP1 / Eight
!Jan08      WRITE(*, '(" ..... E(MP3)_1 =", F16.10)') EMP1
!
!     o (OO|OO) term
!
      EMP2 = Zero
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO KOc = 1, NOc
               DO LOc = 1, NOc
                  DumA = G4(JOc,IOc,LOc,KOc)
                  IF (ABS(DumA) < Small) CYCLE
                  DO IVr = 1, NVr
                     DO JVr = 1, NVr
                        IF ((IOc == JOc) .OR. (KOc == LOc) .OR. (IVr == JVr)) CYCLE
                        DumB = G4(NOc+JVr,NOc+IVr,JOc,IOc)
                        DumC = G4(LOc,KOc,NOc+JVr,NOc+IVr)
                        Efac1 = Ene(KOc) + Ene(LOc) - Ene(NOc+IVr) - Ene(NOc+JVr)
                        Efac2 = Ene(IOc) + Ene(JOc) - Ene(NOc+IVr) - Ene(NOc+JVr)
                        EMP2 = EMP2 + DumA * DumB * DumC / (Efac1 * Efac2)
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP2 = EMP2 / Eight
!DebugWRITE(*, '(" ..... E(MP3)_2 =", F16.10)') EMP2
!
!     o (OO|VV) term
!
      EMP3 = Zero
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO KOc = 1, NOc
               DO IVr = 1, NVr
                  DO JVr = 1, NVr
                     DO KVr = 1, NVr
                        IF ((IOc == JOc) .OR. (IVr == JVr) .OR. (JOc == KOc) .OR. (IVr == KVr)) CYCLE
                        DumA = G4(NOc+KVr,IOc,NOc+JVr,KOc)
                        DumB = G4(NOc+JVr,NOc+IVr,JOc,IOc)
                        DumC = G4(JOc,KOc,NOc+KVr,NOc+IVr)
                        Efac1 = Ene(IOc) + Ene(JOc) - Ene(NOc+IVr) - Ene(NOc+JVr)
                        Efac2 = Ene(KOc) + Ene(JOc) - Ene(NOc+IVr) - Ene(NOc+KVr)
                        EMP3 = EMP3 - DumA * DumB * DumC / (Efac1 * Efac2)
                     END DO
                  END DO
               END DO
            END DO
         END DO
      END DO
!DebugWRITE(*, '(" ..... E(MP3)_3 =", F16.10)') EMP3
!
      WRITE(*, '(" ... E(MP3) =", F16.10)') EMP1 + EMP2 + EMP3
!
!     o MP4(single)
!
      ALLOCATE(G2(NVr,NOc))
!
!     o Diagram 1
!
      CALL CCLib_DClear(G2, NVr*NOc)
      DO IOc = 1, NOc
         DO KOc = 1, NOc
            DO IVr = 1, NVr
               DO JVr = 1, NVr
                  DO KVr = 1, NVr
                     DumA = G4(NOc+KVr,NOc+IVr,KOc,IOc)
                     DumB = G4(KOc,NOc+JVr,NOc+KVr,NOc+IVr)
                     Efac1 = Ene(IOc) + Ene(KOc) - Ene(NOc+IVr) - Ene(NOc+KVr)
                     G2(JVr,IOc) = G2(JVr,IOc) + DumA * DumB / Efac1
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      EMP1 = Zero
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO JVr = 1, NVr
               DO LOc = 1, NOc
                  DO LVr = 1, NVr
                     DumC = G4(NOc+LVr,IOc,LOc,JOc)
                     DumD = G4(LOc,JOc,NOc+LVr,NOc+JVr)
                     Efac2 = Ene(IOc) - Ene(NOc+JVr)
                     Efac3 = Ene(JOc) + Ene(LOc) - Ene(NOc+JVr) - Ene(NOc+LVr)
                     EMP1 = EMP1 - G2(JVr,IOc) * DumC * DumD / (Efac2 * Efac3)
                  END DO
               END DO
            END DO
         END DO
      END DO
!
      EMP1 = EMP1 / Four
!DebugWRITE(*, '(" ..... E(MP4S)_1 =", F16.10)') EMP1
!
!     o Diagram 2
!
      CALL CCLib_DClear(G2, NVr*NOc)
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO KOc = 1, NOc
               DO IVr = 1, NVr
                  DO KVr = 1, NVr
                     DumA = G4(NOc+KVr,NOc+IVr,KOc,IOc)
                     DumB = G4(KOc,IOc,NOc+KVr,JOc)
                     Efac1 = Ene(IOc) + Ene(KOc) - Ene(NOc+IVr) - Ene(NOc+KVr)
                     G2(IVr,JOc) = G2(IVr,JOc) + DumA * DumB / Efac1
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP2 = Zero
      DO JOc = 1, NOc
         DO LOc = 1, NOc
            DO IVr = 1, NVr
               DO JVr = 1, NVr
                  DO LVr = 1, NVr
                     DumC = G4(NOc+LVr,NOc+JVr,LOc,NOc+IVr)
                     DumD = G4(LOc,JOc,NOc+LVr,NOc+JVr)
                     Efac2 = Ene(JOc) - Ene(NOc+IVr)
                     Efac3 = Ene(JOc) + Ene(LOc) - Ene(NOc+JVr) - Ene(NOc+LVr)
                     EMP2 = EMP2 - G2(IVr,JOc) * DumC * DumD / (Efac2 * Efac3)
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP2 = EMP2 / Four
!DebugWRITE(*, '(" ..... E(MP4S)_2 =", F16.10)') EMP2
!
!     o Diagram 3
!
      CALL CCLib_DClear(G2, NVr*NOc)
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO IVr = 1, NVr
               DO JVr = 1, NVr
                  DO LVr = 1, NVr
                     DumA = G4(NOc+LVr,NOc+IVr,JOc,IOc)
                     DumB = G4(JOc,NOc+JVr,NOc+LVr,NOc+IVr)
                     Efac1 = Ene(IOc) + Ene(JOc) - Ene(NOc+IVr) - Ene(NOc+LVr)
                     G2(JVr,IOc) = G2(JVr,IOc) + DumA * DumB / Efac1
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP3 = Zero
      DO IOc = 1, NOc
         DO KOc = 1, NOc
            DO JVr = 1, NVr
               DO KVr = 1, NVr
                  DO MVr = 1, NVr
                     DumC = G4(NOc+MVr,NOc+KVr,KOc,NOc+JVr)
                     DumD = G4(KOc,IOc,NOc+MVr,NOc+KVr)
                     Efac2 = Ene(IOc) - Ene(NOc+JVr)
                     Efac3 = Ene(IOc) + Ene(KOc) - Ene(NOc+KVr) - Ene(NOc+MVr)
                     EMP3 = EMP3 + G2(JVr,IOc) * DumC * DumD / (Efac2 * Efac3)
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP3 = EMP3 / Four
!DebugWRITE(*, '(" ..... E(MP4S)_3 =", F16.10)') EMP3
!
!     o Diagram 4
!
      CALL CCLib_DClear(G2, NVr*NOc)
      DO IOc = 1, NOc
         DO JOc = 1, NOc
            DO LOc = 1, NOc
               DO IVr = 1, NVr
                  DO JVr = 1, NVr
                     DumA = G4(NOc+JVr,NOc+IVr,LOc,IOc)
                     DumB = G4(LOc,IOc,NOc+JVr,JOc)
                     Efac1 = Ene(IOc) + Ene(LOc) - Ene(NOc+IVr) - Ene(NOc+JVr)
                     G2(IVr,JOc) = G2(IVr,JOc) + DumA * DumB / Efac1
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP4 = Zero
      DO JOc = 1, NOc
         DO KOc = 1, NOc
            DO MOc = 1, NOc
               DO IVr = 1, NVr
                  DO KVr = 1, NVr
                     DumC = G4(NOc+KVr,JOc,MOc,KOc)
                     DumD = G4(MOc,KOc,NOc+KVr,NOc+IVr)
                     Efac2 = Ene(JOc) - Ene(NOc+IVr)
                     Efac3 = Ene(KOc) + Ene(MOc) - Ene(NOc+IVr) - Ene(NOc+KVr)
                     EMP4 = EMP4 + G2(IVr,JOc) * DumC * DumD / (Efac2 * Efac3)
                  END DO
               END DO
            END DO
         END DO
      END DO
      EMP4 = EMP4 / Four
!DebugWRITE(*, '(" ..... E(MP4S)_4 =", F16.10)') EMP4
      WRITE(*, '(" ... E(MP4S) =", F16.10)') EMP1 + EMP2 + EMP3 + EMP4
!
      DEALLOCATE(G2)
      DEALLOCATE(G4)
!
      END SUBROUTINE
