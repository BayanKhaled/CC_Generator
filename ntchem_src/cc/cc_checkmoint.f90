      SUBROUTINE CC_CheckMOint
!
      USE CC_Module, ONLY : NMO, NOc, NVr, Ene
      USE CC_Constant_Module, ONLY : Zero
!
!     o Numerical check of MO integrals (For debugging purpose only)
!
      IMPLICIT NONE
!
!YA061512      REAL(8), PARAMETER :: Small = 1.0D-12
      REAL(8), PARAMETER :: Middle = 1.0D-06
      INTEGER :: NSize, CCLib_NComb, IOc, JOc, KOc, LOc, IVr, JVr, KVr, LVr, &
     &   IJOc, IJVr, IKOc, IKVr, ILOc, ILVr, JKOc, JKVr
      REAL(8), ALLOCATABLE :: G2(:,:), G3(:,:,:), G4(:,:,:,:), G4_(:,:,:,:)
      REAL(8) :: Error, DumA, DumB
!
      ALLOCATE(G4(NMO,NMO,NMO,NMO))
      CALL CC_ReadMOint(G4, NMO*NMO*NMO*NMO,0)
!
!     o Type-4 integrals
!
      NSize = CCLib_NComb(NVr, 2) * CCLib_NComb(NVr, 2)
      ALLOCATE(G2(CCLib_NComb(NVr, 2),CCLib_NComb(NVr, 2)))
      CALL CC_ReadMOint(G2, NSize, 4)
      Error = Zero
      DO LVr = 2, NVr
         DO IVr = 1, LVr - 1
            DO KVr = 2, NVr
               DO JVr = 1, KVr - 1
                  DumA = G4(KVr+NOc,JVr+NOc,LVr+NOc,IVr+NOc)
                  ILVr = ((LVr - 1) * (LVr - 2)) / 2 + IVr
                  JKVr = ((KVr - 1) * (KVr - 2)) / 2 + JVr
                  DumB = G2(JKVr,ILVr)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 4 Error(", 4I3, ")= ", 2F12.6)') IVr, LVr, JVr, KVr, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 04 int: ", F20.14)') Error
      DEALLOCATE(G2)
!
!     o Type-5 integrals
!
      NSize = CCLib_NComb(NOc, 2) * CCLib_NComb(NOc,2)
      ALLOCATE(G2(CCLib_NComb(NOc, 2),CCLib_NComb(NOc, 2)))
      CALL CC_ReadMOint(G2, NSize, 5)
      Error = Zero
      DO LOc = 2, NOc
         DO IOc = 1, LOc - 1
            DO KOc = 2, NOc
               DO JOc = 1, KOc - 1
                  DumA = G4(LOc,IOc,KOc,JOc)
                  ILOc = ((LOc - 1) * (LOc - 2)) / 2 + IOc
                  JKOc = ((KOc - 1) * (KOc - 2)) / 2 + JOc
                  DumB = G2(JKOc,ILOc)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 5 Error(", 4I3 ,")= ", 2F12.6)') JOc, KOc, IOc, LOc, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 05 int: ", F20.14)') Error
      DEALLOCATE(G2)
!
!     o Type-6 integrals
!
      NSize = NOc * NVr * NOc * NVr
      ALLOCATE(G4_(NVr,NOc,NVr,NOc))
      CALL CC_ReadMOint(G4_, NSize, 6)
      Error = Zero
      DO IOc = 1, NOc
         DO IVr = 1, NVr
            DO JOc = 1, NOc
               DO JVr = 1, NVr
                  DumA = G4(JVr+NOc,IOc,IVr+NOc,JOc)
                  DumB = G4_(JVr,JOc,IVr,IOc)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 6 Error(", 4I3, ")= ", 2F12.6)') JOc, IVr, IOc, JVr, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 06 int: ", F20.14)') Error
      DEALLOCATE(G4_)
!
!     o Type-7 integrals
!
      NSize = CCLib_NComb(NVr, 2) * NOc * NVr
      ALLOCATE(G3(CCLib_NComb(NVr, 2),NOc,NVr))
      CALL CC_ReadMOint(G3, NSize, 7)
      Error = Zero
      DO KVr = 1, NVr
         DO IOc = 1, NOc
            DO JVr = 2, NVr
               DO IVr = 1, JVr - 1
                  DumA = G4(JVr+NOc,IVr+NOc,KVr+NOc,IOc)
                  IJVr = ((JVr - 1) * (JVr - 2)) / 2 + IVr
                  DumB = G3(IJVr,IOc,KVr)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 7 Error(", 4I3, ")= ", 2F12.6)') IOc, KVr, IVr, JVr, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 07 int: ", F20.14)') Error
      DEALLOCATE(G3)
!
!     o Type-8 integrals
!
      NSize = CCLib_NComb(NOc, 2) * NOc * NVr
      ALLOCATE(G3(NVr,CCLib_NComb(NOc, 2),NOc))
      CALL CC_ReadMOint(G3, NSize, 8)
      Error = Zero
      DO KOc = 1, NOc
         DO IVr = 1, NVr
            DO JOc = 2, NOc
               DO IOc = 1, JOc - 1
                  DumA = G4(KOc,NOc+IVr,JOc,IOc)
                  IJOc = ((JOc - 1) * (JOc - 2)) / 2 + IOc
                  DumB = G3(IVr,IJOc,KOc)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 8 Error(", 4I3, ")= ", 2F12.6)') IOc, JOc, IVr, KOc, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 08 int: ", F20.14)') Error
      DEALLOCATE(G3)
!
!     o Type-9 integrals
!
      NSize = CCLib_NComb(NVr, 2) * NOc * NVr
      ALLOCATE(G3(NVr,CCLib_NComb(NVr, 2),NOc))
      CALL CC_ReadMOint(G3, NSize, 9)
      Error = Zero
      DO IOc = 1, NOc
         DO KVr = 2, NVr
            DO IVr = 1, KVr - 1
               DO JVr = 1, NVr
                  DumA = G4(JVr+NOc,IOc,KVr+NOc,IVr+NOc)
                  IKVr = ((KVr - 1) * (KVr - 2)) / 2 + IVr
                  DumB = G3(JVr,IKVr,IOc)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 9 Error(", 4I3, ")= ", 2F12.6)') IVr, KVr, IOc, JVr, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 09 int: ", F20.14)') Error
      DEALLOCATE(G3)
!
!     o Type-10 integrals
!
      NSize = CCLib_NComb(NOc, 2) * NOc * NVr
      ALLOCATE(G3(NOc,NVr,CCLib_NComb(NOc, 2)))
      CALL CC_ReadMOint(G3, NSize, 10)
      Error = Zero
      DO KOc = 2, NOc
         DO  IOc = 1, KOc - 1
            DO IVr = 1, NVr
               DO JOc = 1, NOc
                  DumA = G4(KOc,IOc,JOc,NOc+IVr)
                  IKOc = ((KOc - 1) * (KOc - 2)) / 2 + IOc
                  DumB = G3(JOc,IVr,IKOc)
                  IF (ABS(DumA - DumB) > Middle) THEN
                     WRITE(*, '(" 10 Error(", 4I3, ")= ", 2F12.6)') IVr, JOc, IOc, KOc, DumA, DumB
                  END IF
                  Error = Error + ABS(DumA - DumB)
               END DO
            END DO
         END DO
      END DO
      WRITE(*, '(" --- Error for Type 10 int: ", F20.14)') Error
      DEALLOCATE(G3)
!
      DEALLOCATE(G4)
!
      END SUBROUTINE
