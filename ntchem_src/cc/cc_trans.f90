      SUBROUTINE CC_Trans
!
      USE CC_Module, ONLY : UHF, NOcA, NOcB, NOc, NVr, NMO, NMOA, NMOB, Name, Ene, NBF, CalcMP, CMOA, CMOB
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o MO integral generation
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FName, FBuf
      INTEGER :: NSizeG1, NSizeG2, N
      INTEGER :: NMOC, NOc2, NVr2, NBC
      INTEGER :: IMO, JMO, IJMO
      REAL(8), ALLOCATABLE :: G1(:), G2(:)
      REAL(8), ALLOCATABLE :: FockTot(:,:), FckTot2(:,:), FckOn(:,:), FckHlf(:,:)
!
!     o 1-electron Hamiltonian matrix
!
!      NSizeG1 = NMO * NMO
!      NSizeG2 = (NBF * (NBF + 1)) / 2
!      ALLOCATE(G1(NSizeG1))
!      ALLOCATE(G2(NSizeG2))
!
!     o Load AO Hcore matrix
!
!      FName = TRIM(Name)//".HCore"
!      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
!      READ(IO1) (G2(N), N = 1, NSizeG2)
!      CLOSE(IO1, STATUS='KEEP')
!      WRITE(*, '(" ..... Hcore matrix in AO basis has been loaded from disk.")')
!
!      CALL CC_Trn_1eMat(G1,G2)
!
!     o Save MO Hcore matrix
!
!      FName = TRIM(Name)//".HCoreMO"
!      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
!      WRITE(IO1) (G1(N), N = 1, NSizeG1)
!      CLOSE(IO1, STATUS='KEEP')
!      WRITE(*, '(" ..... Hcore matrix in MO basis has been saved on disk.")')
!
!      DEALLOCATE(G1)
!      DEALLOCATE(G2)
!
!     o Allocate large arrays
!
      IF (CalcMP) THEN
         NsizeG1 = NMO * NMO * NMO * NMO
         NsizeG2 = NMO * NMO * NMO * NMO
         ALLOCATE(G1(NsizeG1))
         ALLOCATE(G2(NsizeG2))
      ELSE
         NBC = ((NBF + 1) * NBF) / 2
         NOc2 = ((NOc - 1) * NOc) / 2
         NVr2 = ((NVr - 1) * NVr) / 2
         NMOC = ((NMO - 1) * NMO) / 2
!
         NSizeG1 = NBC * NBF * NVr
         NSizeG1 = MAX(NSizeG1, (NBF * NVr * NVr2))
         NSizeG1 = MAX(NSizeG1, (NBC * NBF * NOc))
         NSizeG1 = MAX(NSizeG1, (NBF * NOc * NOc2))
         NSizeG1 = MAX(NSizeG1, (NBF * NMO * NOc * NVr))
         NSizeG1 = MAX(NSizeG1, (NBF * NOc * NVr * NVr))
         NSizeG1 = MAX(NSizeG1, (NBF * NOc * NOc * NVr))
         NSizeG1 = MAX(NSizeG1, (NVr2 * NVr2))
         NSizeG1 = MAX(NSizeG1, (NOc2 * NOc2))
         NSizeG1 = MAX(NSizeG1, (NVr * NVr * NOc * NOc))
         NSizeG1 = MAX(NSizeG1, (NOc * NVr * NVr2))
         NSizeG1 = MAX(NSizeG1, (NOc * NVr * NOc2))
!
         NSizeG2 = NBF * NBF * NVr2
         NSizeG2 = MAX(NSizeG2, (NBF * NBF * NOc2))
         NSizeG2 = MAX(NSizeG2, (NVr * NVr * NVr2))
         NSizeG2 = MAX(NSizeG2, (NOc * NOc * NOc2))
         NSizeG2 = MAX(NSizeG2, (NBF * NBF * NOc * NVr))
         NSizeG2 = MAX(NSizeG2, (NMO * NMO * NOc * NVr))
         NSizeG2 = MAX(NSizeG2, (NOc * NVr * NVr * NVr))
         NSizeG2 = MAX(NSizeG2, (NOc * NOc * NOc * NVr))
         NSizeG2 = MAX(NSizeG2, (NVr * NVr * NOc2))
!
         ALLOCATE(G1(NsizeG1))
         ALLOCATE(G2(NsizeG2))
      END IF
!
      IF (CalcMP) THEN
!
!        o General (PQ||RS) integrals (Type 00)
!
         CALL CC_Trn1_Pqrs(G1)
         CALL CC_Trn2_PqRs(G1, G2)
         CALL CC_Trn3_PQRs(G1, G2)
         CALL CC_Trn4_PQRS(G1, G2)
!
!        o Save PQRS integrals
!
         FName = TRIM(Name)//".INT00P"
         OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
         WRITE(IO1) (G2(N), N = 1, (NMO * NMO * NMO * NMO))
         CLOSE(IO1, STATUS='KEEP')
         WRITE(*, '(" ..... Plane type 00 integrals saved on disk.")')
!
         CALL CC_AntiSym_PQRS(G1, G2)
!
!        o Save anti-symmetrized PQRS integrals
!
         FName = TRIM(Name)//".INT00"
         OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
         WRITE(IO1) (G1(N), N = 1, (NMO * NMO * NMO * NMO))
         CLOSE(IO1, STATUS='KEEP')
         WRITE(*, '(" ..... Antisymmetrized type 00 integrals saved on disk.")')
!
      END IF
!
!     o Generate type 4 interaction vertex
!     o First transformation (Aq|rs)
!
      CALL CC_Trn1_Cqrs(G1)
!
!     o Sscond transformation (Aq|Ds)
!
      CALL CC_Trn2_CqBs(G1, G2)
!
!     o Third transformation (Aq|DC)
!
      CALL CC_Trn3_CDBs(G1, G2)
!
!     o Fourth transformation (AB|DC)
!
      CALL CC_Trn4_CDBA(G1, G2)
!
!     o Anti-symmetrize
!
      CALL CC_AntiSym_CDBA(G1, G2)
!
!     o Save anti-symmetrized type 4 integrals
!
      FName = TRIM(Name)//".INT04"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, ((NVr * (NVr - 1)) / 2 * (NVr * (NVr - 1)) / 2))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 04 integrals saved on disk.")')
!
!     o Generate type 5 interaction vertex
!     o First transformation (Jq|rs)
!
      CALL CC_Trn1_Lqrs(G1)
!
!     o Sscond transformation 
!
      CALL CC_Trn2_LqIs(G1, G2)
!
!     o Third transformation 
!
      CALL CC_Trn3_LKIs(G1, G2)
!
!     o Fourth transformation 
!
      CALL CC_Trn4_LKIJ(G1, G2)
!
!     o Anti-symmetrize
!
      CALL CC_AntiSym_LKIJ(G1, G2)
!
!     o Save type 5 integrals
!
      FName = TRIM(Name)//".INT05"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, ((NOc * (NOc - 1)) / 2 * (NOc * (NOc - 1)) / 2))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 05 integrals saved on disk.")')
!
!     o Generate type 6 interaction vertex
!     o First transformation (Iq|rs)
!
      CALL CC_Trn1_Lqrs(G1)
!
!     o Second transformation
!
      CALL CC_Trn2_IqAs(G1, G2)
!
!     o Third transformation
!
      CALL CC_Trn3_IQAs(G1, G2)
!
!     o Fourth transformation
!
      CALL CC_Trn4_IQAP(G1, G2)
!
!     o Anti-symmetrize
!
      CALL CC_AntiSym_IJAB(G1, G2)
!
!     o Save antisymmetrized type 6 integrals
!
      FName = TRIM(Name)//".INT06"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NOc * NVr * NOc * NVr))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 06 integrals saved on disk.")')
!
!     o Generate type 7 interaction vertex
!     o First transformation (Cq|rs)
!
      CALL CC_Trn1_Cqrs(G1)
!
!     o Second transformation (Iq|Cs)
!
      CALL CC_Trn2_CqIs(G1, G2)
!
!     o Third transformation (Iq|CB)
!
      CALL CC_Trn3_CqIA(G1, G2)
!
!     o Fourth transformation (IA|CB)
!
      CALL CC_Trn4_CBIA(G1, G2)
!
!     o Anti-symmetrize (A <--> B)
!
      CALL CC_AntiSym_CBIA(G1, G2)
!
!     o Save anti-symmetrized type-7 integrals
!
      FName = TRIM(Name)//".INT07"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NOc * (NVr * (NVr - 1)) / 2 * NVr))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 07 integrals saved on disk.")')
!
!     o Generate type 8 interaction vertex
!
      CALL CC_Trn1_Lqrs(G1)
!
!     o Second transformation
!
      CALL CC_Trn2_IqAs(G1, G2)
!
!     o Third transformation
!
      CALL CC_Trn3_IqAK(G1, G2)
!
!     o Fourth trnasformation
!
      CALL CC_Trn4_IJAK(G1, G2)
!
!     o Anti-symmetrize (K <--> J)
!
      CALL CC_AntiSym_IJAK(G1, G2)
!
!     o Save anti-symmetrized type-8 integrals
!
      FName = TRIM(Name)//".INT08"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NVr * (NOc * (NOc - 1)) / 2 * NOc))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 08 integrals saved on disk.")')
!
!     o Generate type 9 interaction vertex
!
      CALL CC_Trn1_Lqrs(G1)
!
!     o Second transformation
!
      CALL CC_Trn2_IqAs(G1, G2)
!
!     o Third transformation
!
      CALL CC_Trn3_IqBC(G1, G2)
!
!     o Fourth transformation
!
      CALL CC_Trn4_IABC(G1, G2)
!
!     o Anti-symmetrize (A <--> C)
!
      CALL CC_AntiSym_IABC(G1, G2)
!
!     o Save anti-symmetrized type-9 integrals
!
      FName = TRIM(Name)//".INT09"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NVr * (NVr * (NVr - 1)) / 2 * NOc))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 09 integrals saved on disk.")')
!
!     o Generate type 10 interaction vertex (Transposition of Type-08 integral array)
!
      FName = TRIM(Name)//".INT08"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      READ(IO1) (G1(N), N = 1, (NOc * (NOc * (NOc - 1)) / 2 * NVr))
      CLOSE(IO1, STATUS='KEEP')
!
      CALL CC_Gen08to10(G1, G2)
!
!     o Save anti-symmetrized type-10 integrals
!
      FName = TRIM(Name)//".INT10"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G2(N), N = 1, (NOc * NVr * (NOc * (NOc - 1)) / 2))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 10 integrals saved on disk.")')
!
!     o Generate type 11 interaction vertex
!
      CALL CC_Trn1_Lqrs(G1)
!
!     o Second transformation
!
      CALL CC_Trn2_LqIs(G1, G2)
!
!     o Third transformation
!
      CALL CC_Trn3_IAJs(G1, G2)
!
!     o Fourth transformation
!
      CALL CC_Trn4_IAJB(G1, G2)
!
!     o Anti-symmetrize
!
      CALL CC_AntiSym_IAJB(G1, G2)
!
!     o Save anti-symmetrized type-11 integrals
!
      FName = TRIM(Name)//".INT11"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, ((NVr * (NVr - 1)) / 2 * (NOc * (NOc - 1)) / 2))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 11 integrals saved on disk.")')
!
!     o Generate type 13 interaction vertex
!       The values are complex conjugate of type-11 integrals
!
      FName = TRIM(Name)//".INT13"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, ((NVr * (NVr - 1)) / 2 * (NOc * (NOc - 1)) / 2))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Antisymmetrized type 13 integrals saved on disk.")')
!
!     o Free large arrays
!
      DEALLOCATE(G1)
      DEALLOCATE(G2)
!
!     o One-electron vertices
!
      NBC = (NBF * (NBF + 1)) / 2
!
!     o Read AO Fock matrix
!
      ALLOCATE(FockTot(NBC,2))
      FBuf = TRIM(Name)//".FockTot"
      IF (.NOT. UHF) THEN
         CALL Util_ReadIO1(FBuf, FockTot, NBC, 1)
         CALL DCOPY(NBC, FockTot(1,1), 1, FockTot(1,2), 1)
      ELSE
         CALL Util_ReadIO1(FBuf, FockTot, NBC, 2)
      END IF
!
!     o AO -> MO transform
!
      ALLOCATE(FckOn(NMO,NMO))
      ALLOCATE(FckHlf(NMO,NBF))
      ALLOCATE(FckTot2(NBF,NBF))
      CALL CCLib_DClear(FckOn, NMO*NMO)
!
!     --- Alpha-alpha
!
      CALL Util_Tr1To2(FockTot(1,1), FckTot2, NBF)
      CALL DGEMM('T', 'N', NMOA, NBF, NBF, One, CMOA, NBF, FckTot2, NBF, &
     &   Zero, FckHlf, NMO)
      CALL DGEMM('N', 'N', NMOA, NMOA, NBF, One, FckHlf, NMO, CMOA, NBF, &
     &   Zero, FckOn, NMO)
!
!     --- Beta-beta
!
      CALL Util_Tr1To2(FockTot(1,2), FckTot2, NBF)
      CALL DGEMM('T', 'N', NMOB, NBF, NBF, One, CMOB, NBF, FckTot2, NBF, &
     &   Zero, FckHlf, NMO)
      CALL DGEMM('N', 'N', NMOB, NMOB, NBF, One, FckHlf, NMO, CMOB, NBF, &
     &   Zero, FckOn(NMOA+1,NMOA+1), NMO)
!
      DEALLOCATE(FockTot)
      DEALLOCATE(FckTot2)
      DEALLOCATE(FckHlf)
!
      ALLOCATE(G1(NMO*NMO))
!
!     o Type-1 vertex
!
      CALL CCLib_DClear(G1, NVr*NVr)
      IJMO = 0
      DO IMO = 1, NMO
         DO JMO = 1, NMO
            IF (((IMO > NOcA) .AND. (IMO <= NMOA)) .OR. (IMO > (NMOA + NOcB))) THEN
               IF (((JMO > NOcA) .AND. (JMO <= NMOA)) .OR. (JMO > (NMOA + NOcB))) THEN
                  IJMO = IJMO + 1
                  G1(IJMO) = FckOn(IMO,JMO)
               END IF
            END IF
         END DO
      END DO
!
      FName = TRIM(Name)//".INT01"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NVr * NVr))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Type 01 integrals saved on disk.")')
!
!     o Type-2 vertex
!
      CALL CCLib_DClear(G1, NOc*NOc)
      IJMO = 0
      DO IMO = 1, (NMOA + NOcB)
         DO JMO = 1, (NMOA + NOcB)
            IF ((IMO <= NOcA) .OR. (IMO > NMOA)) THEN
               IF ((JMO <= NOcA) .OR. (JMO > NMOA)) THEN
                  IJMO = IJMO + 1
                  G1(IJMO) = FckOn(IMO,JMO)
               END IF
            END IF
         END DO
      END DO
!
      FName = TRIM(Name)//".INT02"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NOc * NOc))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Type 02 integrals saved on disk.")')
!
!     o Type-3 vertex
!
      CALL CCLib_DClear(G1, NVr*NOc)
      IJMO = 0
      DO IMO = 1, (NMOA + NOcB)
         DO JMO = (NOcA + 1), NMO
            IF ((IMO <= NOcA) .OR. (IMO > NMOA)) THEN
               IF ((JMO <= NMOA) .OR. (JMO > (NMOA + NOcB))) THEN
                  IJMO = IJMO + 1
                  G1(IJMO) = FckOn(IMO,JMO)
               END IF
            END IF
         END DO
      END DO
!
      FName = TRIM(Name)//".INT03"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NVr * NOc))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Type 03 integrals saved on disk.")')
!
!     o Type-12 vertex
!
      CALL CCLib_DClear(G1, NVr*NOc)
      IJMO = 0
      DO IMO = 1, (NMOA + NOcB)
         DO JMO = (NOcA + 1), NMO
            IF ((IMO <= NOcA) .OR. (IMO > NMOA)) THEN
               IF ((JMO <= NMOA) .OR. (JMO > (NMOA + NOcB))) THEN
                  IJMO = IJMO + 1
                  G1(IJMO) = FckOn(JMO,IMO)
               END IF
            END IF
         END DO
      END DO
!
      FName = TRIM(Name)//".INT12"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='UNKNOWN', FORM='UNFORMATTED')
      WRITE(IO1) (G1(N), N = 1, (NVr * NOc))
      CLOSE(IO1, STATUS='KEEP')
      WRITE(*, '(" ..... Type 12 integrals saved on disk.")')
!
      DEALLOCATE(G1)
      DEALLOCATE(FckOn)
!
      END SUBROUTINE
