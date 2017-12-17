      SUBROUTINE CC_GetMOandE
!
      USE CC_Module, ONLY : NBF, NOcA, NOcB, NVrA, NVrB, UHF, &
    &   NOc, NVr, NMO, NMOA, NMOB, NFrzOA, NFrzOB, NFrzVA, NFrzVB, NFrzO, NFrzV, &
    &   Name, CMOA, CMOB, Ene, CMO
!
!     o Get MO coefficients and orbital energies
!
      IMPLICIT NONE
!
      CHARACTER(LEN=255) :: FBuf
      LOGICAL :: Debug = .FALSE.
      INTEGER, PARAMETER :: IO1 = 90
      INTEGER :: N, IBF, NOcARef, NOcBRef, NOcRef, NVrARef, NVrBRef, NVrRef, NMOARef, NMOBRef, NMORef
      REAL(8), ALLOCATABLE :: RWork1(:), RWork2A(:,:), RWork2B(:,:)
!
      NOcARef = NOcA + NFrzOA
      NOcBRef = NOcB + NFrzOB
      NOcRef = NOcARef + NOcBRef
      NVrARef = NVrA + NFrzVA
      NVrBRef = NVrB + NFrzVB
      NVrRef = NVrARef + NVrBRef
      NMOARef = NOcARef + NVrARef
      NMOBRef = NOcBRef + NVrBRef
      NMORef = NMOARef + NMOBRef
      ALLOCATE(RWork2A(NBF,NMOARef))
      ALLOCATE(RWork2B(NBF,NMOBRef))
      ALLOCATE(RWork1(NMORef))
!
!     o Read MO coefficients
!
      FBuf = TRIM(Name)//".MO"
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='UNFORMATTED')
!FC      READ(IO1) CMOA(1:NBF,1:NMOA)
      READ(IO1) RWork2A(1:NBF,1:NMOARef)
      IF (UHF) THEN
!FC         READ(IO1) CMOB(1:NBF,1:NMOB)
         READ(IO1) RWork2B(1:NBF,1:NMOBRef)
      ELSE
!FC         CALL DCOPY(NBF*NMOA, CMOA, 1, CMOB, 1)
         CALL DCOPY(NBF*NMOARef, RWork2A, 1, RWork2B, 1)
      END IF
      CLOSE(IO1)
!
!FC_Bgn
      DO N = 1, NMOA
         CALL DCOPY(NBF, RWork2A(1,NFrzOA+N), 1, CMOA(1,N), 1)
      END DO
      DO N = 1, NMOB
         CALL DCOPY(NBF, RWork2B(1,NFrzOB+N), 1, CMOB(1,N), 1)
      END DO
      DEALLOCATE(RWork2A)
      DEALLOCATE(RWork2B)
!FC_End
!
!     o Form CMO in spin-orbital basis
!
      CALL CCLib_DClear(CMO, NBF*NMO)
      CALL DCOPY(NBF*NOcA, CMOA, 1, CMO, 1)
      CALL DCOPY(NBF*NOcB, CMOB, 1, CMO(1,NOcA+1), 1)
      CALL DCOPY(NBF*NVrA, CMOA(1,NOcA+1), 1, CMO(1,NOc+1), 1)
      CALL DCOPY(NBF*NVrB, CMOB(1,NOcB+1), 1, CMO(1,NOc+NVrA+1), 1)
!
      FBuf = TRIM(Name)//".OrbEne"
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='UNFORMATTED')
!FC      READ(IO1) Ene(1:NOcA), Ene(NOc+1:NOc+NVrA)
      READ(IO1) RWork1(1:NOcARef), RWork1(NOcRef+1:NOcRef+NVrARef)
      IF (UHF) THEN
!FC         READ(IO1) Ene(NOcA+1:NOc), Ene(NOc+NVrA+1:NMO)
         READ(IO1) RWork1(NOcARef+1:NOcRef), RWork1(NOcRef+NVrARef+1:NMORef)
      ELSE
!FC         CALL DCOPY(NOcB, Ene, 1, Ene(NOcA+1), 1)
!FC         CALL DCOPY(NVrB, Ene(NOc+1), 1, Ene(NOc+NVrA+1), 1)
         CALL DCOPY(NOcBRef, RWork1, 1, RWork1(NOcARef+1), 1)
         CALL DCOPY(NVrBRef, RWork1(NOcRef+1), 1, RWork1(NOcRef+NVrARef+1), 1)
      END IF
      CLOSE(IO1)
!
!FC_Bgn
      CALL DCOPY(NOcA, RWork1(NFrzOA+1), 1, Ene, 1)
      CALL DCOPY(NOcB, RWork1(NOcARef+NFrzOB+1), 1, Ene(NOcA+1), 1)
      CALL DCOPY(NVrA, RWork1(NOcRef+1), 1, Ene(NOc+1), 1)
      CALL DCOPY(NVrB, RWork1(NOcRef+NVrARef+1), 1, Ene(NOc+NVrA+1), 1)
      DEALLOCATE(RWork1)
!FC_End
!
      IF (Debug) THEN
         WRITE(*, '(" *** Alpha MO coefficients (Occ.)")')
         DO N = 1, NOcA
            DO IBF = 1, NBF
               WRITE(*, '(X, I3, X, F12.6)') IBF, CMOA(IBF,N)
            END DO
         END DO
         WRITE(*, '(" *** Alpha MO coefficients (Vir.)")')
         DO N = 1, NVrA
            DO IBF = 1, NBF
               WRITE(*, '(X, I3, X, F12.6)') IBF, CMOA(IBF,NOcA+N)
            END DO
         END DO

         WRITE(*, '(" *** Beta MO coefficients (Occ.)")')
         DO N = 1, NOcB
            DO IBF = 1, NBF
               WRITE(*, '(X, I3, X, F12.6)') IBF, CMOB(IBF,N)
            END DO
         END DO
         WRITE(*, '(" *** Beta MO coefficients (Vir.)")')
         DO N = 1, NVrB
            DO IBF = 1, NBF
               WRITE(*, '(X, I3, X, F12.6)') IBF, CMOB(IBF,NOcB+N)
            END DO
         END DO
!
         WRITE(*, '(" *** Obital energy (alpha) ***")')
         DO N = 1, NOcA
            WRITE(*, '(5X, I5, F12.6)') N, Ene(N)
         END DO
         DO N = 1, NVrA
            WRITE(*, '(5X, I5, F12.6)') N, Ene(NOc+N)
         END DO
         WRITE(*, '(" *** Obital energy (beta) ***")')
         DO N = 1, NOcB
            WRITE(*, '(5X, I5, F12.6)') N, Ene(NOcA+N)
         END DO
         DO N = 1, NVrB
            WRITE(*, '(5X, I5, F12.6)') N, Ene(NOc+NVrA+N)
         END DO
      END IF
!
      END SUBROUTINE
