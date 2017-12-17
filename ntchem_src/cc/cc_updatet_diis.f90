      SUBROUTINE CC_UpdateT_DIIS(Iter)
!
      USE CC_Module, ONLY : Name, NOc, NVr, Ene, MaxDIIS, NMax, NoT1
      USE CC_Constant_Module, ONLY : Zero, One
!
!     o DIIS extrapolation of T vectors
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: Iter
!
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      INTEGER, PARAMETER :: IO1 = 98
      INTEGER, PARAMETER :: IO2 = 97
      INTEGER :: NRank
      INTEGER :: NDIIS
      INTEGER :: IOLen, LocRec
      INTEGER :: NSize
      INTEGER :: I, J, N
      INTEGER :: CCLib_NComb
      INTEGER :: Info   ! LAPACK
      INTEGER, ALLOCATABLE :: IPiv(:)   ! LAPACK
      REAL(8), ALLOCATABLE :: RWork1A(:), RWork1B(:)
      REAL(8), ALLOCATABLE :: ADIIS(:,:), BDIIS(:)
      REAL(8) :: DDOT
!
      IF (Iter <= MaxDIIS) THEN
         NDIIS = Iter
      ELSE
         NDIIS = MaxDIIS
      END IF
      LocRec = MOD(Iter, MaxDIIS)
      IF (LocRec == 0) LocRec = MaxDIIS
      ALLOCATE(ADIIS(NDIIS+1,NDIIS+1))
      CALL DCOPY((NDIIS+1)*(NDIIS+1), Zero, 0, ADIIS, 1)
!
      DO NRank = 1, NMax
         IF ((NRank == 1) .AND. NoT1) CYCLE
!
         NSize = CCLib_NComb(NVr,NRank) * CCLib_NComb(NOc,NRank)
         ALLOCATE(RWork1A(NSize))
         ALLOCATE(RWork1B(NSize))
!
!        o Read latest T and Delta-T vector
!
         CALL CCLib_Int2Char(NRank, c1, "0")
         FBuf = TRIM(Name)//".T"//TRIM(c1)
         OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
         READ(IO1) RWork1A(1:NSize)
         CLOSE(IO1)
         FBuf = TRIM(Name)//".W"//TRIM(c1)
         OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
         READ(IO1) RWork1B(1:NSize)
         CLOSE(IO1)
!
!        o Save latest T and Delta-T into DIIS files
!
         CALL CCLib_Int2Char(NRank, c1, "0")
         IOLen = NSize * 8   ! 8 byte
         FBuf = TRIM(Name)//".T"//TRIM(c1)//"DIIS"
         OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', ACCESS='DIRECT', FORM='UNFORMATTED', RECL=IOLen)
         WRITE(IO1, REC=LocRec) RWork1A(1:NSize)
         CLOSE(IO1)
         FBuf = TRIM(Name)//".W"//TRIM(c1)//"DIIS"
         OPEN(UNIT=IO2, FILE=TRIM(FBuf), STATUS='UNKNOWN', ACCESS='DIRECT', FORM='UNFORMATTED', RECL=IOLen)
         WRITE(IO2, REC=LocRec) RWork1B(1:NSize)
!
!        o Form DIIS matrix
!
         DO I = 1, NDIIS
            READ(IO2, REC=I) RWork1A(1:NSize)
            DO J = 1, I
               READ(IO2, REC=J) RWork1B(1:NSize)
               ADIIS(I,J) = ADIIS(I,J) + DDOT(NSize, RWork1A, 1, RWork1B, 1)
               ADIIS(J,I) = ADIIS(I,J)
            END DO
            ADIIS(I,NDIIS+1) = -One
            ADIIS(NDIIS+1,I) = -One
         END DO
         ADIIS(NDIIS+1,NDIIS+1) = Zero
!
         CLOSE(IO2)
         DEALLOCATE(RWork1A)
         DEALLOCATE(RWork1B)
      END DO   ! NRank
!
!     o RHS vector
!
      ALLOCATE(BDIIS(NDIIS+1))
      DO I = 1, NDIIS
         BDIIS(I) = Zero
      END DO
      BDIIS(NDIIS+1) = -One
!
!     o Solve linear equation
!
      Info = 0
      ALLOCATE(IPiv(NDIIS+1))
      CALL DGESV((NDIIS+1), 1, ADIIS, (NDIIS+1), IPiv, BDIIS, (NDIIS+1), Info)
      IF (Info /= 0) STOP 'Error: DGESV in DIIS'
      DEALLOCATE(IPiv)
      DEALLOCATE(ADIIS)
!
!     o Extrapolation
!
      DO NRank = 1, NMax
         IF ((NRank == 1) .AND. NoT1) CYCLE
!
         CALL CCLib_Int2Char(NRank, c1, "0")
         NSize = CCLib_NComb(NVr,NRank) * CCLib_NComb(NOc,NRank)
         ALLOCATE(RWork1A(NSize))
         ALLOCATE(RWork1B(NSize))
!
!        o Open DIIS file
!
         IOLen = NSize * 8   ! 8 byte
         FBuf = TRIM(Name)//".T"//TRIM(c1)//"DIIS"
         OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', ACCESS='DIRECT', FORM='UNFORMATTED', RECL=IOLen)
         READ(IO1, REC=1) RWork1A(1:NSize)
         CALL DSCAL(NSize, BDIIS(1), RWork1A, 1)
         DO I = 2, NDIIS
            READ(IO1, REC=I) RWork1B(1:NSize)
            CALL DAXPY(NSize, BDIIS(I), RWork1B, 1, RWork1A, 1)
         END DO
         CLOSE(IO1)
!
!        o Save extrapolation for T
!
         FBuf = TRIM(Name)//".T"//TRIM(c1)
         OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
         REWIND(IO1)
         WRITE(IO1) (RWork1A(N), N = 1, NSize)
         CLOSE(IO1)
!
         DEALLOCATE(RWork1A)
         DEALLOCATE(RWork1B)
      END DO
!
      DEALLOCATE(BDIIS)
!
      END SUBROUTINE
