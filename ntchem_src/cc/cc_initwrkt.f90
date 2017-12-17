      SUBROUTINE CC_InitWrkT
!
      USE CC_Module, ONLY : NMax, Name, NOc, NVr, ASCC
!
!     o Initialize T amplitudes
!
      IMPLICIT NONE
!
      INTEGER, PARAMETER :: IO = 90
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      LOGICAL :: Exist1, Exist2
      INTEGER :: NRank, N, NSize, CCLib_NComb
      REAL(8), ALLOCATABLE :: T(:)
!
!ActiveCC_Bgn
      IF (TRIM(ASCC) == 'HM') THEN
         FBuf = TRIM(Name)//".T01"
         INQUIRE(FILE=TRIM(FBuf), EXIST=Exist1)
         FBuf = TRIM(Name)//".T02"
         INQUIRE(FILE=TRIM(FBuf), EXIST=Exist2)
         IF ((.NOT. Exist1) .OR. (.NOT. Exist2)) THEN
            WRITE(*, '(" Prepare T01 and T02 files for HM calculation")')
            STOP
         END IF
         DO NRank = 3, NMax
            NSize = CCLib_NComb(NOc, NRank) * CCLib_NComb(NVr, NRank)
            ALLOCATE(T(NSize))
            CALL CCLib_DClear(T, NSize)
            CALL CCLib_Int2Char(NRank, c1, "0")
            FBuf = TRIM(Name)//".T"//TRIM(c1)
            OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
            WRITE(IO) (T(N), N = 1, NSize)
            CLOSE(IO, STATUS='KEEP')
            DEALLOCATE(T)
         END DO
!ActiveCC_End
      ELSE
         DO NRank = 1, NMax
            NSize = CCLib_NComb(NOc, NRank) * CCLib_NComb(NVr, NRank)
            ALLOCATE(T(NSize))
            CALL CCLib_DClear(T, NSize)
            IF (NRank == 2) CALL CC_T2init(T)   ! Initialize T2 by MP2 amplitude
            CALL CCLib_Int2Char(NRank, c1, "0")
            FBuf = TRIM(Name)//".T"//TRIM(c1)
            OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
            WRITE(IO) (T(N), N = 1, NSize)
            CLOSE(IO, STATUS='KEEP')
            DEALLOCATE(T)
         END DO
      END IF
!
      END SUBROUTINE
