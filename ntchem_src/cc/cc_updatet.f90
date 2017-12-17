      SUBROUTINE CC_UpdateT(NRank)
!
      USE CC_Module, ONLY : NOc, NVr, DoDIIS, Name
!
!     o Update CC amplitudes
!
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NRank
!
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      INTEGER, PARAMETER :: IO = 90
      INTEGER :: NSize, N, CCLib_NComb
      REAL(8), ALLOCATABLE :: T(:), W(:)
!
      NSize = CCLib_NComb(NOc, NRank) * CCLib_NComb(NVr, NRank)
      ALLOCATE(T(NSize))
      ALLOCATE(W(NSize))
!
      CALL CCLib_Int2Char(NRank, c1, "0")
!
!     o Read T amplitude and residue W from disk
!
      FBuf = TRIM(Name)//".T"//TRIM(c1)
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      REWIND(IO)
      READ(IO) (T(N), N = 1, NSize)
      CLOSE(IO)
!
      FBuf = TRIM(Name)//".W"//TRIM(c1)
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      REWIND(IO)
      READ(IO) (W(N), N = 1, NSize)
      CLOSE(IO)
!
!     o Quasi-Newton update of T vectors
!
      SELECT CASE (NRank)
         CASE (1)
!ActiveCC            CALL CC_UpdateT1_QN(W, T)
            CALL CC_UpdateT1_QN_ASCC(W, T)
         CASE (2)
!ActiveCC            CALL CC_UpdateT2_QN(W, T)
            CALL CC_UpdateT2_QN_ASCC(W, T)
         CASE (3)
!ActiveCC            CALL CC_UpdateT3_QN(W, T)
            CALL CC_UpdateT3_QN_ASCC(W, T)
         CASE (4)
!ActiveCC            CALL CC_UpdateT4_QN(W, T)
            CALL CC_UpdateT4_QN_ASCC(W, T)
         CASE (5)
!ActiveCC            CALL CC_UpdateT5_QN(W, T)
            CALL CC_UpdateT5_QN_ASCC(W, T)
         CASE DEFAULT
            STOP 'NYI: CC_UpdateT?_QN_ASCC'
      END SELECT
!
!     o Write W and new T to disk
!
      FBuf = TRIM(Name)//".T"//TRIM(c1)
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
      REWIND(IO)
      WRITE(IO) (T(N), N = 1, NSize)
      CLOSE(IO)
      IF (DoDIIS) THEN
         FBuf = TRIM(Name)//".W"//TRIM(c1)
         OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', FORM='UNFORMATTED')
         REWIND(IO)
         WRITE(IO) (W(N), N = 1, NSize)
         CLOSE(IO)
      END IF
!
      DEALLOCATE(T)
      DEALLOCATE(W)
!
      END SUBROUTINE
