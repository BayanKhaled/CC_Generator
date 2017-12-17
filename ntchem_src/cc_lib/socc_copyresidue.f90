      SUBROUTINE SOCC_CopyResidue(AName, NRank)
!
      USE CC_Module, ONLY : NOc, NVr, Name
!
!     o Copy residues Name.V1 to Name.AName_NRank
!
      IMPLICIT NONE
!
      CHARACTER(*), INTENT(IN) :: AName
      INTEGER, INTENT(IN) :: NRank
!
      INTEGER, PARAMETER :: IO = 90
      CHARACTER(LEN=2) :: c1
      CHARACTER(LEN=255) :: FBuf
      INTEGER :: NSize, N, CCLib_NComb
      COMPLEX(8), ALLOCATABLE :: V(:)
!
      NSize  = CCLib_NComb(NOc, NRank) * CCLib_NComb(NVr, NRank)
      ALLOCATE(V(NSize))
!
      CALL SOCC_ReadV(NSize, V, 'V1')
!
      CALL CCLib_Int2Char(NRank, c1, "0")
!      FBuf = TRIM(Name)//".W"//TRIM(c1)
      FBuf = TRIM(Name)//"."//TRIM(AName)//TRIM(c1)
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='UNKNOWN', ERR=9000, FORM='UNFORMATTED')
      GO TO 9100
 9000 STOP 'Error: in opening residue file in -SOCC_CopyResidue-'
 9100 CONTINUE
      REWIND(IO)
      WRITE(IO) (V(N), N = 1, NSize)
      CLOSE(IO)
      DEALLOCATE(V)
!
      END SUBROUTINE
