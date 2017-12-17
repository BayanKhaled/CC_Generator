      SUBROUTINE CC_CheckConv(DECorr, Conv, RMS)
!
      USE CC_Module, ONLY : Name, NMax, NOc, NVr, ThrEne, ThrGrd, NoT1
      USE CC_Constant_Module, ONLY : Zero
!
!     o Check convergence of CC iteration
!
      IMPLICIT NONE
!
      LOGICAL, INTENT(OUT) :: Conv
      REAL(8), INTENT(IN) :: DECorr
      REAL(8), INTENT(OUT) :: RMS
!
      CHARACTER(LEN=255) :: FBuf
      CHARACTER(LEN=2) :: c1
      INTEGER, PARAMETER :: IO = 90
      LOGICAL :: EConv
      INTEGER :: NRank, NSize, NSizeT, CCLib_NComb
      INTEGER :: N
      INTEGER :: IDAMAX
      REAL(8), ALLOCATABLE :: W(:)
      REAL(8) :: DNorm
      REAL(8) :: DDOT
!
!     o Check energy convergence
!
      EConv = (ABS(DECorr) < ThrEne)
!
!     o Read W vectors
!
      NSize = 0
      DNorm = Zero
      DO NRank = 1, NMax
         IF ((NRank == 1) .AND. NoT1) CYCLE
         NSizeT = CCLib_NComb(NVr, NRank) * CCLib_NComb(NOc, NRank)
         ALLOCATE(W(NSizeT))
         CALL CCLib_Int2Char(NRank, c1, "0")
         FBuf = TRIM(Name)//".W"//TRIM(c1)
         OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
         REWIND(IO)
         READ(IO) (W(N), N = 1, NSizeT)
         CLOSE(IO)
!
!        o RMS
!
         DNorm = DNorm + DDOT(NSizeT, W, 1, W, 1)
         NSize = NSize + NSizeT
!
!        o Max
!
!Temp         DNorm = MAX(ABS(W(IDAMAX(NSizeT, W, 1))), DNorm)
!
         DEALLOCATE(W)
      END DO
!
      RMS = SQRT(DNorm / DBLE(NSize))   ! RMS
!Temp      RMS = DNorm   ! Max
      Conv = (RMS < ThrGrd) .AND. EConv
!
      END SUBROUTINE
