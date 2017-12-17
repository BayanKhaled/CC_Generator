      SUBROUTINE CC_ECorr(ECorr)
!
      USE CC_Module, ONLY : Name, NOc, NVr, NMax, NoT1
      USE CC_Constant_Module, ONLY : Zero
!
!     o Coupled-cluster Correlation energy
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: ECorr
!
      INTEGER, PARAMETER :: IO = 90
      CHARACTER(LEN=255):: FBuf
      INTEGER :: NSizeG, NSizeT1, NSizeT2, CCLib_NComb, N
      REAL(8), ALLOCATABLE :: T(:), G(:)
      REAL(8) :: DDOT
!
      IF (NMax == 1) THEN
         ECorr = Zero
         RETURN
      END IF
!
      NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)
      NSizeT2 = NSizeG
      ALLOCATE(G(NSizeG))
      ALLOCATE(T(NSizeT2))
!
!     o Load type-11 MO integral
!
      FBuf = TRIM(Name)//".INT11"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      READ(IO) (G(N), N = 1, NSizeG)
      CLOSE(IO, STATUS='KEEP')
!
!     o Load T2 amplitude
!
      FBuf = TRIM(Name)//".T02"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      READ(IO) (T(N), N = 1, NSizeT2)
      CLOSE(IO, STATUS='KEEP')
!
!     o Corr. Ene.: T2 contribution
!
      ECorr = DDOT(NSizeT2, T, 1, G, 1)
!
      DEALLOCATE(T)
!
      IF (NoT1) THEN
         DEALLOCATE(G)
         RETURN
      END IF
!
      NSizeT1 = NOc * NVr
      ALLOCATE(T(NSizeT1))
!
!     o Load T1 amplitude
!
      FBuf = TRIM(Name)//".T01"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      READ(IO) (T(N), N = 1, NSizeT1)
      CLOSE(IO, STATUS='KEEP')
!
!     o Two-electron T1 contribution
!
      CALL CC_ECorr_T1G(G, T, ECorr)
!
      DEALLOCATE(G)
!
      NSizeG = NSizeT1
      ALLOCATE(G(NSizeG))
!
!     o One-electron contribution
!
      FBuf = TRIM(Name)//".INT03"
      OPEN(UNIT=IO, FILE=TRIM(FBuf), STATUS='OLD', FORM='UNFORMATTED')
      READ(IO) (G(N), N = 1, NSizeG)
      CLOSE(IO, STATUS='KEEP')
!
      ECorr = ECorr + DDOT(NSizeT1, T, 1, G, 1)
!
      END SUBROUTINE
