      MODULE CC_Module
!
!     o Basic info. 
!
!     NMOA and NMOB (#'s of alpha- and beta-orbitals) are equal to NMO/2
!     NOcA + NOcB = NOc
!     NVrA + NVrB = NVr
!     NBF : # of basis functions
!
      CHARACTER(LEN=255) :: Name, WfnName
      INTEGER :: NBF
      INTEGER :: NMO
      INTEGER :: NOc, NVr, NMOA, NMOB, NOcA, NOcB, NVrA, NVrB
      INTEGER :: NFrzOA, NFrzOB, NFrzVA, NFrzVB, NFrzO, NFrzV
      INTEGER :: NMax        ! Level of truncation of cluster operator
      INTEGER :: MaxIter     ! Maximum # of CC iterations
      REAL(8) :: ECorr       ! Correlation energy
      REAL(8) :: EneRef      ! Reference energy
      REAL(8) :: ThrEne      ! Threshold for energy
      REAL(8) :: ThrGrd      ! Threshold for RMS gradient
      REAL(8), ALLOCATABLE :: CMO(:,:), CMOA(:,:), CMOB(:,:), Ene(:)
!
      LOGICAL :: UHF
      LOGICAL :: DoTran
      LOGICAL :: T1Trn            ! TRUE if T1-transformed integrals are used
      LOGICAL :: NoT1             ! TRUE if T1 operator should be ignored
      LOGICAL :: Bruck            ! TRUE if Brueckner orbitals are used
      LOGICAL :: CC2              ! TRUE for CC2
!
!     o Flag for MP3 and MP4(S) energies
!
      LOGICAL :: CalcMP
!
!     o Variables for string addressing
!
      INTEGER, ALLOCATABLE :: IArcWgtOcc(:,:,:), IArcWgtVir(:,:,:)
!
!     o Conventional SCF
!
      INTEGER, PARAMETER :: MaxBuf = 15000
      INTEGER, ALLOCATABLE :: Lp(:), Lq(:), Lr(:), Ls(:)
      REAL(8), ALLOCATABLE :: ERIBuf(:)
      INTEGER :: NBuf
      INTEGER :: NLine
!
!     o The followings are for SOCC
!
      INTEGER :: NBF2   ! NBF2 = NBF + NBF
      COMPLEX(8):: ECorrSO
      COMPLEX(8), ALLOCATABLE :: CMOSO(:,:)
      REAL(8), ALLOCATABLE :: EneSO(:)
!
!     o Initial guess for T amplitude
!
      CHARACTER(LEN=4) :: Guess
!
!     o DIIS acceleration
!
      LOGICAL :: DoDIIS
      INTEGER :: MaxDIIS
!
!     o Active-space CC (temporary)
!
      CHARACTER(LEN=2) :: ASCC   ! 'HM' or 'LR' or ''
      INTEGER :: NOccModelA, NVirModelA, NOccModelB, NVirModelB
      INTEGER :: NFrzOAM, NFrzVAM, NFrzOBM, NFrzVBM
!
!     o Print option
!
      INTEGER :: IPrint
!
      END MODULE
