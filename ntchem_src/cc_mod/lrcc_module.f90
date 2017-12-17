      MODULE LRCC_Module
!
!     o Info for linear-response CC calculations
!
      CHARACTER(LEN=4) :: GuessR   ! Way of initialization of R vector (NONE or READ)
      LOGICAL :: GrndProp    ! Calculate ground-state properties
      LOGICAL :: ExcdProp    ! Calculate excited-state properties
      LOGICAL :: ExcdEne     ! Calculate excited-state energies
      LOGICAL :: IonzPot     ! Calculate ionization energies
      INTEGER :: NMaxR       ! Level of truncation of R operator
      INTEGER :: NState      ! # of excited states to be calculated
      INTEGER :: NStateIP    ! # of ionized states to be calculated
      INTEGER :: MaxItDiag   ! Maximum # of iterations for Jacobian diagonalization
      INTEGER :: NumVec      ! Initial # of expansion vectors in Jacobian diagonalization
      INTEGER :: NumVecIP    ! Initial # of expansion vectors in Jacobian diagonalization for IP
      INTEGER :: NumVecMax   ! Max. # of expansion vectors in Jacobian diagonalization
      INTEGER :: IStateProp  ! The excited state for which properties are requested
      REAL(8) :: ARTol       ! Convergence criterion for ARPACK diagonalization (residual norm)
      REAL(8) :: ThreDiagE   ! Energy threshold of convergence of davidson diagonalization
      REAL(8) :: RPrintTol   ! Threshold for printing R amplitudes
      REAL(8) :: ThreResidue ! Threshold for linear equations
!
      LOGICAL :: DoJacobDiag   ! Right diagonalize CC Jacobian
      LOGICAL :: DoJacobLeft   ! Left eigenvectors of CC Jacobian
      LOGICAL :: DoLambda      ! Solve ground-state lambda equation
      LOGICAL :: DoZeta        ! Solve excited-state zeta equation
      LOGICAL :: DoIPRight     ! Right diagonalize CC Jacobian for IP
!
      REAL(8), ALLOCATABLE :: EigJacob(:)   ! Jacobian eigenvalues
      REAL(8), ALLOCATABLE :: EigJacobIP(:)   ! Jacobian eigenvalues (IP)
!
!     o Print option
!
      INTEGER :: IPrint
!
      END MODULE
