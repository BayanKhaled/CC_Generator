!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      MODULE BasicInformation
      !     
      IMPLICIT NONE
      !
      CHARACTER(LEN=255) :: PreFixSrc, PreFixFile   ! Prefix of subroutine and subroutine file
      LOGICAL :: Facto             ! Flag IF diagrams are factorized or not
      LOGICAL :: NoT1              ! Flag to ignore T1 operator
      LOGICAL :: Complx            ! Flag for double complex arithmetic
      LOGICAL :: OrderCut          ! Flag to discard some high-order diagrams
      LOGICAL :: Lambda            ! Flag to generate diagrams and subroutines for Lambda equations
      LOGICAL :: RespDM            ! Flag to generate diagrams and response density matrices
      LOGICAL :: JacobRight        ! Flag to generate diagrams and subroutines for Jacobian right diagonalization
      LOGICAL :: JacobLeft         ! Flag to generate diagrams and subroutines for Jacobian left diagonalization
      LOGICAL :: Zeta              ! Flag to generate diagrams and subroutines for Zeta equations (constant terms)
      LOGICAL :: IPRight           ! Flag to generate diagrams and subroutines for IP-EOM right diagonalization
      INTEGER, PARAMETER :: MaxDig = 1000   ! Limit of # of diagrams
      INTEGER, PARAMETER :: MaxOp = 200   ! Limit of # of creation/annihilation operators per diagram
      INTEGER, PARAMETER :: LOff = 1000   ! Offset for De-excitation vertices; A de-excitation vertex is 
                                          ! represented by (n+LOff,l+LOff,m+LOff)
      INTEGER :: NMax              ! Maximum excitation level of T vertices
      INTEGER :: NRank             ! Level of excitation manifold to which CC eqns. are projected
      INTEGER :: NDig              ! # of diagrams
      INTEGER :: MaxOrder          ! Maximum order (higher-order diagrams will be discarded)
      !
      END MODULE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      MODULE Vertex_Module
      !     
      IMPLICIT NONE
      !
      ! o Information of interaction vertices
      !
      INTEGER :: NExct(13), NInter(13), NExtPart(13), NIntPart(13), NExtHole(13), NIntHole(13)
      !
      ! o Indicate if the vertex is eigher ionization or electron attachment type
      !
      LOGICAL :: RightIP(4), LeftIP(4), RightEA(4), LeftEA(4)
      !
      END MODULE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      PROGRAM CCGEN
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Generator of CC amplitude equations
      !     Ver. 1
      !
      !     2012 Apr Ver. 1 (Fortran90)
      !     2011     Ver. 0
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation
      USE Vertex_Module, ONLY : NExct, NInter, NIntPart, NIntHole, NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=2) :: c2
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=255) :: Name
      CHARACTER(LEN=30) :: Arg(14), c1, CWrk, CWrk1, CWrk2, CWrk3, CWrk4, CWrk5, CWrk6, &
     &   CWrk7, CWrk8, CWrk9, CWrk10
      CHARACTER(LEN=120) :: CDum120
      LOGICAL :: JacobDiag
      INTEGER :: IOUT, ICOD1, ICOD2
      INTEGER :: NArg
      INTEGER :: NDigJ, NDigL, NDigP
      INTEGER :: M1(4,MaxDig), M2(4,MaxDig), M3(4,MaxDig), M13(MaxDig), Mwp(MaxDig)
      INTEGER :: M1J(4,MaxDig), M2J(4,MaxDig), M3J(4,MaxDig), M13J(MaxDig), MwpJ(MaxDig)
      INTEGER :: M1P(4,MaxDig), M2P(4,MaxDig), M3P(4,MaxDig), M13P(MaxDig), MwpP(MaxDig)
      INTEGER :: M1L(4,MaxDig), M2L(4,MaxDig), M3L(4,MaxDig), M13L(MaxDig), MwpL(MaxDig)
      INTEGER :: I
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      ! --- Assign unit numbers
      !
      IOUT  = 4
      ICOD1 = 3
      ICOD2 = 2
      !
      ! --- Initialize interaction vertices (excitation level)
      !
      NExct(1)  =  0
      NExct(2)  =  0
      NExct(3)  = -1
      NExct(4)  =  0
      NExct(5)  =  0
      NExct(6)  =  0
      NExct(7)  = -1
      NExct(8)  = -1
      NExct(9)  =  1
      NExct(10) =  1
      NExct(11) = -2
      NExct(12) =  1
      NExct(13) =  2
      !
      ! --- # of internal lines (outgoing hole and/or incoming particle)
      !
      NInter(1)  = 1
      NInter(2)  = 1
      NInter(3)  = 2
      NInter(4)  = 2
      NInter(5)  = 2
      NInter(6)  = 2
      NInter(7)  = 3
      NInter(8)  = 3
      NInter(9)  = 1
      NInter(10) = 1
      NInter(11) = 4
      NInter(12) = 0
      NInter(13) = 0
      !
      ! --- # of internal particles
      !
      NIntPart(1)  = 1
      NIntPart(2)  = 0
      NIntPart(3)  = 1
      NIntPart(4)  = 2
      NIntPart(5)  = 0
      NIntPart(6)  = 1
      NIntPart(7)  = 2
      NIntPart(8)  = 1
      NIntPart(9)  = 1
      NIntPart(10) = 0
      NIntPart(11) = 2
      NIntPart(12) = 0
      NIntPart(13) = 0
      !
      ! --- # of internal holes
      !
      NIntHole(1)  = 0
      NIntHole(2)  = 1
      NIntHole(3)  = 1
      NIntHole(4)  = 0
      NIntHole(5)  = 2
      NIntHole(6)  = 1
      NIntHole(7)  = 1
      NIntHole(8)  = 2
      NIntHole(9)  = 0
      NIntHole(10) = 1
      NIntHole(11) = 2
      NIntHole(12) = 0
      NIntHole(13) = 0
      !
      ! --- # of external particles
      !
      NExtPart(1)  = 1
      NExtPart(2)  = 0
      NExtPart(3)  = 0
      NExtPart(4)  = 2
      NExtPart(5)  = 0
      NExtPart(6)  = 1
      NExtPart(7)  = 1
      NExtPart(8)  = 0
      NExtPart(9)  = 2
      NExtPart(10) = 1
      NExtPart(11) = 0
      NExtPart(12) = 1
      NExtPart(13) = 2
      !
      ! --- # of external holes
      !
      NExtHole(1)  = 0
      NExtHole(2)  = 1
      NExtHole(3)  = 0
      NExtHole(4)  = 0
      NExtHole(5)  = 2
      NExtHole(6)  = 1
      NExtHole(7)  = 0
      NExtHole(8)  = 1
      NExtHole(9)  = 1
      NExtHole(10) = 2
      NExtHole(11) = 0
      NExtHole(12) = 1
      NExtHole(13) = 2
      !
      ! --- Input Variables
      !
      ! NMax       : Level of truncation of cluster operator
      ! NRank      : Excitation level (1 =< NRank =< NMax)
      ! MaxOrder   : Diagrams in the order of MaxOrder or higher will be discarded (MaxOrder must be T to use this option)
      ! OrderCut   : T or F, depending whether higher-order diagrams are discarded or not
      ! Facto      : T or F, depending whether diagrams should be factorized or not
      ! NoT1       : T or F, depending whether T1 should be ignored or not 
      ! Complx     : T or F, depending whether double complex arithmetic is invoked
      ! JacobRight : T or F, depending whether Jacobian right equations are generated
      ! JacobLeft  : T or F, depending whether Jacobian left equations are generated
      ! Lambda     : T or F, depending whether Lambda equations are generated
      ! IPRight    : T or F, depending whether IP-EOM right equations are generated
      !
      NArg = 13
      DO I = 1, NArg
         CALL GETARG(I, Arg(I))
      END DO
      READ(Arg(1:13), *) NMax, NRank, MaxOrder, CWrk1, CWrk2, CWrk3, CWrk4, CWrk5, CWrk6, &
     &   CWrk7, CWrk8, CWrk9, CWrk10
      OrderCut = (TRIM(CWrk1) == 'T')
      Facto = (TRIM(CWrk2) == 'T')
      NoT1 = (TRIM(CWrk3) == 'T')
      Complx = (TRIM(CWrk4) == 'T')
      Lambda = (TRIM(CWrk5) == 'T')
      JacobRight = (TRIM(CWrk6) == 'T')
      JacobLeft = (TRIM(CWrk7) == 'T')
      Zeta = (TRIM(CWrk8) == 'T')
      RespDM = (TRIM(CWrk9) == 'T')
      IPRight = (TRIM(CWrk10) == 'T')
      IF (OrderCut) THEN
         WRITE(*, '("Diagrams higher than order", I3, X, "will be discarded.")') MaxOrder
      END IF
      IF (.NOT. Facto) THEN
         WRITE(*, '("!!! Diagrams will not be factorized !!!")')
      END IF
      IF (Complx) THEN
         WRITE(*, '("Assuming double complex arithmetic ...")')
      END IF
      !
      ! --- Generate programs for ground-state amplitude equations
      !
      CALL GenCC_GS(NDig, M1, M2, M3, M13, Mwp)
      !
      ! --- Generate programs for CC Jacobian
      !
      IF (JacobRight) THEN
         CALL GenCC_JRight(NDig, M1, M2, M3, M13, Mwp, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      END IF
      !
      ! --- Generate programs for calculating diagonal elements of CC Jacobian
      !
      JacobDiag = .FALSE.   ! YA042412
      !
      IF (JacobDiag) THEN
         CALL GenCC_JDiagonal(NDig, M1, M2, M3, M13, Mwp, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      END IF
      !
      ! --- Generate programs for Lambda vectors
      ! Lambda diagrams are generated after all parent diagrams are prepared
      !
      IF (Lambda .AND. (NRank == NMax)) THEN
         CALL GenCC_L(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
      END IF
      !
      ! --- Generate programs for Jacobian left eigenvectors
      ! The diagrams are generated after all parent diagrams are prepared
      !
      IF (JacobLeft .AND. (NRank == NMax)) THEN
         CALL GenCC_JLeft(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
      END IF
      !
      ! --- Generate programs for Zeta equations (constant terms only)
      !
      IF (Zeta .AND. (NRank == NMax)) THEN
         CALL GenCC_Zeta(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
      END IF
      !
      ! --- Generate programs for response density matrices
      !
      IF (RespDM .AND. (NRank == NMax)) THEN
         CALL GenCC_DM_Common(M1P, M2P, M3P, M13P, MwpP, NDigL)
      END IF
      !
      ! --- Generate programs for IP-EOM
      !
      IF (IPRight) THEN
         CALL GenCC_IPRight(M1P, M2P, M3P, M13P, MwpP, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      END IF
      !
      STOP
      END PROGRAM
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiag(MaxDig, NDig, NMax, NRank, M1, M2, M3, M13, NoT1, MaxOrder, OrderCut)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Generate the diagrams of T(NRank) equation in CC(NMax) 
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NExct, NInter, NIntPart
      !
      IMPLICIT NONE
      !
      LOGICAL :: NoT1, OrderCut
      INTEGER :: MaxDig, NDig, NMax, NRank, MaxOrder
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*)
      !
      INTEGER :: Kini, Kfin, Num1, K, K1, K2, K3, K4
      INTEGER :: NCnt, N, I1, I2, I3, I4, LevExc, IV, Minter, MPartcl, J1, J2, J3, J4, NOrder
      INTEGER :: M_init(4,MaxDig)
      INTEGER :: IOUT
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Determine T amplitudes
      !
      Kini = NRank - 2   ! Kini and Kfin define the range of allowed excitation created by T operators
      Kfin = NRank + 2
      IF ((NRank - 2) < 0) Kini = 0
      !
      Num1 = 0
      DO K = Kini, Kfin   ! loop over allowed excitation level
         DO K1 = 0, NMax   ! 4-fold loop over T operators
         DO K2 = 0, NMax
         DO K3 = 0, NMax
         DO K4 = 0, NMax
            IF (NoT1 .AND. (K1==1 .OR. K2==1 .OR. K3==1 .OR. K4==1)) CYCLE   ! Optionally ignore T1
!
            IF ((K1 + K2 + K3 + K4) /= K) CYCLE
            IF (K4 > K3) CYCLE
            IF (K3 > K2) CYCLE
            IF (K2 > K1) CYCLE
            Num1 = Num1 + 1
            M_init(1,Num1) = K1
            M_init(2,Num1) = K2
            M_init(3,Num1) = K3
            M_init(4,Num1) = K4
            WRITE(IOUT, '("M_init(", I3, ")=", 4I3)') Num1, K1, K2, K3, K4
         END DO
         END DO
         END DO
         END DO
      END DO
      !
      ! ---- Determine interaction vertex and partition internal lines
      !
      NCnt = 0
      DO N = 1, Num1
         I1 = M_init(1,N)
         I2 = M_init(2,N)
         I3 = M_init(3,N)
         I4 = M_init(4,N)
         LevExc = I1 + I2 + I3 + I4
         DO IV = 1, 13
            IF (LevExc + NExct(IV) /= NRank) CYCLE
            Minter = Ninter(IV)
            MPartcl = NIntPart(IV)
            !
            ! ---  Partition internal lines
            !
            DO J1 = 0, I1 + I1
               IF ((I1 /= 0) .AND. (J1 == 0)) CYCLE
            DO J2 = 0, I2 + I2
               IF ((I2 /= 0) .AND. (J2 == 0)) CYCLE
            DO J3 = 0, I3 + I3
               IF ((I3 /= 0) .AND. (J3 == 0)) CYCLE
            DO J4 = 0, I4 + I4
               IF ((I4 /= 0) .AND. (J4 == 0)) CYCLE
               IF ((J1 + J2 + J3 + J4) .NE. Minter) CYCLE
               IF ((I1 == I2) .AND. (J1 > J2)) CYCLE
               IF ((I2 == I3) .AND. (J2 > J3)) CYCLE
               IF ((I3 == I4) .AND. (J3 > J4)) CYCLE
               !
               ! ---  Distribute internal particle lines
               !
               DO K1 = MAX(J1 - I1, 0), MIN(I1, J1)
               DO K2 = MAX(J2 - I2, 0), MIN(I2, J2)
               DO K3 = MAX(J3 - I3, 0), MIN(I3, J3)
               DO K4 = MAX(J4 - I4, 0), MIN(I4, J4)
                  IF (K1 + K2 + K3 + K4 .NE. MPartcl) CYCLE
                  IF ((I1 == I2) .AND. (J1 == J2) .AND. (K1 > K2)) CYCLE
                  IF ((I2 == I3) .AND. (J2 == J3) .AND. (K2 > K3)) CYCLE
                  IF ((I3 == I4) .AND. (J3 == J4) .AND. (K3 > K4)) CYCLE
                  !
                  ! --- Discard the diagram if the order is higher than MaxOrder
                  ! T_n operator is assumed to be (n-1) order
                  !
                  IF (OrderCut) THEN
                     SELECT CASE (IV)
                     CASE (1, 2, 3, 12)
                        NOrder = 0
                     CASE (4, 5, 6, 7, 8, 9, 10, 11, 13)
                        NOrder = 1
                     END SELECT
                     IF (I1 > 0) NOrder = NOrder + (I1 - 1)
                     IF (I2 > 0) NOrder = NOrder + (I2 - 1)
                     IF (I3 > 0) NOrder = NOrder + (I3 - 1)
                     IF (I4 > 0) NOrder = NOrder + (I4 - 1)
                     IF (NOrder > MaxOrder) CYCLE
                  END IF
                  !
                  NCnt = NCnt + 1
                  M1(1,NCnt) = I1
                  M1(2,NCnt) = I2
                  M1(3,NCnt) = I3
                  M1(4,NCnt) = I4
                  !
                  M2(1,NCnt) = J1
                  M2(2,NCnt) = J2
                  M2(3,NCnt) = J3
                  M2(4,NCnt) = J4
                  !
                  M3(1,NCnt) = K1
                  M3(2,NCnt) = K2
                  M3(3,NCnt) = K3
                  M3(4,NCnt) = K4
                  !
                  M13(NCnt) = IV
                  !
                  WRITE(IOUT, '("M(", I3, ")= ", 3I2, X, "/", X, 3I2, &
     &               X, "/", X, 3I2, X, "/", 3I2, X, "/", X, I2)') &
     &               NCnt, I1, J1, K1, I2, J2, K2, I3, J3, K3, I4, J4, K4,IV
                  !
               END DO
               END DO
               END DO
               END DO
            END DO
            END DO
            END DO
            END DO
         END DO
      END DO
      !
      NDig = NCnt
      !
      WRITE(IOUT, '(/, "Total # of diagrams in CC(", I2, ")-T(", I2, ") eqs.:", I4)') NMax, NRank, NDig
      WRITE(*, '(/, "Total # of diagrams in CC(", I2, ")-T(", I2, ") eqs.:", I4)') NMax, NRank, NDig
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Descend(NDig, M1, M2, M3)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Arrange diagrams in descending order
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      INTEGER :: NDig, M1(4,*), M2(4,*), M3(4,*)
      !
      INTEGER :: IOUT
      INTEGER :: IDig, N, NT1, NT2, NT3, N1, N2, N3, N4
      INTEGER :: M1_(4), M2_(4), M3_(4)
      INTEGER :: NT(4)
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Arrange the diagrams in descending order
      !
      DO IDig = 1, NDig
         !
         DO N = 1, 4
            NT(N) = M1(N,IDig) * 100 + M2(N,IDig) * 10 + M3(N,IDig)
         END DO
         !
         NT1 = 0
         NT2 = 0
         NT3 = 0
         N1 = 1
         N2 = 2
         N3 = 3
         N4 = 4
         DO N = 1, 4
            IF (NT(N) >= NT1) THEN
               N1 = N
               NT1 = NT(N)
            END IF
         END DO
         DO N = 1, 4
            IF (N == N1) CYCLE
            IF (NT(N) >= NT2) THEN
               N2 = N
               NT2 = NT(N)
            END IF
         END DO
         DO N = 1, 4
            IF ((N == N1) .OR. (N == N2)) CYCLE
            IF (NT(N) >= NT3) THEN
               N3 = N
               NT3 = NT(N)
            END IF
         END DO
         DO N = 1, 4
            IF ((N == N1) .OR. (N == N2) .OR. (N == N3)) CYCLE
            N4 = N
         END DO
         !
         M1_(1) = M1(N1,IDig)
         M1_(2) = M1(N2,IDig)
         M1_(3) = M1(N3,IDig)
         M1_(4) = M1(N4,IDig)
         M2_(1) = M2(N1,IDig)
         M2_(2) = M2(N2,IDig)
         M2_(3) = M2(N3,IDig)
         M2_(4) = M2(N4,IDig)
         M3_(1) = M3(N1,IDig)
         M3_(2) = M3(N2,IDig)
         M3_(3) = M3(N3,IDig)
         M3_(4) = M3(N4,IDig)
         !
         DO N = 1, 4
            M1(N,IDig) = M1_(N)
            M2(N,IDig) = M2_(N)
            M3(N,IDig) = M3_(N)
         END DO
         !
      END DO   ! IDig
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SortDiag(NDig, M1, M2, M3, MH)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Sort diagrams (Greater strings stand later)
      !CCCCCCCCCCCCCCCCCCCCCCaCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      INTEGER :: NDig, M1(4,*), M2(4,*), M3(4,*), MH(*)
      !
      INTEGER :: M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34
      INTEGER :: MinPrev, MCnt, IDig, JDig, JVal, KDig, KVal, N_map, N
      INTEGER :: M1_(4,NDig), M2_(4,NDig), M3_(4,NDig), MH_(NDig)
      INTEGER :: N_ascen(NDig), NVal(NDig)
      !
      ! --- Assign diagram values
      !
      DO N = 1, NDig
         M11 = M1(1,N)
         M21 = M2(1,N)
         M31 = M3(1,N)
         M12 = M1(2,N)
         M22 = M2(2,N)
         M32 = M3(2,N)
         M13 = M1(3,N)
         M23 = M2(3,N)
         M33 = M3(3,N)
         M14 = M1(4,N)
         M24 = M2(4,N)
         M34 = M3(4,N)
         NVal(N) = M11 * 10 ** 13 + M21 * 10 ** 12 + M31 * 10 ** 11 + &
     &             M12 * 10 ** 10 + M22 * 10 ** 9  + M32 * 10 ** 8  + &
     &             M13 * 10 ** 7  + M23 * 10 ** 6  + M33 * 10 ** 5  + &
     &             M14 * 10 ** 4  + M24 * 10 ** 3  + M34 * 10 ** 2  + &
     &             MH(N)
      END DO
      !
      ! --- Sort diagrams
      !
      MinPrev = 0
      MCnt = 0
      !
      DO IDig = 1, NDig   ! Serach for diagram values
         Jloop: DO JDig = 1, NDig
            JVal = NVal(JDig)
            !         
            IF (JVal <= MinPrev) CYCLE
            !         
            DO KDig = 1, NDig   ! Check IF JVal is the MCnt-th minimum
               KVal = NVal(KDig)
               IF (KVal <= MinPrev) CYCLE
               IF (KVal < JVal) CYCLE Jloop
            END DO
            !         
            MCnt = MCnt + 1
            N_ascen(MCnt) = JDig
            MinPrev = JVal
            EXIT
            !         
         END DO Jloop   ! JDig
      END DO   ! IDig
      !
      DO IDig = 1, NDig
         N_map = N_ascen(IDig)
         DO N=1,4
            M1_(N,IDig) = M1(N,N_map)
            M2_(N,IDig) = M2(N,N_map)
            M3_(N,IDig) = M3(N,N_map)
         END DO
         MH_(IDig) = MH(N_map)
      END DO
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            M1(N,IDig) = M1_(N,IDig)
            M2(N,IDig) = M2_(N,IDig)
            M3(N,IDig) = M3_(N,IDig)
         END DO
         MH(IDig) = MH_(IDig)
      END DO
!cc
      WRITE(*, *) NDig
!cc
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Wgt_Phse(M1orig, M2orig, M3orig, M13, Mwp, NDig)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Determine weight factor (1/2)^m and phase factor (m is the # of equivalent T vertecies)
      !     +m or -m is stored for each diagram in Mwp
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, Only : NRank, NMax
      USE Vertex_Module, ONLY : NExct, NInter, NIntPart
      !
      IMPLICIT NONE
      !
      INTEGER :: NDig, M1orig(4,NDig), M2orig(4,NDig), M3orig(4,NDig), M13(NDig)
      INTEGER :: Mwp(NDig)
      !
      CHARACTER(LEN=3) :: OpTyp(2*NMax*4+4), Parity
      INTEGER :: IOUT
      INTEGER :: IDig, N, NCnt, I, J, IT, JT, ID_V, NumOpsH, IAdd, Iop0, Iop, NumT, &
     &   Ipart, Ihole, Nhole, Nloop
      INTEGER :: M1(4,NDig), M2(4,NDig), M3(4,NDig)
      INTEGER :: NT(4,NDig)   ! stores 3-digit values representing T-verteces
      INTEGER :: IJcont(4,2)   ! stores contraction pairs
      INTEGER :: IpartnerC(2*NMax*4+4), IpartnerP(2*NMax*4+4)
      INTEGER :: nB(4), nJ(4), nBV(2,4), nJV(2,4)
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Prepare NT array
      !     
      DO IDig = 1, NDig
         DO N = 1, 4
            NT(N,IDig) = M1orig(N,IDig) * 100 + &
     &                   M2orig(N,IDig) * 10 + &
     &                   M3orig(N,IDig)
         END DO
      END DO
      !
      ! --- Count equivalent T verteces
      !     * Three or more T's cannot be equivalent (W has two hole/particle lines)
      !     * There can be no more than two pairs of equivalent T's (At most four internal lines)
      !
      DO IDig = 1, NDig
         NCnt = 1
         DO I = 1, 4
            IF (M1orig(I,IDig) == 0) CYCLE
            IT = NT(I,IDig)
            DO J = 1, I - 1
               JT = NT(J,IDig)
               IF (JT == IT) NCnt = NCnt + 1
            END DO
         END DO
         Mwp(IDig) = NCnt
      END DO
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            M1(N,IDig) = IABS(M1orig(N,IDig))
            M2(N,IDig) = IABS(M2orig(N,IDig))
            M3(N,IDig) = IABS(M3orig(N,IDig))
         END DO
      END DO
      !
      ! --- Phase Factor
      !
      DO IDig = 1, NDig
         !
         ! --- Type of operators in bra
         !
         !         DO IH = 1, NRank
         !            OpTyp(2*IH - 1) = "ehd"
         !            OpTyp(2*IH    ) = "epn"
         !         END DO
         !         IAdd = NRank + NRank   ! IAdd is the # of operators which have been determined
         !
         ! --- Type of operators in the interaction vertex
         !
         ID_V = M13(IDig)   ! Interaction vertex number
         !
         ! --- Type of each operator ("i"nternal or "e"xternal; "p"article or "h"ole; "d"aggar or "n"on-daggar
         ! --- Type is characterized by a 3-character variable, e.g. "ipd" (internal particle with daggar)
         !     
         SELECT CASE (ID_V)
            CASE (1)
               OpTyp(1) = "epd"
               OpTyp(2) = "ipn"
               NumOpsH = 2   ! # of operators in interaction vertex
            CASE (2)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ehn"
               NumOpsH = 2
            CASE (3)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ipn"
               NumOpsH = 2
            CASE (4)
               OpTyp(1) = "epd"
               OpTyp(2) = "ipn"
               OpTyp(3) = "epd"
               OpTyp(4) = "ipn"
               NumOpsH = 4
            CASE (5)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ehn"
               OpTyp(3) = "ihd"
               OpTyp(4) = "ehn"
               NumOpsH = 4
            CASE (6)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ehn"
               OpTYp(3) = "epd"
               OpTyp(4) = "ipn"
               NumOpsH = 4
            CASE (7)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ipn"
               OpTyp(3) = "epd"
               OpTyp(4) = "ipn"
               NumOpsH = 4
            CASE (8)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ipn"
               OpTyp(3) = "ihd"
               OpTyp(4) = "ehn"
               NumOpsH = 4
            CASE (9)
               OpTyp(1) = "epd"
               OpTyp(2) = "ehn"
               OpTyp(3) = "epd"
               OpTyp(4) = "ipn"
               NumOpsH = 4
            CASE (10)
               OpTyp(1) = "epd"
               OpTyp(2) = "ehn"
               OpTyp(3) = "ihd"
               OpTyp(4) = "ehn"
               NumOpsH = 4
            CASE (11)
               OpTyp(1) = "ihd"
               OpTyp(2) = "ipn"
               OpTyp(3) = "ihd"
               OpTyp(4) = "ipn"
               NumOpsH = 4
            CASE (12)
               OpTyp(1) = "epd"
               OpTyp(2) = "ehn"
               NumOpsH = 2
            CASE (13)
               OpTyp(1) = "epd"
               OpTyp(2) = "ehn"
               OpTyp(3) = "epd"
               OpTyp(4) = "ehn"
               NumOpsH = 4
         END SELECT
         IAdd = NumOpsH
         !
         ! --- Summation indices
         !
         DO N = 1, 4   ! # of summation indices for each T
            nB(N) = M3(N,IDig)
            nJ(N) = M2(N,IDig) - M3(N,IDig)
         END DO
         !
         ! --- Internal lines of V are assigned to appropriate T vertices
         !
         Iop0 = 1
         DO N = 1, 4
            IF (nB(N) == 0) CYCLE
            NCnt = 1
            DO Iop = Iop0, NumOpsH
               IF (OpTyp(Iop) == "ipn") THEN
                  nBV(NCnt,N) = Iop
                  NCnt = NCnt + 1
                  Iop0 = Iop+1
               END IF
               IF (NCnt > nB(N)) EXIT
            END DO
         END DO
         !
         Iop0 = 1
         DO N = 1, 4
            IF (nJ(N) == 0) CYCLE
            NCnt = 1
            DO Iop = Iop0, NumOpsH
               IF (OpTyp(Iop) == "ihd") THEN
                  nJV(NCnt,N) = Iop
                  NCnt = NCnt + 1
                  Iop0 = Iop+1
               END IF
               IF (NCnt > nJ(N)) EXIT
            END DO
         END DO
         !
         ! --- Type of operators in T vertecies
         ! --- Carry out internal contractions at the same time
         !
         NumT = 0
         !
         DO N = 4, 1, -1
            !
            IF (M1(N,IDig) .eq. 0) CYCLE
            DO Ipart = 1, M3(N,IDig)   ! loop over internal particles
               OpTyp(IAdd+2*IPart-1) = "ipd"
               IpartnerC(IAdd+2*IPart-1) = nBV(Ipart,N)   ! internal contraction
               IpartnerC(nBV(Ipart,N)) = IAdd + 2 * IPart - 1   ! internal contraction
!!!
               WRITE(IOUT, '("IDig =", I3, " Inter. Cont.: ",I3,"--", I3)') &
     &              IDig, nBV(Ipart,N), IAdd + 2 * IPart - 1
!!!               
            END DO   ! Ipart
            DO Ipart = M3(N,IDig) + 1, M1(N,IDig)   ! loop over external particles
               OpTyp(IAdd+2*Ipart-1) = "epd"
            END DO   ! Ipart
            !
            DO Ihole = 1, M2(N,IDig) - M3(N,IDig)   ! loop over internal holes
               OpTyp(IAdd+2*Ihole) = "ihn"
               IpartnerC(IAdd+2*Ihole) = nJV(Ihole,N)   ! internal contraction
               IpartnerC(nJV(Ihole,N)) = IAdd+2*Ihole   ! internal contraction
!!!
               WRITE(IOUT, '("IDig =", I3, " Inter. Cont.: ", I3, "--", I3)') &
     &              IDig, nJV(Ihole,N), IAdd + 2 * Ihole
!!!               
            END DO   ! Ihole
            DO Ihole = M2(N,IDig) - M3(N,IDig) + 1, M1(N,IDig)   ! loop over external holes
               OpTyp(IAdd+2*Ihole) = "ehn"
            END DO   ! Ihole
            IAdd = IAdd + 2 * M1(N,IDig)
            NumT = NumT + 1   ! # of T vertecies
            !
         END DO   ! N
         !
         WRITE(IOUT, '(X, I3, X, "# of T vertices: ", I3)') IDig, NumT
         WRITE(IOUT, '(X, "Total # of operators: ", I3)') IAdd
         WRITE(IOUT, *) (OpTyp(i)//" ", i = 1, IAdd)
         !
         ! --- Count holes, define paths, external connections
         !
         CALL CountHole(OpTyp, IAdd, Nhole)
         CALL DefPath(IAdd, ID_V, M1(1,IDig), IpartnerP)
!!!      CALL IntCont(OpTyp,IAdd,NumOpsH,ID_V,IpartnerC,nB,nJ,nBV,nJV,
!!!  *        IOUT)
         CALL ExtCont(OpTyp, IAdd, IpartnerC, IpartnerP, IOUT)
         !
         WRITE(IOUT, *) (OpTyp(i)//" ", i = 1, IAdd)
         !
         ! --- Define the phase factor
         !
         CALL DefPhase(IpartnerP, IpartnerC, IAdd, Nloop)
         !
         WRITE(IOUT, '(3X, I3, " loops and", I4, " holes")') Nloop, Nhole
         !
         IF (Mod(Nloop-Nhole,2) /= 0) Mwp(IDig) = -Mwp(IDig)
!cc
         WRITE(IOUT, '(X, "Signed weight factor: ", I3, /)') Mwp(IDig)
!cc
         !
      END DO   ! IDig
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenFactorize(NRank, NDig, M1, M2, M3, M13, Mwp, Facto, Complx)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Factorize diagrams: General version (Two types of amplitude can be handled)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : RightIP, LeftIP, RightEA, LeftEA
      !
      IMPLICIT NONE
      !
      LOGICAL :: Facto, Complx
      INTEGER :: NRank, NDig, M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=2) :: a2
      CHARACTER(LEN=255) :: c1, c2, c3, c4, CWrk, TName, VName
      INTEGER :: IOUT, ICOD
      INTEGER :: IDig, N, NCnt, NTlevel, NumT1, NumT2, NumT3, NumT4, NumDig, IT, IT1, IT2, IT3, IT4, &
     &   NT_Typ1, NT_Typ2, NT_Typ3, NT_Typ4, N_1, L_1, M_1, N_2, L_2, M_2, N_3, L_3, M_3, N_4, L_4, M_4
      INTEGER :: nB, nJ, nC, nK, nD, nL, nA, nI, nAI
      INTEGER :: NT(4,NDig), NTyp1(NDig), NTyp2(NDig), NTyp3(NDig), NTyp4(NDig)
      INTEGER :: M1Temp
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      DO IT = 1, 4
         RightIP(IT) = .FALSE.
         RightEA(IT) = .FALSE.
         LeftIP(IT) = .FALSE.
         LeftEA(IT) = .FALSE.
      END DO
      !
      ! --- Make 3-digit integers for T of R verteces
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            NT(N,IDig) = M1(N,IDig) * 100 + M2(N,IDig) * 10 + M3(N,IDig)   !<-- M1, M2 and M3 should be less than 10
         END DO   ! N
      END DO   ! IDig
      !
      ! --- Check the # and types of the outermost T or R vertices
      !
      IF (Facto) THEN
         NumT1 = 0   ! # of types of outermost T
         DO IDig = 1, NDig   ! look for non-zero outermost T
            IF ((IDig > 1) .AND. (NT(1,IDig) == NT(1,IDig-1))) CYCLE
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!",5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE   ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      ELSE
         NumT1 = 0
         DO IDig = 1, NDig
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE   ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      END IF
      WRITE(IOUT, '(/, "Start residue calculation", /)')
      !
      ! --- Size of V1 array (NOc: occ. MO number, NVr: vir. MO number)
      ! --- Integer function CCLib_NComb(N,n) computes N!/(N-n)!
      !
      WRITE(c1, '(I3)') NRank
      CWrk = "CCLib_NComb(NOc,"//TRIM(c1)//")*CCLib_NComb(NVr,"//TRIM(c1)//")"
      WRITE(IOUT, '(/, X, "Initialize V1 of size ", A100)') CWrk
      WRITE(ICOD, '(/, "!", 5("-"), X, "Initialize V1 of size ", A100)') CWrk
      !
      WRITE(ICOD, '(6X, "NSizeV1 = CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ",I2,")")') &
     &   NRank, NRank
      CALL GetVarray("V1", "NSizeV1", "V1", "INIT", ICOD)
      CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
      NumDig = 0
      WRITE(ICOD, '(6X, "NumDig = 0", /)')
      !
      DO IT1 = 1, NumT1
         !
         NT_Typ1 = NTyp1(IT1)
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(/, X, "Processing T1(", I3, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T1(", I3, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         ELSE
            WRITE(IOUT, '(/, X, "Processing R1(", I4, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R1(", I4, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         END IF
         IF (NT_Typ1 == 0) THEN   ! Substitute G into V1, IF there is no 1st T vertex, and GO TO the next T(1)
            NumDig = NumDig + 1
            !
            ! --- Read interaction vertex from disc
            !
            CALL DefGsize(M13(NumDig), ICOD)
            CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
            !
            ! --- Substitute interaction vertex to V1
            !
            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
            CALL AddGtoV("V1", "NSizeV1", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
            CALL FreeArray("G", ICOD)
            !
            ! --- Save and free V1
            !
            CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
            CYCLE
         END IF
         !
         ! --- Initialize V2
         !
         N_1 = NT_Typ1 / 100
         L_1 = (NT_Typ1 - 100 * N_1) / 10
         M_1 = NT_Typ1 - 100 * N_1 - 10 * L_1
         IF (NT_Typ1 < 0) THEN
            N_1 = -N_1
            L_1 = -L_1
            M_1 = -M_1
         END IF
         WRITE(c1, '(I3)') M_1
         WRITE(c2, '(I3)') L_1 - M_1
         WRITE(c3, '(I3)') NRank - N_1 + M_1
         WRITE(c4, '(I3)') NRank - N_1 + L_1 - M_1
         CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &      //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &      //")"
         !
         WRITE(IOUT, '(2X, "Initialize V2 of size ", A100)') CWrk
         WRITE(ICOD, '("!", 5("-"), X, "Initialize V2 of size ", A100)') CWrk
         !
         WRITE(ICOD, '(6X, "NSizeV2 = CCLib_NComb(NVr, ", I2, &
     &      ") * CCLib_NComb(NOc, ", I2, &
     &      ") * CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ", I2, &
     &      ")")') M_1, L_1 - M_1, NRank - N_1 + M_1, NRank - N_1 + L_1 - M_1
         !
         CALL GetVarray("V2", "NSizeV2", "V2", "INIT", ICOD)
         CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
         !
         ! --- Find T(2) or R(2) vertices for current T(1) or R(1)
         !
         IF (Facto) THEN
            NumT2 = 0
            NCnt = 0
            DO IDig = 1, NDig
               IF (NT(1,IDig) /= NT_Typ1) CYCLE
               NCnt = NCnt + 1
               IF ((NCnt > 1) .AND. (NT(2,IDig) == NT(2,IDig-1))) CYCLE
               NumT2 = NumT2 + 1
               NTyp2(NumT2) = NT(2,IDig)
               IF (NTyp2(NumT2) >= 0) THEN
                  WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               ELSE
                  WRITE(IOUT, '(2X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               END IF
            END DO   ! IDig
         ELSE
            NumT2 = 1
            NTyp2(NumT2) = NT(2,IT1)
            IF (NTyp2(NumT2) >= 0) THEN
               WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
            ELSE
               WRITE(IOUT, '(2X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
            END IF
         END IF
         !
         DO IT2 = 1, NumT2
            !
            NT_Typ2 = NTyp2(IT2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(/, X, "Processing T2(", I3, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T2(", I3, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            ELSE
               WRITE(IOUT, '(/, X, "Processing R2(", I4, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R2(" ,I4, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            END IF
            !
            IF (NT_Typ2 == 0) THEN   ! Substitute G into V2 IF there is no 2nd T vertex
               NumDig = NumDig + 1
               !
               ! --- Read interaction vertex from disc
               !
               CALL DefGsize(M13(NumDig), ICOD)
               CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
               !
               ! --- Substitute G to V2
               !
               CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
               CALL AddGtoV("V2", "NSizeV2", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
               CALL FreeArray("G", ICOD)
               !
               ! --- Save V2 on disk
               !
               CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
               CYCLE
            END IF
            !
            ! --- Initialize V3
            !
            N_2 = NT_Typ2 / 100
            L_2 = (NT_Typ2 - 100 * N_2) / 10
            M_2 = NT_Typ2 - 100 * N_2 - 10 * L_2
            IF (NT_Typ2 < 0) THEN
               N_2 = -N_2
               L_2 = -L_2
               M_2 = -M_2               
            END IF
            WRITE(c1, '(I3)') M_1 + M_2
            WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2)
            WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2)
            WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2)
            CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, " &
     &         //TRIM(c2) &
     &         //") * CCLib_NComb(NVr, "//TRIM(c3) &
     &         //")*CCLib_NComb(NOc, "//TRIM(c4) &
     &         //")"
            !
            WRITE(IOUT, '(3X, "Initialize V3 of size ", A100)') CWrk
            WRITE(ICOD, '("!", 5("-"), X, "Initialize V3 of size ", A100)') CWrk
            !
            WRITE(ICOD, '(6X, "NSizeV3 = CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2, &
     &         ") * CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2,")")') &
     &         M_1 + M_2, (L_1 - M_1) + (L_2 - M_2), NRank - (N_1 - M_1) - (N_2 - M_2), &
     &         NRank - (N_1-L_1+M_1) - (N_2-L_2+M_2)
            CALL GetVarray("V3", "NSizeV3", "V3", "INIT", ICOD)
            CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
            !
            ! --- Find T(3) or R(3) for current T(or R)(1) and T(or R)(2)
            !
            IF (Facto) THEN
               NumT3 = 0
               NCnt = 0
               DO IDig=1,NDig
                  IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2)) CYCLE
                  NCnt = NCnt + 1
                  IF ((NCnt > 1) .AND. (NT(3,IDig) == NT(3,IDig-1))) CYCLE
                  NumT3 = NumT3 + 1
                  NTyp3(NumT3) = NT(3,IDig)
                  IF (NTyp3(NumT3) >= 0) THEN
                     WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  ELSE
                     WRITE(IOUT, '(3X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  END IF
               END DO   ! IDig
            ELSE
               NumT3 = 1
               NTyp3(NumT3) = NT(3,IT1)
               IF (NTyp3(NumT3) >= 0) THEN
                  WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
               ELSE
                  WRITE(IOUT, '(3X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
               END IF
            END IF
            !
            DO IT3 = 1, NumT3
               !
               NT_Typ3 = NTyp3(IT3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(/, X, "Processing T3(", I3, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T3(", I3, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               ELSE
                  WRITE(IOUT, '(/, X, "Processing R3(", I4, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R3(" ,I4, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               END IF
               !
               IF (NT_Typ3 == 0) THEN   ! Substitute G into V3 IF there is no 3rd T vertex
                  NumDig = NumDig + 1
                  !
                  ! --- Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Substitute G into V3
                  !
                  CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
                  CALL AddGtoV("V3", "NSizeV3", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                  CALL FreeArray("G", ICOD)
                  !
                  ! --- Save V3 on disk
                  !
                  CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
                  CYCLE
               END IF
               !
               ! --- Initialize V4
               !
               N_3 = NT_Typ3 / 100
               L_3 = (NT_Typ3 - 100 * N_3) / 10
               M_3 = NT_Typ3 - 100 * N_3 - 10 * L_3
               IF (NT_Typ3 < 0) THEN
                  N_3 = -N_3
                  L_3 = -L_3
                  M_3 = -M_3
               END IF
               WRITE(c1, '(I3)') M_1 + M_2 + M_3
               WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3)
               WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2) - (N_3 - M_3)
               WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &            //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &            //")"
               !
               WRITE(IOUT, '(4X, "Initialize V4 of size ", A100)') CWrk
               WRITE(ICOD, '("!", 5("-"), X, "Initialize V4 of size ", A100)') CWrk
               !
               WRITE(ICOD, '(6X, "NSizeV4 = CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ") * CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ")")') &
     &            M_1 + M_2 + M_3, (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3), &
     &            NRank - (N_1 - M_1) - (N_2 - M_2)- ( N_3 - M_3), &
     &            NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "INIT", ICOD)
               CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
               !
               ! --- Fint T(or R)(4) for current T(or R)(1), T(or R)(2), and T(or R)(3)
               !
               IF (Facto) THEN
                  NumT4 = 0
                  NCnt = 0
                  DO IDig = 1, NDig
                     IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2) .OR. (NT(3,IDig) /= NT_Typ3)) CYCLE
                     NCnt = NCnt + 1
                     IF ((NCnt > 1) .AND. (NT(4,IDig) == NT(4,IDig-1))) CYCLE
                     NumT4 = NumT4 + 1
                     NTyp4(NumT4) = NT(4,IDig)
                     IF (NTyp4(NumT4) >= 0) THEN
                        WRITE(IOUT, '(4X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     ELSE
                        WRITE(IOUT, '(4X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     END IF
                  END DO   ! IDig
               ELSE
                  NumT4 = 1
                  NTyp4(NumT4) = NT(4,IT1)
                  IF (NTyp4(NumT4) >= 0) THEN
                     WRITE(IOUT, '(4X, "A new T3 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                  ELSE
                     WRITE(IOUT, '(4X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                  END IF
               END IF
               !
               DO IT4 = 1, NumT4 
                  !
                  NT_Typ4 = NTyp4(IT4)
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(/, X, "Processing T4(", I3, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T4(", I3, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(/, X, "Processing R4(", I4, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R4(", I4, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  END IF
                  !
                  IF (NT_Typ4 == 0) THEN   ! Substitute G into V4, IF there is no 4th T vertex
                     NumDig = NumDig + 1
                     !
                     ! --- Read interaction vertex from disc
                     !
                     CALL DefGsize(M13(NumDig), ICOD)
                     CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                     !
                     ! --- Substitute G into V4
                     !
                     CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                     CALL AddGtoV("V4", "NSizeV4", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                     CALL FreeArray("G", ICOD)
                     !
                     ! --- Save and free V4
                     !
                     CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                     CYCLE
                  END IF
                  !
                  ! --- Get interaction vertex
                  !
                  NumDig = NumDig + 1
                  !
                  ! --- Allocate G; Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Allocate G_
                  !
                  IF (M1(4,NumDig) < 0) THEN
                     CALL DefG_Size(M13(NumDig), -M1(4,NumDig), -M2(4,NumDig), -M3(4,NumDig), ICOD)
                  ELSE
                     CALL DefG_Size(M13(NumDig), M1(4,NumDig), M2(4,NumDig), M3(4,NumDig), ICOD)
                  END IF
                  CALL GetVarray("G_", "NSizeG_", " ", "NONE", ICOD)
                  !
                  ! --- Re-arrange G
                  !
                  WRITE(IOUT, '(4X, "Re-arrange G array")')
                  WRITE(ICOD, '("!", 5("-"), X, "Re-arrange G array")')
                  CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 4, ICOD)
!072112Bgn
                  IF ((nC /= 0) .OR. (nK /= 0)) THEN
!072112End
                     nAI = nA + nI
                     CALL MakeSrcArrange("G", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                     WRITE(ICOD, '(6X, "CALL", $)')
                     WRITE(ICOD, *) TRIM(CWrk)//"( &"
                     WRITE(ICOD, *) "     &   "//"G, G_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
!072112Bgn
                  ELSE
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeG, G, 1, G_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeG, G, 1, G_, 1)")')
                     END IF
                  END IF
!072112End
                  !
                  ! --- Free G
                  !
                  CALL FreeArray("G", ICOD)
!072012Bgn
                  !
                  ! Load T amplitudes
                  !
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
                     M1Temp = M1(4,NumDig)
                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                  ELSE
                     M1Temp = IABS(M1(4,NumDig))
                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
                  END IF
                  !
                  ! --- Re-arrange T if neccessary
                  !
                  N_4 = NT_Typ4 / 100
                  L_4 = (NT_Typ4 - 100 * N_4) / 10
                  M_4 = NT_Typ4 - 100 * N_4 - 10 * L_4
                  IF (NT_Typ4 < 0) THEN
                     N_4 = -N_4
                     L_4 = -L_4
                     M_4 = -M_4
                  END IF
                  !
                  nB = M_4
                  nJ = L_4 - M_4
                  nC = N_4 - nB
                  nK = N_4 - nJ
                  nD = nB + nC
                  nL = nJ + nK
                  nA = 0
                  nI = 0
                  !
                  WRITE(ICOD, '(6X, "nB =", I3)') nB
                  WRITE(ICOD, '(6X, "nJ =", I3)') nJ
                  WRITE(ICOD, '(6X, "nC =", I3)') nC
                  WRITE(ICOD, '(6X, "nK =", I3)') nK
                  WRITE(ICOD, '(6X, "nD =", I3)') nD
                  WRITE(ICOD, '(6X, "nL =", I3)') nL
                  WRITE(ICOD, '(6X, "nA =", I3)') nA
                  WRITE(ICOD, '(6X, "nI =", I3)') nI
                  !
                  WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
                  WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
                  WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
                  WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
                  WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
                  WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
                  WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &               "CCLib_NComb(NOc, nI)")')
                  !
                  WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &               "CCLib_NComb(NVr, nB) * ", &
     &               "CCLib_NComb(NOc, nK) * ", &
     &               "CCLib_NComb(NVr, nC)")')
                  !
                  IF (NT_Typ4 >= 0) THEN
                     CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
                  ELSE
                     CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
                  END IF
                  !
                  IF ((nC == 0) .AND. (nK == 0)) GO TO 1000
                  IF ((nC == 0) .AND. (nJ == 0)) GO TO 1000
                  !
                  WRITE(IOUT, '(4X, "Re-arrange T(4) amplitude")')
                  !
                  nAI = 0
                  CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                     CALL FreeArray("T", ICOD)
                  ELSE
                     WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                     CALL FreeArray("R", ICOD)
                  END IF
                  GO TO 1100
 1000             CONTINUE
                  IF (NT_Typ4 >= 0) THEN
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
                     END IF
                     CALL FreeArray("T", ICOD)
                  ELSE
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
                     END IF
                     CALL FreeArray("R", ICOD)
                  END IF
 1100             CONTINUE
!072012End
                  !
                  ! --- Contract G_ to T(or R)(4); Add to V4
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(5X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(5X, "V4 += R4(", I4, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += R4(", I4, ")*I5")') NT_Typ4
                  END IF
                  !
                  ! --- Load V4
                  !
                  CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                  !
                  ! --- Load T amplitudes, Contraction: G_*T += V4, Free T
                  !
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
!                     M1Temp = M1(4,NumDig)
!                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
!                     CALL Contract("G_", "T", "V4", NRank, NTlevel, &
!     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!                     CALL FreeArray("T", ICOD)
                     CALL ContractBLAS3("G_", "T_", "V4", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("T_", ICOD)
                  ELSE
!072012                     M1Temp = IABS(M1(4,NumDig))
!072012                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
!072012                     CALL Contract("G_", "R", "V4", NRank, NTlevel, &
!072012     &                   M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!072012                     CALL FreeArray("R_", ICOD)
                     CALL ContractBLAS3("G_", "R_", "V4", NRank, NTlevel, &
     &                   M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("R_", ICOD)
                  END IF
                  !
                  ! --- Free G_
                  !
                  CALL FreeArray("G_", ICOD)
                  !
                  ! --- Save and free V4
                  !
                  CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                  !
                  ! --- End of level-4 contraction
                  !
               END DO   ! IT4 (loop over 4th T-vertex)
               !
               WRITE(IOUT, '(4X, "Re-arrange V4 array")')
               WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V4 array")')
               CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 3, ICOD)
               nAI = 99999
               IF (nA == 0 .AND. nI == 0) nAI = 0
               !
               ! --- Allocate V4_ array
               !
               WRITE(ICOD, '(6X, "NSizeV4_ = mAI * mJ * mB * mK * mC")')
               WRITE(ICOD, '(6X, "ALLOCATE(V4_(NSizeV4_))")')
               !
               ! --- Allocate and load V4 intermediate
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
!072112Bgn
               IF ((nC /= 0) .OR. (nK /= 0)) THEN
!072112End
                  !
                  ! --- Make SUBROUTINE to rearrange V4 -> V4_
                  !
                  CALL MakeSrcArrange("V4", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  WRITE(ICOD, *) "     &   "//"V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
                  !
!072112Bgn
               ELSE
                  IF (Complx) THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeV4, V4, 1, V4_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeV4, V4, 1, V4_, 1)")')
                  END IF
               END IF
!072112End
               !
               CALL FreeVarray("V4", "NSizeV4", "V4", "NONE", ICOD)
               WRITE(ICOD, '("!")')
!072112Bgn
               !
               ! Load T amplitudes
               !
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
                  M1Temp = M1(3,NumDig)
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
               ELSE
                  M1Temp = IABS(M1(3,NumDig))
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
               END IF
               !
               WRITE(IOUT, '(4X, "Re-arrange T(3) amplitude")')
               !
               ! --- Re-arrange T if neccessary
               !
               nB = M_3
               nJ = L_3 - M_3
               nC = N_3 - nB
               nK = N_3 - nJ
               nD = nB + nC
               nL = nJ + nK
               nA = 0
               nI = 0
               !
               WRITE(ICOD, '(6X, "nB =", I3)') nB
               WRITE(ICOD, '(6X, "nJ =", I3)') nJ
               WRITE(ICOD, '(6X, "nC =", I3)') nC
               WRITE(ICOD, '(6X, "nK =", I3)') nK
               WRITE(ICOD, '(6X, "nD =", I3)') nD
               WRITE(ICOD, '(6X, "nL =", I3)') nL
               WRITE(ICOD, '(6X, "nA =", I3)') nA
               WRITE(ICOD, '(6X, "nI =", I3)') nI
               !
               WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
               WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
               WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
               WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
               WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
               WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
               WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &            "CCLib_NComb(NOc, nI)")')
               !
               WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &            "CCLib_NComb(NVr, nB) * ", &
     &            "CCLib_NComb(NOc, nK) * ", &
     &            "CCLib_NComb(NVr, nC)")')
               !
               IF (NT_Typ3 >= 0) THEN
                  CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
               ELSE
                  CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
               END IF
               !
               IF ((nC == 0) .AND. (nK == 0)) GO TO 2000
               IF ((nC == 0) .AND. (nJ == 0)) GO TO 2000
               !
               nAI = 0
               CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               IF (NT_Typ3 >= 0) THEN
                  WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("T", ICOD)
               ELSE
                  WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("R", ICOD)
               END IF
               GO TO 2100
 2000          CONTINUE
               IF (NT_Typ3 >= 0) THEN
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
                  END IF
                  CALL FreeArray("T", ICOD)
               ELSE
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
                  END IF
                  CALL FreeArray("R", ICOD)
               END IF
 2100          CONTINUE
!072112End
!072112               !
!072112               ! --- Contract V4_ to T(or R)(3)
!072112               !
!072112               IF (NT_Typ3 >= 0) THEN
!072112                  WRITE(IOUT, '(5X, "V3 += T3(", I3, ")*V4")') NT_Typ3
!072112                  WRITE(ICOD, '("!", 5("-"), X, "V3 += T3(", I3, ")*V4")') NT_Typ3
!072112               ELSE
!072112                  WRITE(IOUT, '(5X, "V3 += R3(", I4, ")*V4")') NT_Typ3
!072112                  WRITE(ICOD, '("!", 5("-"), X, "V3 += R3(", I4, ")*V4")') NT_Typ3
!072112               END IF
               !
               ! --- Load V3
               !
               CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
               !
               ! --- Load T amplitudes, Contraction: V4_*T += V3, Free T array
               !
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
!                  M1Temp = M1(3,NumDig)
!                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
!                  CALL Contract("V4_", "T", "V3", NRank, NTlevel, &
!     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!                  CALL FreeArray("T", ICOD)
                  CALL ContractBLAS3("V4_", "T_", "V3", NRank, NTlevel, &
     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                  CALL FreeArray("T_", ICOD)
               ELSE
!                  M1Temp = IABS(M1(3,NumDig))
!                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
!                  CALL Contract("V4_", "R", "V3", NRank, NTlevel, &
!     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!                  CALL FreeArray("R", ICOD)
                  CALL ContractBLAS3("V4_", "R_", "V3", NRank, NTlevel, &
     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                  CALL FreeArray("R_", ICOD)
               END IF
               !
               ! --- Free V4_ array
               !
               CALL FreeArray("V4_", ICOD)
               !
               ! --- Save and free V3
               !
               CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
               !
               ! --- End of level-3 contraction
               !
            END DO   ! IT3
            !
            WRITE(IOUT, '(4X, "Re-arrange V3 array")')
            WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V3 array")')
            CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 2, ICOD)
            nAI = 99999
            IF (nA == 0 .AND. nI == 0) nAI = 0
            !
            ! --- Allocate V3_ array
            !
            WRITE(ICOD, '(6X, "NSizeV3_ = mAI * mJ * mB * mK * mC")')
            WRITE(ICOD, '(6X, "ALLOCATE(V3_(NSizeV3_))")')
            !
            ! --- Allocate and load V3 intermediate
            !
            CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
!072112Bgn
            IF ((nC /= 0) .OR. (nK /= 0)) THEN
!072112End
               !
               ! --- Make SUBROUTINE to rearrange V3 -> V3_
               !
               CALL MakeSrcArrange("V3", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               WRITE(ICOD, *) "     &   "//"V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
               !
!072112Bgn
            ELSE
               IF (Complx) THEN
                  WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeV3, V3, 1, V3_, 1)")')
               ELSE
                  WRITE(ICOD, '(6X, "CALL DCOPY(NSizeV3, V3, 1, V3_, 1)")')
               END IF
            END IF
!072112End
            !
            CALL FreeVarray("V3", "NSizeV3", "V3", "NONE", ICOD)
            WRITE(ICOD, '("!")')
!072112Bgn
               !
               ! Load T amplitudes
               !
               NTlevel = 2
               IF (NT_Typ2 >= 0) THEN
                  M1Temp = M1(2,NumDig)
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
               ELSE
                  M1Temp = IABS(M1(2,NumDig))
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
               END IF
               !
               WRITE(IOUT, '(4X, "Re-arrange T(2) amplitude")')
               !
               ! --- Re-arrange T if neccessary
               !
               nB = M_2
               nJ = L_2 - M_2
               nC = N_2 - nB
               nK = N_2 - nJ
               nD = nB + nC
               nL = nJ + nK
               nA = 0
               nI = 0
               !
               WRITE(ICOD, '(6X, "nB =", I3)') nB
               WRITE(ICOD, '(6X, "nJ =", I3)') nJ
               WRITE(ICOD, '(6X, "nC =", I3)') nC
               WRITE(ICOD, '(6X, "nK =", I3)') nK
               WRITE(ICOD, '(6X, "nD =", I3)') nD
               WRITE(ICOD, '(6X, "nL =", I3)') nL
               WRITE(ICOD, '(6X, "nA =", I3)') nA
               WRITE(ICOD, '(6X, "nI =", I3)') nI
               !
               WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
               WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
               WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
               WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
               WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
               WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
               WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &            "CCLib_NComb(NOc, nI)")') 
               !
               WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &            "CCLib_NComb(NVr, nB) * ", &
     &            "CCLib_NComb(NOc, nK) * ", &
     &            "CCLib_NComb(NVr, nC)")')
               !
               IF (NT_Typ2 >= 0) THEN
                  CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
               ELSE
                  CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
               END IF
               !
               IF ((nC == 0) .AND. (nJ == 0)) GO TO 3000
               IF ((nC == 0) .AND. (nK == 0)) GO TO 3000
               !
               nAI = 0
               CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               IF (NT_Typ2 >= 0) THEN
                  WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("T", ICOD)
               ELSE
                  WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("R", ICOD)
               END IF
               GO TO 3100
 3000          CONTINUE
               IF (NT_Typ2 >= 0) THEN
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
                  END IF
                  CALL FreeArray("T", ICOD)
               ELSE
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
                  END IF
                  CALL FreeArray("R", ICOD)
               END IF
 3100          CONTINUE
!072112End
            !
            ! --- Contract V3_ to T(or R)(2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(3X, "V2 += T2(", I3, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += T2(", I3, ")*V3")') NT_Typ2
            ELSE
               WRITE(IOUT, '(3X, "V2 += R2(" ,I4, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += R2(", I4, ")*V3")') NT_Typ2
            END IF
            !
            ! --- Load V2
            !
            CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
            !
            ! --- Load T amplitudes, Contraction: V3_*T += V2, Free T array
            !
            NTlevel = 2
            IF (NT_Typ2 >= 0) THEN
!GEMV               M1Temp = M1(2,NumDig)
!GEMV               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
!GEMV               CALL Contract("V3_", "T", "V2", NRank, NTlevel, &
!GEMV     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV               CALL FreeArray("T", ICOD)
               CALL ContractBLAS3("V3_", "T_", "V2", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("T_", ICOD)
            ELSE
!GEMV               M1Temp = IABS(M1(2,NumDig))
!GEMV               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "R")
!GEMV               CALL Contract("V3_", "R", "V2", NRank, NTlevel, &
!GEMV     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV               CALL FreeArray("R", ICOD)
               CALL ContractBLAS3("V3_", "R_", "V2", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("R_", ICOD)
            END IF
            !
            ! --- Free V3_ array
            !
            CALL FreeArray("V3_", ICOD)
            !
            ! --- Save and free V2
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
            !
            ! --- End of level-2 contraction
            !
         END DO   ! IT2
         !
         ! --- Contract V2 to T(1)
         !
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(2X, "V1 += T1(", I3, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += T1(", I3, ")*V2")') NT_Typ1
         ELSE
            WRITE(IOUT, '(2X, "V1 += R1(", I4, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += R1(", I4, ")*V2")') NT_Typ1
         END IF
!072112Bgn
         !
         ! Load T amplitudes
         !
         NTlevel = 1
         IF (NT_Typ1 >= 0) THEN
            M1Temp = M1(1,NumDig)
            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
         ELSE
            M1Temp = IABS(M1(1,NumDig))
            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
         END IF
         !
         WRITE(IOUT, '(4X, "Re-arrange T(1) amplitude")')
         !
         ! --- Re-arrange T if neccessary
         !
         nB = M_1
         nJ = L_1 - M_1
         nC = N_1 - nB
         nK = N_1 - nJ
         nD = nB + nC
         nL = nJ + nK
         nA = 0
         nI = 0
         !
         WRITE(ICOD, '(6X, "nB =", I3)') nB
         WRITE(ICOD, '(6X, "nJ =", I3)') nJ
         WRITE(ICOD, '(6X, "nC =", I3)') nC
         WRITE(ICOD, '(6X, "nK =", I3)') nK
         WRITE(ICOD, '(6X, "nD =", I3)') nD
         WRITE(ICOD, '(6X, "nL =", I3)') nL
         WRITE(ICOD, '(6X, "nA =", I3)') nA
         WRITE(ICOD, '(6X, "nI =", I3)') nI
         !
         WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
         WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
         WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
         WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
         WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
         WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
         WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &      "CCLib_NComb(NOc, nI)")')
         !
         WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &      "CCLib_NComb(NVr, nB) * ", &
     &      "CCLib_NComb(NOc, nK) * ", &
     &      "CCLib_NComb(NVr, nC)")')
         !
         IF (NT_Typ1 >= 0) THEN
            CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
         ELSE
            CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
         END IF
         !
         IF ((nC == 0) .AND. (nJ == 0)) GO TO 4000
         IF ((nC == 0) .AND. (nK == 0)) GO TO 4000
         !
         nAI = 0
         CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
         WRITE(ICOD, '(6X, "CALL", $)')
         WRITE(ICOD, *) TRIM(CWrk)//"( &"
         IF (NT_Typ1 >= 0) THEN
            WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
            CALL FreeArray("T", ICOD)
         ELSE
            WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
            CALL FreeArray("R", ICOD)
         END IF
         GO TO 4100
 4000    CONTINUE
         IF (NT_Typ1 >= 0) THEN
            IF (Complx)THEN
               WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
            END IF
            CALL FreeArray("T", ICOD)
         ELSE
            IF (Complx) THEN
               WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
            END IF
            CALL FreeArray("R", ICOD)
         END IF
4100     CONTINUE
!072112End
         !
         ! --- Load V1 and V2
         !
         CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
         CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
         !
         ! --- Load T amplitudes, Contraction: V2*T += V1, free T array
         !
         NTlevel = 1
         IF (NT_Typ1 >= 0) THEN
!            M1Temp = M1(1,NumDig)
!            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
!            CALL Contract("V2", "T", "V1", NRank, NTlevel, &
!     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!            CALL FreeArray("T", ICOD)
            CALL ContractBLAS3("V2", "T_", "V1", NRank, NTlevel, &
     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
            CALL FreeArray("T_", ICOD)
         ELSE
!072112            M1Temp = IABS(M1(1,NumDig))
!072112            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "R")
!072112            CALL Contract("V2", "R", "V1", NRank, NTlevel, &
!072112     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!072112            CALL FreeArray("R", ICOD)
            CALL ContractBLAS3("V2", "R_", "V1", NRank, NTlevel, &
     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
            CALL FreeArray("R_", ICOD)
         END IF
         !
         ! --- Free V2 array
         !
         CALL FreeVarray("V2", "NSizeV2", "V2", "NONE", ICOD)
         !
         ! --- Save and free V1
         !
         CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
         !
         ! --- End of level-1 contraction
         !
      END DO   ! IT1
      !
      WRITE(IOUT, '(/, X, I5, X, "diagrams have been processed")') NumDig
      WRITE(ICOD, '(/, "!", 5("-"), X, I5, X, "diagrams have been processed")') NumDig
      !
      IF (NumDig /= NDig) THEN
         WRITE(IOUT, '(3X, "!!! NumDig is not equal to NDig !!!")')
         WRITE(*, '(3X, "!!! NumDig is not equal to NDig !!!",2I6)') NumDig, NDig
         STOP
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenFactorizeL(NRank, NDig, M1, M2, M3, M13, Mwp, Facto, Complx)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Factorize diagrams: Lambda version (for now, only the simplest
        !     L + VT contraction is implemented)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : RightIP, LeftIP, RightEA, LeftEA
      !
      IMPLICIT NONE
      !
      LOGICAL :: Facto, Complx
      INTEGER :: NRank, NDig, M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=2) :: a2
      CHARACTER(LEN=255) :: c1, c2, c3, c4, CWrk, TName, VName
      INTEGER :: IOUT, ICOD
      INTEGER :: IDig, N, NumT1, NumT2, NumT3, NumT4, IT, IT1, IT2, IT3, IT4, NT_Typ1, NT_Typ2, NT_Typ3, NT_Typ4, &
     &   N_1, L_1, M_1, N_2, L_2, M_2, N_3, L_3, M_3, N_4, L_4, M_4, NCnt, NumDig, &
     &   nB, nJ, nC, nK, nD, nL, nA, nI, nAI, NTlevel
      INTEGER :: NT(4,NDig), NTyp1(NDig), NTyp2(NDig), NTyp3(NDig), NTyp4(NDig)
      INTEGER :: M1Temp
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      DO IT = 1, 4
         RightIP(IT) = .FALSE.
         RightEA(IT) = .FALSE.
         LeftIP(IT) = .FALSE.
         LeftEA(IT) = .FALSE.
      END DO
      !
      ! --- Make 3-digit integers for T and L verteces
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            NT(N,IDig) = M1(N,IDig) * 100 + M2(N,IDig) * 10 + M3(N,IDig)   !<-- M1, M2 and M3 should be less than 10
         END DO   ! N
      END DO   ! IDig
      !
      ! --- Check the # and types of the outermost L vertex
      !
      IF (Facto) THEN
         NumT1 = 0   ! # of types of outermost L
         DO IDig = 1, NDig   ! look for non-zero outermost L
            IF ((IDig > 1) .AND. (NT(1,IDig) == NT(1,IDig-1))) CYCLE
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE   ! L-vertex
               WRITE(IOUT, '(X, "A new L1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new L1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      ELSE
         NumT1 = 0
         DO IDig = 1, NDig
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE   ! L-vertex
               WRITE(IOUT, '(X, "A new L1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new L1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      END IF
      WRITE(IOUT, '(/, "Start residue calculation", /)')
      !
      ! --- Size of V1 array (NOc: occ. MO number, NVr: vir. MO number)
      ! --- Integer function CCLib_NComb(N,n) computes N!/(N-n)!
      !
      WRITE(c1, '(I3)') NRank
      CWrk = "CCLib_NComb(NOc,"//TRIM(c1)//")*CCLib_NComb(NVr,"//TRIM(c1)//")"
      WRITE(IOUT, '(/, X, "Initialize V1 of size ", A100)') CWrk
      WRITE(ICOD, '(/, "!", 5("-"), X, "Initialize V1 of size ", A100)') CWrk
      !
      WRITE(ICOD, '(6X, "NSizeV1 = CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ", I2, ")")') NRank, NRank
      CALL GetVarray("V1", "NSizeV1", "V1", "INIT", ICOD)
      CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
      NumDig = 0
      WRITE(ICOD, '(6X, "NumDig = 0", /)')
      !
      DO IT1 = 1, NumT1
         !
         NT_Typ1 = NTyp1(IT1)
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(/, X, "Processing T1(", I3, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T1(", I3, ")", X, 3("-"), /, "!")') NT_Typ1
         ELSE
            WRITE(IOUT, '(/, X, "Processing L1(", I4, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing L1(", I4, ")", X, 3("-"), /, "!")') NT_Typ1
         END IF
!_YA         IF (NT_Typ1 == 0) THEN   ! Substitute G into V1, IF there is no 1st T vertex, and GO TO the next T(1)
!_YA            NumDig = NumDig + 1
!_YAC     --- Read interaction vertex from disc
!_YA            CALL DefGsize(M13(NumDig), ICOD)
!_YA            CALL GetGarray(M13(NumDig), ICOD, NumDig)
!_YAC     --- Substitute interaction vertex to V1
!_YA            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
!_YA            CALL AddGtoV("V1", "NSizeV1", M13(NumDig), Mwp(NumDig),
!_YA     *           IOUT, ICOD)
!_YA            CALL FreeArray("G", ICOD)
!_YAC     --- Save and free V1
!_YA            CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
!_YA            CYCLE
!_YA         END IF
         !
         ! --- Initialize V2
         !
         N_1 = NT_Typ1 / 100
         L_1 = (NT_Typ1 - 100 * N_1) / 10
         M_1 = NT_Typ1 - 100 * N_1 - 10 * L_1
         IF (NT_Typ1 < 0) THEN
            N_1 = -N_1
            L_1 = -L_1
            M_1 = -M_1
         END IF
         WRITE(c1, '(I3)') M_1
         WRITE(c2, '(I3)') L_1 - M_1
         WRITE(c3, '(I3)') NRank - N_1 + M_1
         WRITE(c4, '(I3)') NRank - N_1 + L_1 - M_1
         CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &      //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &      //")"
         !
         WRITE(IOUT, '(2X, "Initialize V2 of size ", A100)') CWrk
         WRITE(ICOD, '("!", 5("-"), X, "Initialize V2 of size ", A100)') CWrk
         !
         WRITE(ICOD, '(6X, "NSizeV2 = CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ", I2, &
     &      ") * CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ", I2, ")")') &
     &      M_1, L_1 - M_1, NRank - N_1 + M_1, NRank - N_1 + L_1 - M_1
         !
         CALL GetVarray("V2", "NSizeV2", "V2", "INIT", ICOD)
         CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
         !
         ! --- Find T(2) or L(2) vertices for current T(1) or L(1)
         !
         IF (Facto) THEN
            NumT2 = 0
            NCnt = 0
            DO IDig = 1, NDig
               IF (NT(1,IDig) /= NT_Typ1) CYCLE
               NCnt = NCnt + 1
               IF ((NCnt > 1) .AND. (NT(2,IDig) == NT(2,IDig-1))) CYCLE
               NumT2 = NumT2 + 1
               NTyp2(NumT2) = NT(2,IDig)
               IF (NTyp2(NumT2) >= 0) THEN
                  WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               ELSE
                  WRITE(IOUT, '(2X, "A new L2 vertex ", I4, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new L2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               END IF
            END DO   ! IDig
         ELSE
            NumT2 = 1
            NTyp2(NumT2) = NT(2,IT1)
            IF (NTyp2(NumT2) >= 0) THEN
               WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
            ELSE
               WRITE(IOUT, '(2X, "A new L2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new L2 vertex ", I4, X, "found.")') NTyp2(NumT2)
            END IF
         END IF
         !
         DO IT2 = 1, NumT2
            !
            NT_Typ2 = NTyp2(IT2)
!
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(/, X, "Processing T2(", I3, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T2(", I3, ")", X, 3("-"), /, "!")') NT_Typ2
            ELSE
               WRITE(IOUT, '(/, X, "Processing L2(", I4, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing L2(", I4, ")", X, 3("-"), /, "!")') NT_Typ2
            END IF
            !
            IF (NT_Typ2 == 0) THEN   ! Substitute G into V2 IF there is no 2nd T vertex
               NumDig = NumDig + 1
               !
               ! --- Read interaction vertex from disc
               !
               CALL DefGsize(M13(NumDig), ICOD)
               CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
               !
               ! --- Substitute G to V2
               !
               CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
               CALL AddGtoV("V2", "NSizeV2", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
               CALL FreeArray("G", ICOD)
               !
               ! --- Save V2 on disk
               !
               CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
               CYCLE
            END IF
            !
            ! --- Initialize V3
            !
            N_2 = NT_Typ2 / 100
            L_2 = (NT_Typ2 - 100 * N_2) / 10
            M_2 = NT_Typ2 - 100 * N_2 - 10 * L_2
            IF (NT_Typ2 < 0) THEN
               N_2 = -N_2
               L_2 = -L_2
               M_2 = -M_2               
            END IF
            WRITE(c1, '(I3)') M_1 + M_2
            WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2)
            WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2)
            WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2)
            CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &           //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &           //")"
            !
            WRITE(IOUT, '(3X, "Initialize V3 of size ", A100)') CWrk
            WRITE(ICOD, '("!", 5("-"), X, "Initialize V3 of size ", A100)') CWrk
            !
            WRITE(ICOD, '(6X, "NSizeV3 = CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2, &
     &         ") * CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2,")")') &
     &         M_1 + M_2, (L_1 - M_1) + (L_2 - M_2), NRank - (N_1 - M_1) - (N_2 - M_2), &
     &         NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2)
            CALL GetVarray("V3", "NSizeV3", "V3", "INIT", ICOD)
            CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
            !
            ! --- Find T(3) or L(3) for current T(or L)(1) and T(or L)(2)
            !
            IF (Facto) THEN
               NumT3 = 0
               NCnt = 0
               DO IDig = 1, NDig
                  IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2)) CYCLE
                  NCnt = NCnt + 1
                  IF ((NCnt > 1) .AND. (NT(3,IDig) == NT(3,IDig-1))) CYCLE
                  NumT3 = NumT3 + 1
                  NTyp3(NumT3) = NT(3,IDig)
                  IF (NTyp3(NumT3) >= 0) THEN
                     WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  ELSE
                     WRITE(IOUT, '(3X, "A new L3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new L3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  END IF
               END DO   ! IDig
            ELSE
               NumT3 = 1
               NTyp3(NumT3) = NT(3,IT1)
               IF (NTyp3(NumT3) >= 0) THEN
                  WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
               ELSE
                  WRITE(IOUT, '(3X, "A new L3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new L3 vertex ", I4, X, "found.")') NTyp3(NumT3)
               END IF
            END IF
            !
            DO IT3 = 1, NumT3
               !
               NT_Typ3 = NTyp3(IT3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(/, X, "Processing T3(", I3, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T3(", I3, ")", X, 3("-"), /, "!")') NT_Typ3
               ELSE
                  WRITE(IOUT, '(/, X, "Processing L3(", I4, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing L3(", I4, ")", X, 3("-"), /, "!")') NT_Typ3
               END IF
               !
               IF (NT_Typ3 == 0) THEN   ! Substitute G into V3 IF there is no 3rd T vertex
                  NumDig = NumDig + 1
                  !
                  ! --- Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Substitute G into V3
                  !
                  CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
                  CALL AddGtoV("V3", "NSizeV3", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                  CALL FreeArray("G", ICOD)
                  !
                  ! --- Save V3 on disk
                  !
                  CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
                  CYCLE
               END IF
               !
               ! --- Initialize V4
               !
               N_3 = NT_Typ3 / 100
               L_3 = (NT_Typ3 - 100 * N_3) / 10
               M_3 = NT_Typ3 - 100 * N_3 - 10 * L_3
               IF (NT_Typ3 < 0) THEN
                  N_3 = -N_3
                  L_3 = -L_3
                  M_3 = -M_3
               END IF
               WRITE(c1, '(I3)') M_1 + M_2 + M_3
               WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3)
               WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2) - (N_3 - M_3)
               WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2)     &
     &            //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &            //")"
               !
               WRITE(IOUT, '(4X, "Initialize V4 of size ", A100)') CWrk
               WRITE(ICOD, '("!", 5("-"), X, "Initialize V4 of size ", A100)') CWrk
               !
               WRITE(ICOD, '(6X, "NSizeV4 = CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ") * CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ")")') &
     &            M_1 + M_2 + M_3, (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3), &
     &            NRank - (N_1 - M_1) - (N_2 - M_2) - (N_3-M_3), &
     &            NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               CALL GetVarray("V4", "NSizeV4", "V4", "INIT", ICOD)
               CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
               !
               ! --- Fint T(or L)(4) for current T(or L)(1), T(or L)(2), and T(or L)(3)
               !
               IF (Facto) THEN
                  NumT4 = 0
                  NCnt = 0
                  DO IDig = 1, NDig
                     IF (NT(1,IDig) /= NT_Typ1 .OR. &
     &                   NT(2,IDig) /= NT_Typ2 .OR. &
     &                   NT(3,IDig) /= NT_Typ3) CYCLE
                     NCnt = NCnt + 1
                     IF ((NCnt > 1) .AND. (NT(4,IDig) == NT(4,IDig-1))) CYCLE
                     NumT4 = NumT4 + 1
                     NTyp4(NumT4) = NT(4,IDig)
                     IF (NTyp4(NumT4) >= 0) THEN
                        WRITE(IOUT, '(4X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, &
     &                     '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     ELSE
                        WRITE(IOUT, '(4X, "A new L4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, &
     &                     '("!", 5("-"), X, "A new L4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     END IF
                  END DO   ! IDig
               ELSE
                  NumT4 = 1
                  NTyp4(NumT4) = NT(4,IT1)
                  IF (NTyp4(NumT4) >= 0) THEN
                     WRITE(IOUT, '(4X, "A new T3 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                  ELSE
                     WRITE(IOUT, '(4X, "A new L4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new L4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                  END IF
               END IF
               !
               DO IT4 = 1, NumT4 
                  !
                  NT_Typ4 = NTyp4(IT4)
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(/, X, "Processing T4(", I3, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T4(", I3, ")", X, 3("-"), /,"!")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(/, X, "Processing L4(", I4, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing L4(", I4, ")", X, 3("-"), /, "!")') NT_Typ4
                  END IF
                  !
                  IF (NT_Typ4 == 0) THEN   ! Substitute G into V4, IF there is no 4th T vertex
                     NumDig = NumDig + 1
                     !
                     ! --- Read interaction vertex from disc
                     !
                     CALL DefGsize(M13(NumDig), ICOD)
                     CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                     !
                     ! --- Substitute G into V4
                     !
                     CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                     CALL AddGtoV("V4", "NSizeV4", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                     CALL FreeArray("G", ICOD)
                     !
                     ! --- Save and free V4
                     !
                     CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                     CYCLE
                  END IF
                  !
                  ! --- Get interaction vertex
                  !
                  NumDig = NumDig + 1
                  !
                  ! --- Allocate G; Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Allocate G_
                  !
                  IF (M1(4,NumDig) < 0) THEN
                     CALL DefG_Size(M13(NumDig), -M1(4,NumDig), -M2(4,NumDig), -M3(4,NumDig), ICOD)
                  ELSE
                     CALL DefG_Size(M13(NumDig), M1(4,NumDig), M2(4,NumDig), M3(4,NumDig), ICOD)
                  END IF
                  CALL GetVarray("G_", "NSizeG_", " ", "NONE", ICOD)
                  !
                  ! --- Re-arrange G
                  !
                  WRITE(IOUT, '(4X, "Re-arrange G array")')
                  WRITE(ICOD, '("!", 5("-"), X, "Re-arrange G array")')
                  CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank,  &
     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 4, ICOD) 
                  nAI = nA + nI
                  CALL MakeSrcArrange("G", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  WRITE(ICOD, *) "     &   "//"G, G_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
                  !
                  ! --- Free G
                  !
                  CALL FreeArray("G", ICOD)
                  !
                  ! --- Contract G_ to T(or L)(4); Add to V4
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(5X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                  ELSE
!_YA                     WRITE(IOUT, '(5X,"V4 += R4(",I4,")*I5")') NT_Typ4
!_YA                     WRITE(ICOD, '("!",5("-"),X,"V4 += R4(",I4,")*I5")')
!_YA     *                  NT_Typ4
                  END IF
                  !
                  ! --- Load V4
                  !
                  CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                  !
                  ! --- Load T amplitudes, Contraction: G_*T += V4, Free T
                  !
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
                     M1Temp = M1(4,NumDig)
                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                     CALL Contract("G_", "T", "V4", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("T", ICOD)
                  ELSE
!_YA                     M1Temp = IABS(M1(4,NumDig))
!_YA                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
!_YA                     CALL Contract("G_", "R", "V4", NRank, NTlevel,
!_YA     *                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), ICOD)
!_YA                     CALL FreeArray("R",ICOD)
                  END IF
                  !
                  ! --- Free G_
                  !
                  CALL FreeArray("G_" ,ICOD)
                  !
                  ! --- Save and free V4
                  !
                  CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                  !
                  ! --- End of level-4 contraction
                  !
               END DO   ! IT4 (loop over 4th T-vertex)
               !
               WRITE(IOUT, '(4X, "Re-arrange V4 array")')
               WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V4 array")')
               CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 3, ICOD)
               nAI = 99999
               IF (nA == 0 .AND. nI == 0) nAI = 0
               !
               ! --- Allocate V4_ array
               !
               WRITE(ICOD, '(6X, "NSizeV4_ = mAI * mJ * mB * mK * mC")')
               WRITE(ICOD, '(6X, "ALLOCATE(V4_(NSizeV4_))")')
               !
               ! --- Allocate and load V4 intermediate
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
               !
               ! --- Make SUBROUTINE to rearrange V4 -> V4_
               !
               CALL MakeSrcArrange("V4", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               WRITE(ICOD, *) "     &   "//"V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
               CALL FreeVarray("V4", "NSizeV4", "V4", "NONE", ICOD)
               WRITE(ICOD, '(/)')
               !
               ! --- Contract V4_ to T(3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(5X, "V3 += T3(", I3, ")*V4")') NT_Typ3
                  WRITE(ICOD, '("!", 5("-"), X, "V3 += T3(", I3, ")*V4")') NT_Typ3
               ELSE
!_YA                  WRITE(IOUT, '(5X,"V3 += R3(",I4,")*V4")') NT_Typ3
!_YA                  WRITE(ICOD, '("!",5("-"),X,"V3 += R3(",I4,")*V4")') 
!_YA     *               NT_Typ3
               END IF
               !
               ! --- Load V3
               !
               CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
               !
               ! --- Load T amplitudes, Contraction: V4_*T += V3, Free T array
               !
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
                  M1Temp = M1(3,NumDig)
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                  CALL Contract("V4_", "T", "V3", NRank, NTlevel, &
     &               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                  CALL FreeArray("T", ICOD)
               ELSE
!_YA                  M1Temp = IABS(M1(3,NumDig))
!_YA                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
!_YA                  CALL Contract("V4_", "R", "V3", NRank, NTlevel,
!_YA     *               M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), ICOD)
!_YA                  CALL FreeArray("R", ICOD)
               END IF
               !
               ! --- Free V4_ array
               !
               CALL FreeArray("V4_", ICOD)
               !
               ! --- Save and free V3
               !
               CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
               !
               ! --- End of level-3 contraction
               !
            END DO   ! IT3
            !
            WRITE(IOUT, '(4X, "Re-arrange V3 array")')
            WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V3 array")')
            CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 2, ICOD)
            nAI = 99999
            IF (nA == 0 .AND. nI == 0) nAI = 0
            !
            ! --- Allocate V3_ array
            !
            WRITE(ICOD, '(6X, "NSizeV3_ = mAI * mJ * mB * mK * mC")')
            WRITE(ICOD, '(6X, "ALLOCATE(V3_(NSizeV3_))")')
            !
            ! --- Allocate and load V3 intermediate
            !
            CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
            !
            ! --- Make SUBROUTINE to rearrange V3 -> V3_
            !
            CALL MakeSrcArrange("V3", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
            WRITE(ICOD, '(6X, "CALL", $)')
            WRITE(ICOD, *) TRIM(CWrk)//"( &"
            WRITE(ICOD, *) "     &   "//"V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
            CALL FreeVarray("V3", "NSizeV3", "V3", "NONE", ICOD)
            WRITE(ICOD, '(/)')
            !
            ! --- Contract V3_ to T(2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(3X, "V2 += T2(", I3, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += T2(", I3, ")*V3")') NT_Typ2
            ELSE
!_YA               WRITE(IOUT, '(3X, "V2 += R2(",I4,")*V3")') NT_Typ2
!_YA               WRITE(ICOD, '("!", 5("-"), X, "V2 += R2(", I4, ")*V3")') 
!_YA     *            NT_Typ2
            END IF
            !
            ! --- Load V2
            !
            CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
            !
            ! --- Load T amplitudes, Contraction: V3_*T += V2, Free T array
            !
            NTlevel = 2
            IF (NT_Typ2 >= 0) THEN
               M1Temp = M1(2,NumDig)
               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
               CALL Contract("V3_", "T", "V2", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("T", ICOD)
            ELSE
!_YA               M1Temp = IABS(M1(2,NumDig))
!_YA               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "R")
!_YA               CALL Contract("V3_", "R", "V2", NRank, NTlevel,
!_YA     *            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), ICOD)
!_YA               CALL FreeArray("R", ICOD)
            END IF
            !
            ! --- Free V3_ array
            !
            CALL FreeArray("V3_", ICOD)
            !
            ! --- Save and free V2
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
            !
            ! --- End of level-2 contraction
            !
         END DO   ! IT2
         !
         ! --- Contract V2 to T(1) or L
         !
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(2X, "V1 += T1(", I3, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += T1(", I3, ")*V2")') NT_Typ1
         ELSE
            WRITE(IOUT, '(2X, "V1 += L1(", I4, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += L1(", I4, ")*V2")') NT_Typ1
         END IF
         !
         ! --- Load V1 and V2
         !
         IF (NT_Typ1 >= 0) THEN
            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
         END IF
         CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
         !
         ! --- Load T or L amplitudes, Contraction: V2*T += V1, free T array
         !
         NTlevel = 1
         IF (NT_Typ1 >= 0) THEN
            M1Temp = M1(1,NumDig)
            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
            CALL Contract("V2", "T", "V1", NRank, NTlevel, &
     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
            CALL FreeArray("T", ICOD)
         ELSE
            M1Temp = IABS(M1(1,NumDig))
            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "L")
            CALL ContractL1("V2", "L", "V1", NRank, NTlevel, M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), ICOD)
            CALL FreeArray("L", ICOD)
         END IF
         !
         ! --- Free V2 array
         !
         CALL FreeVarray("V2", "NSizeV2", "V2", "NONE", ICOD)
         !
         ! --- Save and free V1
         !
         CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
         !
         ! --- End of level-1 contraction
         !
      END DO   ! IT1
      !
      WRITE(IOUT, '(/, X, I5, X, "diagrams have been processed")') NumDig
      WRITE(ICOD, '(/, "!", 5("-"), X, I5, X, "diagrams have been processed")') NumDig
      !
      IF (NumDig /= NDig) THEN
         WRITE(IOUT, '(3X, "!!! NumDig is not equal to NDig !!!")')
         WRITE(*, '(3X, "!!! NumDig is not equal to NDig !!!",2I6)') NumDig, NDig
         STOP
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Int2Char(I, Cout, A)
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      !     Convert a 2-digit integer to character*2
      !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      CHARACTER(LEN=1) :: A
      CHARACTER(LEN=2) :: Cout
      INTEGER :: I
      !
      INTEGER :: J
      !
      Cout = '  '
      WRITE(Cout, '(I2)') I
      DO J = 1, 2
         IF (Cout(J:J) == ' ') Cout(J:J) = A
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MakeSrcArrange(AName, nB, nJ, nC, nK, nD, nL, mAI, RtnName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Generate the routine which rearranges the intermedeate
        !     V(DLAI) --> V'(CKBJAI)
        !     Name of the generated routine involves nB, nJ, nD, and nL
        !     "N" or "X" at the bottom of the name means:
        !         "N" ... no external lines
        !         "X" ... one or more external lines
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx, PreFixSrc
      USE Vertex_Module, ONLY : NExct, NInter, NIntPart
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=2) :: AName
      CHARACTER(LEN=255) :: RtnName
      INTEGER :: nB, nJ, nC, nK, nD, nL, mAI
      !
      CHARACTER(LEN=2) :: a21, a22, a23, a24
      CHARACTER(LEN=3) :: AName_
      CHARACTER(LEN=7) :: LpName1(100), LpName2(100)
      CHARACTER(LEN=255) :: BasName, RtnName_lw
      LOGICAL :: Finish
      INTEGER :: IOUT, ICOD1, ICOD2
      INTEGER :: iJ, iB, iK, iC
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      ! --- Name of the SUBROUTINE
      !
      BasName = TRIM(PreFixSrc)//"ReArrange"
      !
      CALL Int2Char(nB, a21, "0")
      CALL Int2Char(nJ, a22, "0")
      CALL Int2Char(nD, a23, "0")
      CALL Int2Char(nL, a24, "0")
      RtnName = TRIM(BasName)//"_B"//a21//"J"//a22//"D"//a23//"L"//a24
      IF (mAI == 0) THEN
         RtnName = TRIM(RtnName)//"N"   ! No external lines
      ELSE
         RtnName = TRIM(RtnName)//"X"   ! With external lines
      END IF
      !
      RtnName = TRIM(RtnName)//"_AutoGen"
      !
      ! --- Convert to lower CASE for filename
      !
      RtnName_lw = RtnName
      CALL Up2Lw(RtnName_lw)
      !
      OPEN(UNIT=ICOD2, FILE=TRIM(RtnName_lw)//".f90", STATUS='UNKNOWN')
      REWIND(ICOD2)
      !
      ! --- Header
      !
      WRITE(ICOD2, '(6X, "SUBROUTINE ", $)')
      WRITE(ICOD2, *) TRIM(RtnName)//"  &"
      WRITE(ICOD2, *) "     &   "//"(V, V_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
      WRITE(ICOD2, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!", /, &
     &   "!", 5X, "Re-arranges the array in the form (D,L,AI)", X, &
     &   "to the form (C,K,B,J,AI)", /, &
     &   "!", 5X,"nB, nJ, ...; # of occupations", /, &
     &   "!", 5X,"mB, mJ, ...: # of all possible strings", /, &
     &   "!")')
      !
      WRITE(ICOD2, '(6X, "USE CC_Module, ONLY : NOc, NVr, IArcWgtOcc, IArcWgtVir")')
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "IMPLICIT NONE")')
      !
      ! --- Declaration of input/output variables
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "INTEGER :: nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK")')
      IF (mAI /= 0) THEN
         IF (Complx) THEN
            WRITE(ICOD2, '(6X, "COMPLEX(8):: V(mD,mL,mAI), V_(mC,mK,mB,mJ,mAI)")')
         ELSE
            WRITE(ICOD2, '(6X, "REAL(8):: V(mD,mL,mAI), V_(mC,mK,mB,mJ,mAI)")')
         END IF
      ELSE
         IF (Complx) THEN
            WRITE(ICOD2, '(6X, "COMPLEX(8):: V(mD,mL), V_(mC,mK,mB,mJ)")')
         ELSE
            WRITE(ICOD2, '(6X, "REAL(8):: V(mD,mL), V_(mC,mK,mB,mJ)")')
         END IF
      END IF
      !
      ! --- Declaration of local variables
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "LOGICAL :: COIN")')
      IF (nB /= 0) WRITE(ICOD2, '(6X, "INTEGER :: NStringB(nB)")')
      IF (nJ /= 0) WRITE(ICOD2, '(6X, "INTEGER :: NStringJ(nJ)")')
      IF (nC /= 0) WRITE(ICOD2, '(6X, "INTEGER :: NStringC(nD-nB)")')
      IF (nK /= 0) WRITE(ICOD2, '(6X, "INTEGER :: NStringK(nL-nJ)")')
      IF ((nB /= 0) .AND. (nC /= 0)) WRITE(ICOD2, '(6X, "INTEGER :: NStringCB(nD)")')
      IF ((nJ /= 0) .AND. (nK /= 0)) WRITE(ICOD2, '(6X, "INTEGER :: NStringKJ(nL)")')
      !
!072012Bgn
      !
      ! --- Integer scalars
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "INTEGER :: nC, nK, jB")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddJ")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddB")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddK")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddC")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddKJ")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddCB")')
      WRITE(ICOD2, '(6X, "INTEGER :: ISgnKJ")')
      WRITE(ICOD2, '(6X, "INTEGER :: ISgnCB")')
      !
      IF (mAI /= 0) WRITE(ICOD2, '(6X, "INTEGER :: iAI")')
      !
      IF (nJ /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iJ00, IAddJ00")')
         DO iJ = 1, nJ
            CALL Int2Char(iJ, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iJ", A2, ", IAddJ", A2)') a21, a21
         END DO
      END IF
      !
      IF (nB /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iB00, IAddB00")')
         DO iB = 1, nB
            CALL Int2Char(iB, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iB", A2, ", IAddB", A2)') a21, a21
         END DO
      END IF
      !
      IF (nK /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iK00, IAddK00")')
         DO iK = 1, nK
            CALL Int2Char(iK, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iK", A2, ", IAddK", A2)') a21, a21
         END DO
      END IF
      !
      IF (nC /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iC00, IAddC00")')
         DO iC = 1, nC
            CALL Int2Char(iC, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iC", A2, ", IAddC", A2)') a21, a21
         END DO
      END IF
!072012End
      !
      ! --- Initialize V_ array
      !
      IF (mAI /= 0) THEN
         IF (Complx) THEN
            WRITE(ICOD2, '(6X, "CALL CCLib_ZClear(V_, mC*mK*mB*mJ*mAI)")')
         ELSE
            WRITE(ICOD2, '(6X, "CALL CCLib_DClear(V_, mC*mK*mB*mJ*mAI)")')
         END IF
      ELSE
         IF (Complx) THEN
            WRITE(ICOD2, '(6X, "CALL CCLib_ZClear(V_, mC*mK*mB*mJ)")')
         ELSE
            WRITE(ICOD2, '(6X, "CALL CCLib_DClear(V_, mC*mK*mB*mJ)")')
         END IF
      END IF
      !
      WRITE(ICOD2, '(6X, "nK = nL - nJ")')
      WRITE(ICOD2, '(6X, "nC = nD - nB")')
      !
      ! --- Loop over I and A (External indices)
      !
      IF (mAI /= 0) WRITE(ICOD2, '(6X, "DO iAI = 1, mAI", /)')   ! IA loop
      !
      ! --- Loop over J (Fixed holes); Get Address of J-string
      !
      IF (nJ /= 0) THEN
         WRITE(ICOD2, '(9X, "iJ00 = 0")')
         WRITE(ICOD2, '(9X, "IAddJ00 = 1")')   !072012
         DO iJ = 1, nJ
            CALL Int2Char(iJ, a21, "0")
            CALL Int2Char(iJ-1, a22, "0")
            WRITE(ICOD2, '(9X, "DO ", A4, " = ", A8, ", NOc", A8, I2)') &
     &         "iJ"//a21, "iJ"//a22//" + 1", " - nJ + ", iJ
            WRITE(ICOD2, '(12X, "NStringJ(", I2, ") = ", A4)') iJ,"iJ"//a21
!072012Bgn
            WRITE(ICOD2, '(12X, "IAddJ", A2, " = IAddJ", A2, " + IArcWgtOcc(iJ", &
     &         A2, ", ", I2, ", nJ)")') a21, a22, a21, iJ
            IF (iJ == nJ) THEN
               WRITE(ICOD2, '(12X, "IAddJ = IAddJ", A2)') a21
            END IF
!072012End
         END DO
!072012         WRITE(ICOD2, '(12X, "CALL CCLib_StringAddress(NStringJ, nJ, ""Occ"", IAddJ)")')
      ELSE   ! nJ = 0
         WRITE(ICOD2, '(12X, "IAddJ = 1")')
      END IF
      !
      ! --- Loop over B (Fixed particles), Get Address of B-string
      !
      IF (nB /= 0) THEN
         WRITE(ICOD2, '(/, 12X, "iB00 = 0")')
         WRITE(ICOD2, '(12X, "IAddB00 = 1")')   !072012
         DO iB = 1, nB
            CALL Int2Char(iB, a21, "0")
            CALL Int2Char(iB - 1, a22, "0")
            WRITE(ICOD2, '(12X, "DO ", A4, " = ", A8, ", NVr", A8, I2)') &
     &         "iB"//a21, "iB"//a22//" + 1", " - nB + ", iB
            WRITE(ICOD2, '(15X, "NStringB(", I2, ") = ", A4)') iB, "iB"//a21
!072012Bgn
            WRITE(ICOD2, '(15X, "IAddB", A2, " = IAddB", A2, " + IArcWgtVir(iB", &
     &         A2, ", ", I2, ", nB)")') a21, a22, a21, iB
            IF (iB == nB) THEN
               WRITE(ICOD2, '(15X, "IAddB = IAddB", A2)') a21
            END IF
!072012End
         END DO
!072012         WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringB, nB, ", &
!072012     &      """Vir"", IAddB)")')
      ELSE   ! nB = 0
         WRITE(ICOD2, '(15X, "IAddB = 1")')
      END IF
      !
      ! --- Loop over K (Free holes), Get Address of K-string
      !
      IF (nK /= 0) THEN
         !
         WRITE(ICOD2, '(/, 15X, "iK00 = 0")')
         WRITE(ICOD2, '(15X, "IAddK00 = 1")')   ! 072012
         DO iK = 1, nK
            CALL Int2Char(iK, a21, "0")
            CALL Int2Char(iK - 1, a22, "0")
            LpName1(iK) = "Lp_iK"//a21
            WRITE(ICOD2, '(15X, A7, ": DO", X, A6, " = ", A8, ", NOc", A8, I2)') &
     &         LpName1(iK), "iK"//a21, "iK"//a22//" + 1", " - nK + ", iK
            WRITE(ICOD2, '(18X, "NStringK(", I2, ") = ", A4)') iK, "iK"//a21
!072012Bgn
            DO iJ = 1, nJ
               CALL Int2Char(iJ, a23, "0")
               WRITE(ICOD2, '(18X, "IF (iK", A2, " == iJ", A2, ") CYCLE")') a21, a23
            END DO
            !
            WRITE(ICOD2, '(18X, "IAddK", A2, " = IAddK", A2, " + IArcWgtOcc(iK", &
     &         A2, ", ", I2, ", nK)")') a21, a22, a21, iK
            IF (iK == nK) THEN
               WRITE(ICOD2, '(18X, "IAddK = IAddK", A2)') a21
            END IF
!072012End
         END DO
!072012         WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress", "(NStringK, nK, ""Occ"", IAddK)")')
         !
         ! --- String K.J (Sign and Address)
         !
         IF (nJ /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string K.J", &
     &         X, 3("-"), /, "!")')
            WRITE(ICOD2, '(18X, "COIN = .FALSE.")')
            WRITE(ICOD2, '(18X, "CALL CCLib_MergeStrings(NStringK, NStringJ," &
     &         "NStringKJ, nK, nJ, ISgnKJ, COIN)")')
            WRITE(ICOD2, '(18X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringKJ, nK+nJ, ""Occ"", IAddKJ)")')
         ELSE   ! nK /= 0 and nJ = 0
            WRITE(ICOD2, '(18X, "ISgnKJ = 1", /, 18X, "IAddKJ = IAddK")')
         END IF
         !
      ELSE   ! nK = 0
         !
         IF (nJ /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string KJ", X, &
     &         3("-"), /, "!")')
            WRITE(ICOD2, '(18X, "IAddK = 1", /, 18X, "ISgnKJ = 1", /, &
     &         18X, "IAddKJ = IAddJ")')
!072012     &         18X, "CALL CCLib_StringAddress(NStringJ, nJ, ""Occ"", IAddKJ)")')
         ELSE
            WRITE(ICOD2, '(18X, "IAddK = 1", /, 18X,"ISgnKJ = 1",/, 18X,"IAddKJ = 1")')
         END IF
         !
      END IF
      !
      ! --- Loop over C (Free particles), Get Address of C-string
      !
      IF (nC /= 0) THEN
         !
         WRITE(ICOD2, '(/, 18X, "iC00 = 0")')
         WRITE(ICOD2, '(18X, "IAddC00 = 1")')   !072012
         DO iC = 1, nC
            CALL Int2Char(iC, a21, "0")
            CALL Int2Char(iC - 1, a22, "0")
            LpName2(iC) = "Lp_iC"//a21
            WRITE(ICOD2, '(18X, A7, ": DO", X, A4, " = ", A8, ", NVr", A8, I2)')  &
     &         LpName2(iC), "iC"//a21, "iC"//a22//" + 1", " - nC + ", iC
            WRITE(ICOD2, '(21X, "NStringC(", I2, ") = ", A4)') iC, "iC"//a21
!072012Bgn
            DO iB = 1, nB
               CALL Int2Char(iB, a23, "0")
               WRITE(ICOD2, '(21X, "IF (iC", A2, " == iB", A2, ") CYCLE")') a21, a23
            END DO
            !
            WRITE(ICOD2, '(21X, "IAddC", A2, " = IAddC", A2, " + IArcWgtVir(iC", &
     &         A2, ", ", I2, ", nC)")') a21, a22, a21, iC
            IF (iC == nC) THEN
               WRITE(ICOD2, '(21X, "IAddC = IAddC", A2)') a21
            END IF
!072012End
         END DO
!072012         WRITE(ICOD2, '(21X, "CALL CCLib_StringAddress", "(NStringC, nC, ""Vir"", IAddC)")')
         !
         ! --- Address of string C.B
         !
         IF (nB /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string C.B", X, &
     &         3("-"), /, "!")')
            WRITE(ICOD2, '(21X, "COIN = .FALSE.")')
            WRITE(ICOD2, '(21X, "CALL CCLib_MergeStrings(NStringC, NStringB, ", &
     &         "NStringCB, nC, nB, ISgnCB, COIN)")')
            WRITE(ICOD2, '(21X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '(21X, "CALL CCLib_StringAddress(NStringCB, nC+nB, ""Vir"", IAddCB)")')
         ELSE   ! nB = 0
            WRITE(ICOD2, '(21X, "ISgnCB = 1", /, 21X, "IAddCB = IAddC")')
         END IF
         !
      ELSE   ! nC = 0
         !
         IF (nB /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string B", X, &
     &         3("-"), /, "!")')
            WRITE(ICOD2, '(21X, "IAddC = 1", /, 21X, "ISgnCB = 1", /, &
     &         21X, "IAddCB = IAddB")')
         ELSE
            WRITE(ICOD2, '(21X, "IAddC = 1", /, &
     &         21X, "ISgnCB = 1", /, &
     &         21X, "IAddCB = 1")')
         END IF
         !
      END IF
      !
      ! --- Rearrange the intermediate
      !
      WRITE(ICOD2, '("!", /, &
     &     "!", 5X, "--- Rearrange the intermediate", X, &
     &     3("-"), /, "!")')
      IF (mAI == 0) THEN
         IF (Complx) THEN
            WRITE(ICOD2, '(21X, &
     &         "V_(IAddC,IAddK,IAddB,IAddJ) = V(IAddCB,IAddKJ)", &
     &         " * DCMPLX(ISgnCB * ISgnKJ)")')
         ELSE
            WRITE(ICOD2, '(21X, &
     &         "V_(IAddC,IAddK,IAddB,IAddJ) = V(IAddCB,IAddKJ)", &
     &         " * DBLE(ISgnCB * ISgnKJ)")')
         END IF
      ELSE
         IF (Complx) THEN
            WRITE(ICOD2, '(21X, &
     &         "V_(IAddC,IAddK,IAddB,IAddJ,iAI) = V(IAddCB,IAddKJ,iAI)", &
     &         " * DCMPLX(ISgnCB * ISgnKJ)")')
         ELSE
            WRITE(ICOD2, '(21X, &
     &         "V_(IAddC,IAddK,IAddB,IAddJ,iAI) = V(IAddCB,IAddKJ,iAI)", &
     &         " * DBLE(ISgnCB * ISgnKJ)")')
         END IF
      END IF
      !
      ! --- Close loops
      !
      DO iC = nC, 1, -1
         IF (iC == nC) THEN
            WRITE(ICOD2, '(18X, "END DO ", A7, "  ! Free internal particles")') LpName2(iC)
         ELSE
            WRITE(ICOD2, '(18X, "END DO ", A7)') LpName2(iC)
         END IF
      END DO
      DO iK = nK, 1, -1
         IF (iK == nK) THEN
            WRITE(ICOD2, '(15X, "END DO ", A7, "  ! Free internal holes")') LpName1(iK)
         ELSE
            WRITE(ICOD2, '(15X, "END DO ", A7)') LpName1(iK)
         END IF
      END DO
      DO iB = 1, nB
         IF (iB == 1) THEN
            WRITE(ICOD2, '(12X, "END DO   ! Fixed internal particles")')
         ELSE
            WRITE(ICOD2, '(12X, "END DO")')
         END IF
      END DO
      DO iJ = 1, nJ
         IF (iJ == 1) THEN
            WRITE(ICOD2, '(9X, "END DO   ! Fixed internal holes")')
         ELSE
            WRITE(ICOD2, '(9X, "END DO")')
         END IF
      END DO
      IF (mAI /= 0) WRITE(ICOD2, '(6X, "END DO   ! External indices")')
      !
      ! --- End of the SUBROUTINE
      !
      WRITE(ICOD2, '("!", /, 6X, "END SUBROUTINE")')
      !
      CLOSE(ICOD2)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!!      SUBROUTINE MakeSrcContrct(nAo, nIo, nT, lT, mT, nC, nK, RtnName)
      SUBROUTINE MakeSrcContrct(nAo, nIo, nAt, nIt, nB, nJ, nC, nK, RtnName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Generate a SUBROUTINE which contracts an intermedeate V(CKBJAI)
        !     with a T vertex T(BJA'I') 
        !     to generate a new intermediate W(CKA"I")
        !
        !     In:  nAo and nIo ... # of external lines of the given intermediate
        !          nT, lT, mT ... Specify the T-vertex
        !          nC and nK ... # of free internal lines
        !
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx, PreFixSrc
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=255) RtnName
!      INTEGER :: nAo, nIo, nT, lT, mT, nC, nK
      INTEGER :: nAo, nIo, nAt, nIt, nB, nJ, nC, nK
      !
      CHARACTER(LEN=2) a21, a22, a23, a24, A25, A26
      CHARACTER(LEN=2) AName
      CHARACTER(LEN=3) AName_
      CHARACTER(LEN=7) LpName1(100), LpName2(100)
      CHARACTER(LEN=255) BasName, RtnName_lw
      LOGICAL :: Finish, Vector
      INTEGER :: IOUT, ICOD1, ICOD2
!      INTEGER :: nB, nJ, nAt, nIt, iIt, iAt, iB, iJ, iAo, iIo
      INTEGER :: iIt, iAt, iB, iJ, iAo, iIo
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      ! --- Name of the SUBROUTINE
      !
      BasName = TRIM(PreFixSrc)//"Contract"
      !
      CALL Int2Char(nAo, a21, "0")
      CALL Int2Char(nIo, a22, "0")
      CALL Int2Char(nAt, a23, "0")
      CALL Int2Char(nIt, a24, "0")
      CALL Int2Char(nJ, a25, "0")
      CALL Int2Char(nB, a26, "0")
      RtnName = TRIM(BasName)//"_Ao"//a21//"Io"//a22//"At"//a23//"It"//a24//"J"//a25//"B"//a26
      !
      Vector = ((nC /= 0) .OR. (nK /= 0))
      IF (.NOT. Vector) THEN
         RtnName = TRIM(RtnName)//"S"   ! C and K are zero (scalar)
      ELSE
         RtnName = TRIM(RtnName)//"V"   ! C and/or K is non-zero (vector)
      END IF
      !
      RtnName = TRIM(RtnName)//"_AutoGen"
      !
      ! --- Convert to lower CASE for filename
      !
      RtnName_lw = RtnName
      CALL Up2Lw(RtnName_lw)
      !
      OPEN(UNIT=ICOD2, FILE=TRIM(RtnName_lw)//".f90", STATUS='UNKNOWN')
      REWIND(ICOD2)
      !
      ! --- Header
      !
      WRITE(ICOD2, '(6X, "SUBROUTINE ", $)')
      WRITE(ICOD2, *) TRIM(RtnName)//" &"
      WRITE(ICOD2, *) "     &   "// &
     &   "(V, T, W, nAo, nIo, nAt, nIt, nB, nJ, mCK, mB, mJ, mAo, ", &
     &   "mIo, mD, mL, mAn, mIn)"
      WRITE(ICOD2, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!", /, &
     &   "!", 5X, "Contract intermediate V(C,K,B,J,Ao,Io) to T(D,L)", /, &
     &   "!", 5X, "to generate a new intermediate W(C,K,An,In)", /, &
     &   "!" &
     &     )')
      !
      WRITE(ICOD2, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : OneC")')
      ELSE
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : One")')
      END IF
      !
      ! --- Declare local variables
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(/, 6X, "IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "COMPLEX(8):: V(mCK,mB*mJ,mAo,mIo), T(mD,mL)", &
     &      ", W(mCK,mAn,mIn), F(mB,mJ), Alpha", /, &
     &      6X, "INTEGER:: NStringB(nB), NStringJ(nJ)", &
     &      ", NStringD(nB+nAt), NStringL(nJ+nIt)", /, &
     &      6X, "INTEGER:: NStringAt(nAt), NStringIt(nIt), ", &
     &      "NStringAo(nAo), NStringIo(nIo), ", &
     &      "NStringAn(nAo+nAt), NStringIn(nIo+nIt)", /)')
      ELSE
         WRITE(ICOD2, '(6X, "IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "REAL(8):: V(mCK,mB*mJ,mAo,mIo), T(mD,mL)", &
     &      ", W(mCK,mAn,mIn), F(mB,mJ), Alpha", /, &
     &      6X, "INTEGER:: NStringB(nB), NStringJ(nJ)", &
     &      ", NStringD(nB+nAt), NStringL(nJ+nIt)", /, &
     &      6X, "INTEGER:: NStringAt(nAt), NStringIt(nIt), ", &
     &      "NStringAo(nAo), NStringIo(nIo), ", &
     &      "NStringAn(nAo+nAt), NStringIn(nIo+nIt)", /)')
      END IF
      !
      ! --- Loop over It (External holes in T)
      !
      IF (nIt /= 0) THEN
         !   
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nIt, a21, "0")
         WRITE(ICOD2, '(6X, "DO iIt00 = ", I2, ", NOc", $)') nIt
         WRITE(ICOD2, '("   ! loop over external hole(s) in T-vertex")')
         WRITE(ICOD2, '(9X, "NStringIt(", I2, ") = iIt00")') nIt
         DO iIt = (nIt - 1), 1, -1
            CALL Int2Char((nIt-iIt), a21, "0")
            CALL Int2Char((nIt-iIt-1), a22, "0")
            CALL Int2Char(iIt, a23, " ")
            WRITE(ICOD2, '(6X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIt"//a21//" = "//a23//", iIt"//a22//" - 1")
            WRITE(ICOD2, '(9X, "NStringIt(", I2, ") = ", $)') iIt
            WRITE(ICOD2, *) TRIM("iIt"//a21)
         END DO
         !      
      END IF
      !
      ! --- Loop over At (External particles in T)
      !
      IF (nAt /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nAt, a21, "0")
         WRITE(ICOD2, '(9X, "DO iAt00 = ", I2, ", NVr", $)') nAt
         WRITE(ICOD2, '("   ! loop over external particle(s) in T-vertex")')
         WRITE(ICOD2, '(12X, "NStringAt(", I2, ") = iAt00")') nAt
         DO iAt = (nAt - 1), 1, -1
            CALL Int2Char((nAt - iAt), a21, "0")
            CALL Int2Char((nAt - iAt - 1), a22, "0")
            CALL Int2Char(iAt, a23, " ")
            WRITE(ICOD2, '(9X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAt"//a21//" = "//a23//", iAt"//a22//" - 1")
            WRITE(ICOD2, '(12X, "NStringAt(", I2, ") = ", $)') iAt
            WRITE(ICOD2, *) TRIM("iAt"//a21)
         END DO
         !
      END IF
      !
      ! --- Zero-clear work array
      !      
      IF (Complx) THEN
         WRITE(ICOD2, '(/, 12X, "CALL CCLib_ZClear(F, mB*mJ)")')
      ELSE
         WRITE(ICOD2, '(/, 12X, "CALL CCLib_DClear(F, mB*mJ)")')
      END IF
      !
      ! --- Loop over J (Fixed holes)
      !
      IF (nJ /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nJ, a21, "0")
         WRITE(ICOD2, '(12X, "DO iJ00 = ", I2, ", NOc ", $)') nJ
         WRITE(ICOD2, '("   ! loop over fixed hole(s)")')
         WRITE(ICOD2, '(15X, "NStringJ(", I2, ") = iJ00")') nJ
         DO iJ = (nJ - 1), 1, -1
            CALL Int2Char((nJ - iJ), a21, "0")
            CALL Int2Char((nJ - iJ - 1), a22, "0")
            CALL Int2Char(iJ, a23, " ")
            WRITE(ICOD2, '(12X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iJ"//a21//" = "//a23//",iJ"//a22//" - 1")
            WRITE(ICOD2, '(15X, "NStringJ(",I2,") = ", $)') iJ
            WRITE(ICOD2, *) TRIM("iJ"//a21)
         END DO
         !
         IF (nIt /= 0) THEN
            WRITE(ICOD2, '(15X, "COIN = .FALSE.", /, &
     &         15X, "CALL CCLib_MergeStrings(NStringJ, NStringIt, ", &
     &         "NStringL, nJ, nIt, ISgnL, COIN)", /, &
     &         15X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         15X, "CALL CCLib_StringAddress(NStringL, nJ+nIt, ", &
     &         """Occ"", IAddL)", /, &
     &         15X, "CALL CCLib_StringAddress(NStringJ, nJ, ", &
     &         """Occ"", IAddJ)")')
         ELSE
            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringJ, nJ, ", """Occ"", IAddJ)")')
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X,"IAddL = IAddJ")')
         END IF
         !
      ELSE   ! nJ == 0
         !
         WRITE(ICOD2, '(15X, "IAddJ = 1", /)')
         IF (nIt /= 0) THEN
            WRITE(ICOD2, '(15X, &
     &         "CALL CCLib_StringAddress(NStringIt, nIt, ", &
     &         """Occ"", IAddIt)")')
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X, "IAddL = IAddIt")')
         ELSE
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X, "IAddL = 1")')
         END IF
         !
      END IF
      !
      ! --- Loop over B (Fixed particles)
      !
      IF (nB /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nB, a21, "0")
         WRITE(ICOD2, '(15X, "DO iB00 = ", I2, ", NVr", $)') nB
         WRITE(ICOD2, '("   ! loop over fixed particle(s)")')
         WRITE(ICOD2, '(18X, "NStringB(", I2, ") = iB00")') nB
         DO iB = (nB - 1), 1, -1
            CALL Int2Char((nB - iB), a21, "0")
            CALL Int2Char((nB - iB - 1), a22, "0")
            CALL Int2Char(iB, a23, " ")
            WRITE(ICOD2, '(15X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iB"//a21//" = "//a23//",iB"//a22//" - 1")
            WRITE(ICOD2, '(18X, "NStringB(", I2, ") = ", $)') iB
            WRITE(ICOD2, *) TRIM("iB"//a21)
         END DO
         !
         IF (nAt /= 0) THEN
            WRITE(ICOD2, '(18X, "COIN = .FALSE.", /, &
     &         18X, "CALL CCLib_MergeStrings", &
     &         "(NStringB, NStringAt, NStringD, nB, nAt, ISgnD, COIN)", /, &
     &         18X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         18X, "CALL CCLib_StringAddress(NStringD, nB+nAt, ", """Vir"", IAddD)", /, &
     &         18X, "CALL CCLib_StringAddress(NStringB, nB, ", """Vir"", IAddB)")')
         ELSE
            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringB, nB, ", """Vir"", IAddB)")')
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, 18X, "IAddD = IAddB")')
         END IF
         !
      ELSE   ! nB == 0
         !
         WRITE(ICOD2, '(18X, "IAddB = 1", /)')
         IF (nAt /= 0) THEN
            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAt, nAt, ", """Vir"", IAddAt)")')
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, 18X, "IAddD = IAddAt")')
         ELSE
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, &
     &         18X, "IAddD = 1")')
         END IF
         !
      END IF
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(18X, "F(IAddB,IAddJ) = DCMPLX(ISgnL * ISgnD) * T(IAddD,IAddL)")')
      ELSE
         WRITE(ICOD2, '(18X, "F(IAddB,IAddJ) = DBLE(ISgnL * ISgnD) * T(IAddD,IAddL)")')
      END IF
      !
      ! --- Close loops over fixed indices
      !
      IF (nB /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(15X, "END DO   ! Fixed particles")')
         DO iB = 2, nB
            WRITE(ICOD2, '(15X, "END DO")')
         END DO
      END IF
      !
      IF (nJ /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(12X, "END DO   ! Fixed holes")')
         DO iJ = 2, nJ
            WRITE(ICOD2, '(12X, "END DO")')
         END DO
      END IF
      !
      ! --- Loop over internal holes Io of intermediate
      !
      IF (nIo /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nIo, a21, "0")
         WRITE(ICOD2, '(12X, "DO iIo00 = ", I2, ", NOc ", $)') nIo
         WRITE(ICOD2, '("   ! loop over external hole(s) in V")')
         WRITE(ICOD2, '(15X, "NStringIo(", I2, ") = iIo00")') nIo
         DO iIo = (nIo - 1), 1, -1
            CALL Int2Char((nIo - iIo), a21, "0")
            CALL Int2Char((nIo - iIo - 1), a22, "0")
            CALL Int2Char(iIo, a23, " ")
            WRITE(ICOD2, '(12X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIo"//a21//" = "//a23//", iIo"//a22//" - 1")
            WRITE(ICOD2, '(15X, "NStringIo(", I2, ") = ", $)') iIo
            WRITE(ICOD2, *) TRIM("iIo"//a21)
         END DO
         !
         IF (nIt /= 0) THEN
            WRITE(ICOD2, '( &
     &         15X, "COIN = .FALSE.", /, &
     &         15X, "CALL CCLib_MergeStrings(NStringIo, NStringIt, ", &
     &         "NStringIn, nIo, nIt, ISgnIn, COIN)", /, &
     &         15X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '(15X, &
     &         "CALL CCLib_StringAddress(NStringIn, nIo+nIt, ""Occ"", IAddIn)", /, &
     &         15X, "CALL CCLib_StringAddress(NStringIo, nIo, ""Occ"", IAddIo)")')
         ELSE   ! nIt = 0
            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIo, nIo, ""Occ"", IAddIo)")')
            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
     &         15X, "IAddIn = IAddIo")')
         END IF
         !
      ELSE   ! nIo == 0
         !
         WRITE(ICOD2, '(15X, "IAddIo = 1")')
         IF (nIt /= 0) THEN
            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIt, nIt, ""Occ"", IAddIt)")')
            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
     &         15X, "IAddIn = IAddIt")')
         ELSE
            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
     &         15X, "IAddIn = 1")')
         END IF
         !
      END IF
      !
      ! --- Loop over external particles Ao of intermediate
      !
      IF (nAo /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nAo, a21, "0")
         WRITE(ICOD2, '(15X, "DO iAo00 = ",I2,", NVr", $)') nAo
         WRITE(ICOD2, '("   ! loop over external particle(s) in V")')
         WRITE(ICOD2, '(18X, "NStringAo(",I2,") = iAo00")') nAo
         DO iAo = (nAo-1), 1, -1
            CALL Int2Char((nAo-iAo), a21, "0")
            CALL Int2Char((nAo-iAo-1), a22, "0")
            CALL Int2Char(iAo, a23, " ")
            WRITE(ICOD2, '(15X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAo"//a21//" = "//a23//", iAo"//a22//" - 1")
            WRITE(ICOD2, '(18X, "NStringAo(", I2, ") = ", $)') iAo
            WRITE(ICOD2, *) TRIM("iAo"//a21)
         END DO
         !
         IF (nAt /= 0) THEN
            WRITE(ICOD2, '( &
     &         18X, "COIN = .FALSE.", /, &
     &         18X, "CALL CCLib_MergeStrings(NStringAo, NStringAt, ", &
     &         "NStringAn, nAo, nAt, ISgnAn, COIN)", /, &
     &         18X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         18X, "CALL CCLib_StringAddress(NStringAn, nAo+nAt, ", &
     &         """Vir"", IAddAn)", /, &
     &         18X, "CALL CCLib_StringAddress(NStringAo, nAo, ", &
     &         """Vir"", IAddAo)")')
         ELSE
            WRITE(ICOD2, '( &
     &         18X, "CALL CCLib_StringAddress(NStringAo, nAo, ", &
     &         """Vir"", IAddAo)")')
            WRITE(ICOD2, '(18X,"ISgnAn = 1",/, &
     &         18X,"IAddAn = IAddAo")')
         END IF
         !
      ELSE   ! nAo = 0
         !
         WRITE(ICOD2, '(18X, "IAddAo = 1")')
         IF (nAt /= 0) THEN
            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAt, nAt, ""Vir"", IAddAt)")')
            WRITE(ICOD2, '(18X, "ISgnAn = 1", /, &
     &         18X, "IAddAn = IAddAt")')
         ELSE
            WRITE(ICOD2, '(18X, "ISgnAn = 1", /, &
     &         18X, "IAddAn = 1")')
         END IF
         !
      END IF
      !
      ! --- Contract V and T
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(18X, "Alpha = DCMPLX(ISgnAn * ISgnIn)")')
         WRITE(ICOD2, '(18X, "CALL ZGEMV(""N"", mCK, mB*mJ, ", &
     &      "Alpha, V(1,1,IAddAo,IAddIo), mCK, F, ", &
     &      "1, OneC, W(1,IAddAn,IAddIn), 1)")')
      ELSE
         WRITE(ICOD2, '(18X, "Alpha = DBLE(ISgnAn * ISgnIn)")')
         WRITE(ICOD2, '(18X, "CALL DGEMV(""N"", mCK, mB*mJ, ", &
     &      "Alpha, V(1,1,IAddAo,IAddIo), mCK, F, ", &
     &      "1, One, W(1,IAddAn,IAddIn), 1)")')
      END IF
      !
      ! --- Close loops
      !
      IF (nAo /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(15X, "END DO   ! V ext. particles")')
         DO iAo = 2, nAo
            WRITE(ICOD2, '(15X, "END DO")')
         END DO
      END IF
      !
      IF (nIo /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(12X, "END DO   ! V ext. holes")')
         DO iIo = 2, nIo
            WRITE(ICOD2, '(12X, "END DO")')
         END DO
      END IF
      !
      IF (nIt /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(9X, "END DO   ! T ext. holes")')
         DO iIt = 2, nIt
            WRITE(ICOD2, '(9X, "END DO")')
         END DO
      END IF
      !
      IF (nAt /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(6X, "END DO   ! T ext. particles")')
         DO iAt = 2, nAt
            WRITE(ICOD2, '(6X, "END DO")')
         END DO
      END IF
      !
      ! --- End of the SUBROUTINE
      !
      WRITE(ICOD2, '("!", /, 6X, "END SUBROUTINE")')
      !
      CLOSE(ICOD2)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, M1, M2, M3, M13, NTlev, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Define indices for contraction V*T = W
        !  nB, nJ ... Summation indices
        !  nC, nK ... Free internal indices (This is equal to the #'s of internal lines of W)
        !  nD, nL ... Total #'s of internal lines of V
        !  nA, nI ... #'s of external lines of V
        !
        ! * Reduce nA or nI if R operator is electron detachment or attachment
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole, &
     &   RightIP, RightEA, LeftIP, LeftEA
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: M1(4), M2(4), M3(4), M13, NRank, NTlev, ICOD
      INTEGER, INTENT(OUT) :: nB, nJ, nC, nK, nD, nL, nA, nI
      !
      INTEGER:: M1_(4), M2_(4), M3_(4), IT, I
      !
      DO IT = 1, 4
         M1_(IT) = IABS(M1(IT))
         M2_(IT) = IABS(M2(IT))
         M3_(IT) = IABS(M3(IT))
      END DO
      !
      ! Summation indices
      !
      nB = M3_(NTlev)
      nJ = M2_(NTlev) - M3_(NTlev)
      !
      ! No-summation internal indices
      !
      nC = NIntPart(M13)
      nK = NIntHole(M13)
      DO I = 4, NTlev, -1
         nC = nC - M3_(I)
         nK = nK - (M2_(I) - M3_(I))
      END DO
      !
      ! Total number of internal indices
      !
      nD = nB + nC
      nL = nJ + nK
      !
      ! All external indices of the intermediate
      !
      nA = NExtPart(M13)
      nI = NExtHole(M13)
      DO I = 4, (NTlev + 1), -1
         nA = nA + (M1_(I) - M3_(I))
         nI = nI + (M1_(I) - (M2_(I) - M3_(I)))
         IF (RightIP(I)) nA = nA - 1
         IF (RightEA(I)) nI = nI - 1
      END DO
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, "nA, nI ... # of ext. lines of intermediate", /, &
     &   "!", 5X, "nB, nJ ... Summation indices", /, &
     &   "!", 5X, "nC, nK ... Free int. indices", /, &
     &   "!", 5X, "nD, nL ... nB+nC and nJ+nK", /, &
     &   "!")')
      !
      WRITE(ICOD, '(6X, &
     &   "nA = ", I3, /, 6X, &
     &   "nI = ", I3, /, 6X, &
     &   "nB = ", I3, /, 6X, &
     &   "nJ = ", I3, /, 6X, &
     &   "nC = ", I3, /, 6X, &
     &   "nK = ", I3, /, 6X, &
     &   "nD = ", I3, /, 6X, &
     &   "nL = ", I3, /, 6X, &
     &   "mAI = CCLib_NComb(NOc, nI) * CCLib_NComb(NVr, nA)", /, 6X, &
     &   "mB = CCLib_NComb(NVr, nB)", /, 6X, &
     &   "mJ = CCLib_NComb(NOc, nJ)", /, 6X, &
     &   "mD = CCLib_NComb(NVr, nD)", /, 6X, &
     &   "mL = CCLib_NComb(NOc, nL)", /, 6X, &
     &   "mC = CCLib_NComb(NVr, nC)", /, 6X, &
     &   "mK = CCLib_NComb(NOc, nK)")') &
     &   nA, nI, nB, nJ, nC, nK, nD, nL
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefGsize(M13, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define size of MO integral array
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      INTEGER :: M13, ICOD
      !
      SELECT CASE (M13)
         CASE (1)
            WRITE(ICOD, '(6X,"NSizeG = NVr * NVr")')
         CASE (2)
            WRITE(ICOD, '(6X,"NSizeG = NOc * NOc")')
         CASE (3,12)
            WRITE(ICOD, '(6X,"NSizeG = NVr * NOc")')
         CASE (4)
            WRITE(ICOD, '(6X, "NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NVr, 2)")')
         CASE (5)
            WRITE(ICOD, '(6X, "NSizeG = CCLib_NComb(NOc, 2) * CCLib_NComb(NOc, 2)")')
         CASE (6)
            WRITE(ICOD, '(6X,"NSizeG = NVr * NOc * NVr * NOc")')
         CASE (7)
            WRITE(ICOD, '(6X, "NSizeG = CCLib_NComb(NVr, 2) * NOc * NVr")')
         CASE (8)
            WRITE(ICOD, '(6X, "NSizeG = CCLib_NComb(NOc, 2) * NVr * NOc")')
         CASE (9)
            WRITE(ICOD, '(6X, "NSizeG = NVr * CCLib_NComb(NVr, 2) * NOc")')
         CASE (10)
            WRITE(ICOD, '(6X, "NSizeG = NOc * NVr * CCLib_NComb(NOc, 2)")')
         CASE (11, 13)
            WRITE(ICOD, '(6X, "NSizeG = CCLib_NComb(NVr, 2) * CCLib_NComb(NOc, 2)")')
         CASE DEFAULT
            WRITE(*, '("Illegal type of interaction vertex in -DefGsize-", I3)') M13
            WRITE(ICOD, '("Illegal type of interaction vertex in -DefGsize-", I3)') M13
!!!            STOP
      END SELECT
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefG_Size(M13, M1, M2, M3, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define size of G_ array
        !     M13 ... Type of MO integral
        !     M1, M2, M3 ... Specify the T vertex 
        !                    which will be contracted to MO integral
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      INTEGER :: M13, M1, M2, M3, ICOD
      !
      INTEGER :: nA, nI, nD, nL, nB, nJ, nC, nK
      !
      WRITE(ICOD, '("!", 5("-"), "Define G_ size")')
      !
      nA = NExtPart(M13)
      nI = NExtHole(M13)
      nD = NIntPart(M13)
      nL = NIntHole(M13)
      !
      nB = M3
      nJ = M2 - M3
      nC = nD - nB
      nK = nL - nJ
      WRITE(ICOD, '(6X, "nA = ", I3, /, &
     &   6X, "nI = ", I3, /, &
     &   6X, "nB = ", I3, /, &
     &   6X, "nJ = ", I3, /, &
     &   6X, "nC = ", I3, /, &
     &   6X, "nK = ", I3)') nA, nI, nB, nJ, nC, nK
      WRITE(ICOD, '( &
     &   6X, "mAI = CCLib_NComb(NVr, nA) * CCLib_NComb(NOc, nI)", /, &
     &   6X, "mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)", /, &
     &   6X, "mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)")')
      WRITE(ICOD, '(6X, "NSizeG_ = mAI * mBJ * mCK")')
!      WRITE(ICOD, '(6X, "ALLOCATE(G(NSizeG))")')
!
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefL_Size(L1, L2, L3, M1, M2, M3, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define size of L_ array
        !     L1, L2, L3 ... Specify L vertex
        !     M1, M2, M3 ... Specify T vertex 
        !                    which will be contracted to L vertex
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: L1, L2, L3, M1, M2, M3, ICOD
      !
      INTEGER :: nA, nI, nAt, nIt
      !
      WRITE(ICOD, '("!", 5("-"), "Define G_ size (de-excitation vertex)")')
      !
      nAt = M1 - M3
      nIt = M1 - (M2 - M3)
      nA  = L1 - nAt
      nI  = L1 - nIt
      !
      WRITE(ICOD, '(6X, &
     &   "nA  = ", I3, /, &
     &   6X, "nI  = ", I3, /, &
     &   6X, "nAt = ", I3, /, &
     &   6X, "nIt = ", I3)') &
     &   nA, nI, nAt, nIt
      WRITE(ICOD, '( &
     &   6X, "mA  = CCLib_NComb(NVr, nA)", /, &
     &   6X, "mI  = CCLib_NComb(NOc, nI)", /, &
     &   6X, "mAt = CCLib_NComb(NVr, nAt)", /, &
     &   6X, "mIt = CCLib_NComb(NOc, nIt)")')
      WRITE(ICOD, '(6X, "NSizeG_ = mAt * mIt * mA * mI")')
!
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GetVarray(ArrayName, SizeName, File, Status, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define intermediate array "ArrayName"
        !
        !     ArrayName ... Name of the array
        !     SizeName  ... Name of the arary size
        !     File ... File name
        !     Status ... INIT : zero clear
        !                LOAD : read from file
        !                NONE : DO nothing (allocation only)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      CHARACTER(*) :: File, SizeName, ArrayName
      CHARACTER(4) :: Status
      INTEGER :: ICOD
      !
      WRITE(ICOD, '(6X, "ALLOCATE(", $)')
      WRITE(ICOD, *) TRIM(ArrayName)//"("//TRIM(SizeName)//"))"
      !
      IF (.NOT. Complx) THEN
         !
         IF (Status == "INIT") THEN
            WRITE(ICOD, '(6X, "CALL CCLib_DClear(", $)')
            WRITE(ICOD, *) TRIM(ArrayName)//", "//TRIM(SizeName)//")"
         ELSE IF (Status == "LOAD") THEN
            WRITE(ICOD, '(6X, "CALL CC_ReadV(", $)')
            WRITE(ICOD, *) TRIM(SizeName)//", "//TRIM(ArrayName)//", "//"'"//TRIM(File)//"'"//")"
         ELSE IF (Status == "NONE") THEN
            CONTINUE
         ELSE
            WRITE(*, *) "Illegal status in -GetVarray- !!"
            STOP
         END IF
         !
      ELSE   ! Complx
         !
         IF (Status == "INIT") THEN
            WRITE(ICOD, '(6X, "CALL CCLib_ZClear(", $)')
            WRITE(ICOD, *) TRIM(ArrayName)//", "//TRIM(SizeName)//")"
         ELSE IF (Status == "LOAD") THEN
            WRITE(ICOD, '(6X, "CALL SOCC_ReadV(", $)')
            WRITE(ICOD, *) TRIM(SizeName)//", "//TRIM(ArrayName)//", "//"'"//TRIM(File)//"'"//")"
         ELSE IF (Status == "NONE") THEN
            CONTINUE
         ELSE
            WRITE(*, *) "Illegal status in -GetVarray- !!"
            STOP
         END IF
         !
      END IF   ! Complx
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE FreeVarray(ArrayName, SizeName, File, Status, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Free intermediate array "ArrayName"
        !
        !     ArrayName ... Name of the array
        !     SizeName  ... Name of the arary size
        !     File ... File name
        !     Status ... SAVE : write to file
        !                NONE : DO nothing
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      CHARACTER(*) :: File, SizeName, ArrayName
      CHARACTER(4) :: Status
      INTEGER :: ICOD
      !
      IF (Status == "SAVE") THEN
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "CALL SOCC_WriteV(", $)')
         ELSE
            WRITE(ICOD, '(6X, "CALL CC_WriteV(", $)')
         END IF
         WRITE(ICOD, *) TRIM(SizeName)//", "//TRIM(ArrayName)//", "//"'"//TRIM(File)//"'"//")"
      ELSE IF (Status == "NONE") THEN
         CONTINUE
      ELSE
         WRITE(*, *) "Illegal status in -FreeVarray- !!"
         STOP
      END IF
      !
      WRITE(ICOD, '(6X, "DEALLOCATE(", $)')
      WRITE(ICOD, *) TRIM(ArrayName)//")"
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GetGarray(Mtyp, ICOD, IOUT, NumDig)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define MO integral array G
        !     Load MO integral and multiply weight and phase factors
        !
        !     Mtyp   ... 2-digit integer specifying the type of interaction vertex
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      INTEGER :: ICOD, IOUT, Mtyp, NumDig
      !
      CHARACTER(2) :: c2
      !
      WRITE(IOUT, '(5X, "Loading MO integral", I3, " for NumDig =", I4)') MTyp, NumDig
      WRITE(ICOD, '("!"/, 6X, "NumDig = NumDig + 1  ! NumDig =", I4, /, "!")') NumDig
      !
      WRITE(ICOD, '(6X, "ALLOCATE(G(NSizeG))")')
      CALL Int2Char(Mtyp, c2, " ")
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "CALL SOCC_ReadMOint(G, NSizeG, ",$)')
      ELSE
         WRITE(ICOD, '(6X, "CALL CC_ReadMOint(G, NSizeG, ",$)')
      END IF
      WRITE(ICOD, *) c2//")"
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "NPwr = IABS(Mwp(NumDig)) - 1", /, &
     &      6X, "WgtFac = DCMPLX(Half ** NPwr)", /, &
     &      6X, "IF (Mwp(NumDig) < 0) WgtFac = -WgtFac")')
         WRITE(ICOD, '(6X, "CALL ZSCAL(NSizeG, WgtFac, G, 1)")')
      ELSE
         WRITE(ICOD, '(6X,"NPwr = IABS(Mwp(NumDig)) - 1", /, &
     &      6X, "WgtFac = Half ** NPwr", /, &
     &      6X, "IF (Mwp(NumDig) < 0) WgtFac = -WgtFac")')
         WRITE(ICOD, '(6X, "CALL DSCAL(NSizeG, WgtFac, G, 1)")')
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE FreeArray(ArrayName, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Free MO integral array "ArrayName"
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      CHARACTER(*) :: ArrayName
      INTEGER :: ICOD
      !
      WRITE(ICOD, '(6X, "DEALLOCATE(",$)')
      WRITE(ICOD, *) TRIM(ArrayName)//")"
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE AddGtoV(Vname, Size, Mtyp, Mwp, IOUT, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCc
        !     Add interaction vertex G to intermediate "Vname"
        !     Weight and phase factors are considered as well
        !     Vname ... Name of intermediate
        !     Size  ... Name of array size
        !     Mtyp  ... Type of interaction vertex
        !
        !     (28 Jan 2012) Weight and phase factors are multiplied in 
        !                   GetGarray
        !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      CHARACTER(*) :: Vname, Size
      CHARACTER(255) :: CWrk
      INTEGER :: Mtyp, Mwp, ICOD,IOUT
      !
      SELECT CASE (Vname)
         CASE ("V1")
            WRITE(IOUT, '(2X,"V1 += I5(",I12,") with Mwp =",I2)') Mtyp, Mwp
            WRITE(ICOD, '("!",5("-"),X,"V1 += G(",I12,") with Mwp =", I2)') Mtyp, Mwp
         CASE ("V2")
            WRITE(IOUT, '(2X,"V2 += I5(",I12,") with Mwp =",I2)') Mtyp, Mwp
            WRITE(ICOD, '("!",5("-"),X,"V2 += G(",I12,") with Mwp =", I2)') Mtyp, Mwp
         CASE ("V3")
            WRITE(IOUT, '(2X,"V3 += I5(",I12,") with Mwp =",I2)') Mtyp, Mwp
            WRITE(ICOD, '("!",5("-"),X,"V3 += G(",I12,") with Mwp =", I2)') Mtyp, Mwp
         CASE ("V4")
            WRITE(IOUT, '(2X,"V4 += I5(",I12,") with Mwp =",I2)') Mtyp, Mwp
            WRITE(ICOD, '("!",5("-"),X,"V4 += G(",I12,") with Mwp =", I2)') Mtyp, Mwp
      END SELECT
      !
      IF (Complx) THEN
!28Jan2012         WRITE(ICOD, '(6X, "NPwr = IABS(Mwp(NumDig)) - 1",/,
!28Jan2012     *      6X, "WgtFac = DCMPLX(Half ** NPwr)",/,
!28Jan2012     *      6X, "IF (Mwp(NumDig) < 0) WgtFac = -WgtFac")')
         WRITE(ICOD, '(6X, "CALL ZAXPY(", $)')
         CWrk = TRIM(Size)//", OneC, G, 1, "//TRIM(Vname)//", 1)"
      ELSE
!28Jan2012         WRITE(ICOD, '(6X, "NPwr = IABS(Mwp(NumDig)) - 1",/,
!28Jan2012     *      6X, "WgtFac = Half ** NPwr",/,
!28Jan2012     *      6X, "IF (Mwp(NumDig) < 0) WgtFac = -WgtFac")')
         WRITE(ICOD, '(6X, "CALL DAXPY(", $)')
         CWrk = TRIM(Size)//", One, G, 1, "//TRIM(Vname)//", 1)"
      END IF
!28Jan2012      CWrk = TRIM(Size)//", WgtFac, G, 1, "//TRIM(Vname)//", 1)"
      WRITE(ICOD, *) TRIM(CWrk)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GetAmp(LevelT, LevelTOcc, LevelTVir, ICOD, Name)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Define and load a set of amplitudes
        !
        ! LevelT     ... Excitation level (for array and file name)
        ! LevelTOcc  ... # of holes (actual value)
        ! LevelTVir  ... # of particles (actual value)
        ! Name       ... Name of array (same name is used for file)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      INTEGER :: ICOD, LevelT, LevelTOcc, LevelTVir
      CHARACTER(*) :: Name
      !
      WRITE(ICOD, '(6X, "LevelT = ", I3)') LevelT
      WRITE(ICOD, '(6X, "LevelTOcc = ", I3)') LevelTOcc
      WRITE(ICOD, '(6X, "LevelTVir = ", I3)') LevelTVir
      WRITE(ICOD, '(6X, "NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)")')
      WRITE(ICOD, '(6X, "ALLOCATE(", $)')
      WRITE(ICOD, *) TRIM(Name)//"(NSizeT))"
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "CALL SOCC_ReadAmp(NSizeT, ", $)')
         WRITE(ICOD, *) TRIM(Name)//", '"//TRIM(Name)//"'"//", LevelT)"
      ELSE
         WRITE(ICOD, '(6X, "CALL CC_ReadAmp(NSizeT, ", $)')
         WRITE(ICOD, *) TRIM(Name)//", '"//TRIM(Name)//"'"//", LevelT)"
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GetAmpFile(LevelT, LevelTOcc, LevelTVir, ICOD, ArrayName, FileName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Define and load a set of amplitudes
        !
        !  LevelT     ... Excitation level (for file name)
        !  LevelTOcc  ... # of holes (actual value)
        !  LevelTVir  ... # of particles (actual value)
        ! * In this version, the file name is taken from the argument
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx
      !
      IMPLICIT NONE
      !
      INTEGER :: ICOD, LevelT, LevelTOcc, LevelTVir
      CHARACTER(*) :: ArrayName, FileName
      !
      WRITE(ICOD, '(6X, "LevelT = ", I3)') LevelT
      WRITE(ICOD, '(6X, "LevelTOcc = ", I3)') LevelTOcc
      WRITE(ICOD, '(6X, "LevelTVir = ", I3)') LevelTVir
      WRITE(ICOD, '(6X, "NSizeT = CCLib_NComb(NOc, LevelTOcc) * CCLib_NComb(NVr, LevelTVir)")')
      WRITE(ICOD, '(6X, "ALLOCATE(", $)')
      WRITE(ICOD, *) TRIM(ArrayName)//"(NSizeT))"
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "CALL SOCC_ReadAmp(NSizeT, ", $)')
         WRITE(ICOD, *) TRIM(ArrayName)//", "//"TRIM("//TRIM(FileName)//")"//", LevelT)"
      ELSE
         WRITE(ICOD, '(6X, "CALL CC_ReadAmp(NSizeT, ",$)')
         WRITE(ICOD, *) TRIM(ArrayName)//", "//"TRIM("//TRIM(FileName)//")"//", LevelT)"
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Contract(Vname, Tname, Wname, NRank, NT, M1, M2, M3, M13, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Contract "Vname" to T --> Generate "Wname"
        !
        !     NT ... Level of T vertex (1 to 4)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole, &
     &   RightIP, RightEA, LeftIP, LeftEA
      !
      IMPLICIT NONE
      !
      CHARACTER(*) :: Vname, Wname, Tname
      INTEGER :: ICOD, M1(4), M2(4), M3(4), M13, NT, NRank
      !
      CHARACTER(LEN=255) :: CWrk
      INTEGER :: M1_(4), M2_(4), M3_(4), IT
      INTEGER :: nAt, nIt, nB, nJ, nC, nK, nD, nL, nAo, nIo, nAn, nIn
      !
      DO IT = 1, 4
         M1_(IT) = IABS(M1(IT))
         M2_(IT) = IABS(M2(IT))
         M3_(IT) = IABS(M3(IT))
      END DO
      !
      ! External indices of T vertex
      !
      nAt = M1_(NT) - M3_(NT)
      nIt = M1_(NT) - (M2_(NT) - M3_(NT))
      IF (RightIP(NT)) nAt = nAt - 1
      IF (RightEA(NT)) nIt = nIt - 1
      !
      ! Summation indices
      !
      nB = M3_(NT)
      nJ = M2_(NT) - nB
      !
      ! Full indices of T vertex
      !
      nD = nAt + nB
      nL = nIt + nJ
!L1      nC = 0
!L1      nK = 0
!L1      DO IT = 1, NT-1
!L1         nC = nC + M3_(IT)
!L1         nK = nK + M2_(IT) - M3_(IT)
!L1      END DO
      !
      ! No-summation internal indices
      !
      nC = NIntPart(M13)
      nK = NIntHole(M13)
      DO IT = 4, NT, -1
         nC = nC - M3_(IT)
         nK = nK - (M2_(IT) - M3_(IT))
      END DO
!L1      nAo = NRank
!L1      nIo = NRank
!L1      DO IT = 1, NT
!L1         nAo = nAo - (M1_(IT) - M3_(IT))
!L1         nIo = nIo - (M1_(IT) - (M2_(IT) - M3_(IT)))
!L1      END DO
      !
      ! External indices of old intermediate
      !
      nAo = NExtPart(M13)
      nIo = NExtHole(M13)
      DO IT = 4, (NT + 1), -1
         nAo = nAo + (M1_(IT) - M3_(IT))
         nIo = nIo + (M1_(IT) - (M2_(IT) - M3_(IT)))
         IF (RightIP(IT)) nAo = nAo - 1
         IF (RightEA(IT)) nIo = nIo - 1
      END DO
      !
      ! External indices of new intermediate
      !
      nAn = nAt + nAo
      nIn = nIt + nIo
      !
      WRITE(ICOD, '( &
     &   6X, "nAo = ", I2, /, &
     &   6X, "nIo = ", I2, /, &
     &   6X, "nAt = ", I2, /, &
     &   6X, "nIt = ", I2, /, &
     &   6X, "nB  = ", I2, /, &
     &   6X, "nJ  = ", I2, /, &
     &   6X, "nC  = ", I2, /, &
     &   6X, "nK  = ", I2, /, &
     &   6X, "nD  = ", I2, /, &
     &   6X, "nL  = ", I2, /, &
     &   6X, "nAn = ", I2, /, &
     &   6X, "nIn = ", I2, /, &
     &   6X, "mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)", /, &
     &   6X, "mB  = CCLib_NComb(NVr, nB)", /, &
     &   6X, "mJ  = CCLib_NComb(NOc, nJ)", /, &
     &   6X, "mD  = CCLib_NComb(NVr, nD)", /, &
     &   6X, "mL  = CCLib_NComb(NOc, nL)", /, &
     &   6X, "mAo = CCLib_NComb(NVr, nAo)", /, &
     &   6X, "mIo = CCLib_NComb(NOc, nIo)", /, &
     &   6X, "mAn = CCLib_NComb(NVr, nAn)", /, &
     &   6X, "mIn = CCLib_NComb(NOc, nIn)")') &
     &   nAo, nIo, nAt, nIt, nB, nJ, nC, nK, nD, nL, nAn, nIn
      !
      ! --- Make SUBROUTINE for contraction
      !
!      CALL MakeSrcContrct(nAo, nIo, M1_(NT), M2_(NT), M3_(NT), nC, nK, CWrk)
      CALL MakeSrcContrct(nAo, nIo, nAt, nIt, nB, nJ, nC, nK, CWrk)
      !
      ! --- CALL contraction SUBROUTINE
      !
      WRITE(ICOD, '(6X, "CALL", $)')
!      WRITE(ICOD, *) TRIM(CWrk)//"("//TRIM(Vname)//",T,"
!     *   //TRIM(Wname)//",nAo,nIo,nAt,",
!     *   "nIt,nB,nJ,mCK,mB,mJ,mAo,mIo,mD,mL,mAn,mIn)"
      WRITE(ICOD, *) TRIM(CWrk)//"( &"
      WRITE(ICOD, *) "     &   " &
     &   //TRIM(Vname)//", "//TRIM(Tname)//", "//TRIM(Wname)//", nAo, nIo, nAt, ", &
     &   "nIt, nB, nJ, mCK, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)"
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ContractBLAS3(Vname, Tname, Wname, NRank, NT, M1, M2, M3, M13, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Contract "Vname" to T --> Generate "Wname"
        !
        !     NT ... Level of T vertex (1 to 4)
        !     This version uses BLAS3 (DGEMM or ZGEMM)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole, &
     &   RightIP, RightEA, LeftIP, LeftEA
      !
      IMPLICIT NONE
      !
      CHARACTER(*) :: Vname, Wname, Tname
      INTEGER :: ICOD, M1(4), M2(4), M3(4), M13, NT, NRank
      !
      CHARACTER(LEN=255) :: CWrk
      INTEGER :: M1_(4), M2_(4), M3_(4), IT
      INTEGER :: nAt, nIt, nB, nJ, nC, nK, nAo, nIo, nAn, nIn
      !
      DO IT = 1, 4
         M1_(IT) = IABS(M1(IT))
         M2_(IT) = IABS(M2(IT))
         M3_(IT) = IABS(M3(IT))
      END DO
      !
      ! External indices of T vertex
      !
      nAt = M1_(NT) - M3_(NT)
      nIt = M1_(NT) - (M2_(NT) - M3_(NT))
      IF (RightIP(NT)) nAt = nAt - 1
      IF (RightEA(NT)) nIt = nIt - 1
      !
      ! Summation indices
      !
      nB = M3_(NT)
      nJ = M2_(NT) - nB
      !
      ! No-summation internal indices
      !
      nC = NIntPart(M13)
      nK = NIntHole(M13)
      DO IT = 4, NT, -1
         nC = nC - M3_(IT)
         nK = nK - (M2_(IT) - M3_(IT))
      END DO
      !
      ! External indices of old intermediate
      !
      nAo = NExtPart(M13)
      nIo = NExtHole(M13)
      DO IT = 4, (NT + 1), -1
         nAo = nAo + (M1_(IT) - M3_(IT))
         nIo = nIo + (M1_(IT) - (M2_(IT) - M3_(IT)))
         IF (RightIP(IT)) nAo = nAo - 1
         IF (RightEA(IT)) nIo = nIo - 1
      END DO
      !
      ! External indices of new intermediate
      !
      nAn = nAt + nAo
      nIn = nIt + nIo
      !
      WRITE(ICOD, '( &
     &   6X, "nAo = ", I2, /, &
     &   6X, "nIo = ", I2, /, &
     &   6X, "nAt = ", I2, /, &
     &   6X, "nIt = ", I2, /, &
     &   6X, "nB  = ", I2, /, &
     &   6X, "nJ  = ", I2, /, &
     &   6X, "nC  = ", I2, /, &
     &   6X, "nK  = ", I2, /, &
     &   6X, "nAn = ", I2, /, &
     &   6X, "nIn = ", I2, /, &
     &   6X, "mCK = CCLib_NComb(NVr, nC) * CCLib_NComb(NOc, nK)", /, &
     &   6X, "mBJ = CCLib_NComb(NVr, nB) * CCLib_NComb(NOc, nJ)", /, &
     &   6X, "mAo = CCLib_NComb(NVr, nAo)", /, &
     &   6X, "mIo = CCLib_NComb(NOc, nIo)", /, &
     &   6X, "mAt = CCLib_NComb(NVr, nAt)", /, &
     &   6X, "mIt = CCLib_NComb(NOc, nIt)", /, &
     &   6X, "mAn = CCLib_NComb(NVr, nAn)", /, &
     &   6X, "mIn = CCLib_NComb(NOc, nIn)")') &
     &   nAo, nIo, nAt, nIt, nB, nJ, nC, nK, nAn, nIn
      !
      ! --- Make SUBROUTINE for contraction
      !
      CALL MakeSrcContrctBLAS3(nAo, nIo, nAt, nIt, nB, nJ, nC, nK, CWrk)
      !
      ! --- CALL contraction SUBROUTINE
      !
      WRITE(ICOD, '(6X, "CALL", $)')
      WRITE(ICOD, *) TRIM(CWrk)//"( &"
      WRITE(ICOD, *) "     &   " &
     &   //TRIM(Vname)//", "//TRIM(Tname)//", "//TRIM(Wname)//", nAo, nIo, nAt, ", &
     &   "nIt, mCK, mBJ, mAo, mIo, mAt, mIt, mAn, mIn)"
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CountHole(OpTyp, Nop, Nhole)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define pathes
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      CHARACTER(3) :: OpTyp(*)
      INTEGER :: Nop, Nhole, NCnt,Iop
      !
      Nhole = 0
      !
      ! --- # of external holes
      !      
      DO Iop = 1, Nop
         IF (OpTyp(Iop) == "ehn") Nhole = Nhole + 1
      END DO
      !
      ! --- # of internal holes
      !
      NCnt = 0
      DO Iop = 1, Nop
         IF (OpTyp(Iop) == "ihd" .OR. OpTyp(Iop) == "ihn")  THEN
            NCnt = NCnt + 1
         END IF
      END DO
      !
      IF (Mod(NCnt,2) /= 0) THEN
         WRITE(*, '(" Inconsistency in -CountHole-. STOP")')
         STOP
      END IF
      !
      Nhole = Nhole + NCnt/2
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefPath(Nop, ID_V, M1, Ipartner)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Define pathes
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      INTEGER :: Nop, ID_V, Iop, IAdd, N, IT
      INTEGER :: Ipartner(*),M1(4)
      !
      ! --- Interaction vertex
      !
      SELECT CASE (ID_V)
         CASE (1,2,3,12)
            Ipartner(1) = 2
            Ipartner(2) = 1
            IAdd = 2
         CASE (4,5,6,7,8,9,10,11,13)
            Ipartner(1) = 2
            Ipartner(2) = 1
            Ipartner(3) = 4
            Ipartner(4) = 3
            IAdd = 4
      END SELECT
      !
      ! --- T vertices
      !
!      IT = 1
! 100  Continue
      DO IT = 4, 1, -1
         IF (M1(IT) == 0) CYCLE
         DO Iop = 1, 2*M1(IT)-1, 2
            Ipartner(IAdd + Iop    ) = IAdd + Iop + 1
            Ipartner(IAdd + Iop + 1) = IAdd + Iop
         END DO
         IAdd = IAdd + 2*M1(IT)
!         IF (IT == 4) RETURN
!         IT = IT + 1
      END DO
!      GO TO 100
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE IntCont(OpTyp, Nop, NumOpsH, ID_V, Ipartner, nB, nJ, nBV, NJV, IOUT)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Internal contraction
        !     Out: Ipartner(1:Nops)
        !          OpTyp(1:Nops)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, Only: NMax
      USE Vertex_Module, ONLY : NIntPart, NIntHole
      !
      IMPLICIT NONE
      !
      CHARACTER(3) :: OpTyp(*),CDum
      INTEGER :: Nop, NumOpsH, ID_V, IAdd, NumInP, NumInH,IOUT, IopT, IopH
      INTEGER :: Ipartner(*)
      INTEGER :: nB(4),nJ(4),nBV(2,4),nJV(2,4)
      !
      NumInP = NIntPart(ID_V)
      NumInH = NIntHole(ID_V)
      !
!      DO IopH = 1, NumOpsH
      DO IopH = NumOpsH, 1, -1
         IF (OpTyp(IopH) == "ipn") THEN   ! Internal particle
            NumInP = NumInp - 1
            DO IopT = NumOpsH+1, Nop
               IF (OpTyp(IopT) == "ipd") THEN
                  Ipartner(IopH) = IopT
                  Ipartner(IopT) = IopH
                  OpTyp(IopH) = "---"
                  OpTyp(IopT) = "---"
!!!
                 WRITE(IOUT, '("Internal Cont.: ", I3, "--", I3)') IopH, IopT
!!!
                  EXIT
               END IF
            END DO   ! IopT
         END IF
         IF (NumInP < 0) THEN
            WRITE(*, '("Inconsistency 1 in -IntCont-")')
            STOP
         END IF
         !
         IF (OpTyp(IopH) == "ihd") THEN   ! Internal holes
            NumInH = NumInH - 1
            DO IopT = NumOpsH+1, Nop
               IF (OpTyp(IopT) == "ihn") THEN
                  Ipartner(IopH) = IopT
                  Ipartner(IopT) = IopH
                  OpTyp(IopH) = "---"
                  OpTyp(IopT) = "---"
!!!
                 WRITE(IOUT, '("Internal Cont.: ", I3, "--", I3)') IopH, IopT
!!!
                  EXIT
               END IF
            END DO   ! IopT
         END IF
         IF (NumInH < 0) THEN
            WRITE(*, '("Inconsistency 2 in -IntCont-")')
            STOP
         END IF
         !
      END DO   ! IopH
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ExtCont(OpTyp, Nop, IpartnerC, IpartnerP, IOUT)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     External Contraction
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      CHARACTER(3) :: OpTyp(*)
      INTEGER :: Nop, IpartnerC(*), IpartnerP(*), IOUT, Iop, Jop, Icurrnt
      !
      DO Iop = 1, Nop
         IF (OpTyp(Iop) == "epd") THEN
            DO Jop = 1, Nop
               IF (OpTyp(Jop) /= "ehn") CYCLE
               IpartnerC(Iop) = Jop
               IpartnerC(Jop) = Iop
               OpTyp(Iop) = "---"
               OpTyp(Jop) = "---"
!!!
              WRITE(IOUT, '("External connection: ", I3, "--", I3)') Iop, Jop
!!!
               EXIT
            END DO
         ELSE IF (OpTyp(Iop) == "ehn") THEN
            DO Jop = 1, Nop
               IF (OpTyp(Jop) /= "epd") CYCLE
               IpartnerC(Iop) = Jop
               IpartnerC(Jop) = Iop
               OpTyp(Iop) = "---"
               OpTyp(Jop) = "---"
!!!
              WRITE(IOUT, '("External connection: ", I3, "--", I3)') Iop, Jop
!!!
               EXIT
            END DO
         END IF
      END DO   ! Iop
      !
      RETURN
!!!
      DO Iop = 1, Nop
         IF ((OpTyp(Iop) == "epd") .OR. (OpTyp(Iop) == "ehn")) THEN
            Icurrnt = IpartnerP(Iop)
 100        Continue
            IF ((OpTyp(Icurrnt) == "epd") .OR. (OpTyp(Icurrnt) == "ehn")) THEN
               IpartnerC(Iop    ) = Icurrnt
               IpartnerC(Icurrnt) = Iop
               OpTyp(Iop    ) = "---"
               OpTyp(Icurrnt) = "---"
               CYCLE
            ELSE
               Icurrnt = IpartnerC(Icurrnt)
               Icurrnt = IpartnerP(Icurrnt)
               GO TO 100
            END IF
         END IF
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefPhase(IpartnerP, IpartnerC, Nop, Nloop)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Count loops 
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      !
      INTEGER :: Nop, IpartnerP(*), IpartnerC(*), Nloop, Icurrnt, NextP, NextC, Iop
      LOGICAL :: Done(Nop)
      !
      DO Iop = 1, Nop
         Done(Iop) = .FALSE.
      END DO
      !
      Nloop = 0
      !
      DO Iop = 1, Nop
         IF (Done(Iop)) CYCLE
         Icurrnt = Iop
 100     Continue
         NextP = IpartnerP(Icurrnt)
         NextC = IpartnerC(NextP)
         Done(Icurrnt) = .TRUE.
         Done(NextP) = .TRUE.
         Done(NextC) = .TRUE.
         IF (NextC == Iop) THEN
            Nloop = Nloop + 1
            CYCLE
         END IF
         Icurrnt = NextC
         GO TO 100
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CheckOrderT(M1, M2, M3, M13, IOUT)
      !************************************************************
      !
      !************************************************************
      IMPLICIT NONE
      !
      INTEGER :: M1(4), M2(4), M3(4), M1o(4), M2o(4), M3o(4), M13
      INTEGER :: NdimV, I, N1234, IT1, IT2, IT3, IT4, N1234tot
      INTEGER :: M1T1, M2T1, M3T1, M1T2, M2T2, M3T2, M1T3, M2T3, M3T3, M1T4, M2T4, M3T4
      INTEGER :: Nstring(4,24), NdimMx(24), NclcMx(24), NdimOccMx(24), NdimVirMx(24), &
     &   NclcOccMx(24), NclcVirMx(24)
      INTEGER :: NCntT, NdimW4, NdimW3, NdimW2, NdimW1, NclcW4, NclcW3, NclcW2, NclcW1
      INTEGER :: IDdim, Ndim, NdimVir, NdimMn, NclcMn, NdimVirMn, NclcVirMn, Nclc, NclcVir
      INTEGER :: IDclc, IDuse, IOUT
      INTEGER :: NDimOccV, NdimVirV, NdimOccW4, NdimOccW3, NdimOccW2, NdimOccW1, &
     &   NdimVirW4, NdimVirW3, NdimVirW2, NdimVirW1, &
     &   NclcOccW4, NclcOccW3, NclcOccW2, NclcOccW1, &
     &   NclcVirW4, NclcVirW3, NclcVirW2, NclcVirW1
      !
      ! --- Type of interaction vertex
      !
      SELECT CASE (M13)
         CASE (1)
            NdimV = 2
            NdimOccV = 0
            NdimVirV = 2
         CASE (2)
            NdimV = 2
            NdimOccV = 2
            NdimVirV = 0
         CASE (3, 12)
            NdimV = 2
            NdimOccV = 1
            NdimVirV = 1
         CASE (4)
            NdimV = 4
            NdimOccV = 0
            NdimVirV = 4
         CASE (5)
            NdimV = 4
            NdimOccV = 4
            NdimVirV = 0
         CASE (6, 11, 13)
            NdimV = 4
            NdimOccV = 2
            NdimVirV = 2
         CASE (7, 9)
            NdimV = 4
            NdimOccV = 1
            NdimVirV = 3
         CASE (8, 10)
            NdimV = 4
            NdimOccV = 3
            NdimVirV = 1
      END SELECT
      !
      ! --- Count T vertices
      !
      NCntT = 0
      DO I = 1, 4
         IF (M1(I) > 0) NCntT = NCntT + 1
      END DO
      IF (NCntT <= 1) RETURN
      !
      ! --- Save original ordering
      !
      DO I = 1, 4
         M1o(I) = M1(I)
         M2o(I) = M2(I)
         M3o(I) = M3(I)
      END DO
      !
      ! --- Examine (NT)! ways of contraction (NT = # of T operators)
      !
      N1234 = 0
      DO 100 IT1 = 1, NCntT
         M1T1 = M1(IT1)
         M2T1 = M2(IT1)
         M3T1 = M3(IT1)
         DO 200 IT2 = 1, NCntT
            IF  (IT2 == IT1) CYCLE
            M1T2 = M1(IT2)
            M2T2 = M2(IT2)
            M3T2 = M3(IT2)
            IF (NCntT == 2) THEN
               N1234 = N1234 + 1
               NString(1,N1234) = IT1
               NString(2,N1234) = IT2
               NdimW2 = NdimV  + 2*M1T2 - 2*M2T2
               NclcW2 = NdimV  + 2*M1T2 - M2T2
               NdimW1 = NdimW2 + 2*M1T1 - 2*M2T1
               NclcW1 = NdimW2 + 2*M1T1 - M2T1
!
               NdimOccW2 = NdimOccV  + M1T2 - 2*(M2T2 - M3T2)
               NclcOccW2 = NdimOccV  + M1T2 - (M2T2 - M3T2)
               NdimOccW1 = NdimOccW2 + M1T1 - 2*(M2T1 - M3T1)
               NclcOccW1 = NdimOccW2 + M1T1 - (M2T1 - M3T1)
!
               NdimVirW2 = NdimVirV  + M1T2 - 2*M3T2
               NclcVirW2 = NdimVirV  + M1T2 - M3T2
               NdimVirW1 = NdimVirW2 + M1T1 - 2*M3T1
               NclcVirW1 = NdimVirW2 + M1T1 - M3T1
!
               NdimMx(N1234) = MAX(NdimW1, NdimW2)
               NdimOccMx(N1234) = MAX(NdimOccW1, NdimOccW2)
               NdimVirMx(N1234) = MAX(NdimVirW1, NdimVirW2)
               NclcMx(N1234) = MAX(NclcW1, NclcW2)
               NclcOccMx(N1234) = MAX(NclcOccW1, NclcOccW2)
               NclcVirMx(N1234) = MAX(NclcVirW1, NclcVirW2)
               GO TO 200
            END IF
            DO 300 IT3 = 1, NCntT
               IF ((IT3 == IT1) .OR. (IT3 == IT2)) CYCLE
               M1T3 = M1(IT3)
               M2T3 = M2(IT3)
               M3T3 = M3(IT3)
               IF (NCntT == 3) THEN
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NdimW3 = NdimV  + 2*M1T3 - 2*M2T3
                  NclcW3 = NdimV  + 2*M1T3 - M2T3
                  NdimW2 = NdimW3 + 2*M1T2 - 2*M2T2
                  NclcW2 = NdimW3 + 2*M1T2 - M2T2
                  NdimW1 = NdimW2 + 2*M1T1 - 2*M2T1
                  NclcW1 = NdimW2 + 2*M1T1 - M2T1
!
                  NdimOccW3 = NdimOccV  + M1T3 - 2*(M2T3 - M3T3)
                  NclcOccW3 = NdimOccV  + M1T3 - (M2T3 - M3T3)
                  NdimOccW2 = NdimOccW3 + M1T2 - 2*(M2T2 - M3T2)
                  NclcOccW2 = NdimOccW3 + M1T2 - (M2T2 - M3T2)
                  NdimOccW1 = NdimOccW2 + M1T1 - 2*(M2T1 - M3T1)
                  NclcOccW1 = NdimOccW2 + M1T1 - (M2T1 - M3T1)
!
                  NdimVirW3 = NdimVirV  + M1T3 - 2*M3T3
                  NclcVirW3 = NdimVirV  + M1T3 - M3T3
                  NdimVirW2 = NdimVirW3 + M1T2 - 2*M3T2
                  NclcVirW2 = NdimVirW3 + M1T2 - M3T2
                  NdimVirW1 = NdimVirW2 + M1T1 - 2*M3T1
                  NclcVirW1 = NdimVirW2 + M1T1 - M3T1
!
                  NdimMx(N1234) = MAX(NdimW1, NdimW2, NdimW3)
                  NdimOccMx(N1234) = MAX(NdimOccW1, NdimOccW2, NdimOccW3)
                  NdimVirMx(N1234) = MAX(NdimVirW1, NdimVirW2, NdimVirW3)
                  NclcMx(N1234) = MAX(NclcW1, NclcW2, NclcW3)
                  NclcOccMx(N1234) = MAX(NclcOccW1, NclcOccW2, NclcOccW3)
                  NclcVirMx(N1234) = MAX(NclcVirW1, NclcVirW2, NclcVirW3)
                  GO TO 300
               END IF
               DO 400 IT4 = 1, NCntT
                  IF ((IT4 == IT1) .OR. (IT4 == IT2) .OR. (IT4 == IT3)) CYCLE
                  M1T4 = M1(IT4)
                  M2T4 = M2(IT4)
                  M3T4 = M3(IT4)
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NString(4,N1234) = IT4
                  NdimW4 = NdimV  + 2*M1T4 - 2*M2T4
                  NclcW4 = NdimV  + 2*M1T4 -   M2T4
                  NdimW3 = NdimW4 + 2*M1T3 - 2*M2T3
                  NclcW3 = NdimW4 + 2*M1T3 -   M2T3
                  NdimW2 = NdimW3 + 2*M1T2 - 2*M2T2
                  NclcW2 = NdimW3 + 2*M1T2 -   M2T2
                  NdimW1 = NdimW2 + 2*M1T1 - 2*M2T1
                  NclcW1 = NdimW2 + 2*M1T1 -   M2T1
!
                  NdimOccW4 = NdimOccV  + M1T4 - 2*(M2T4 - M3T4)
                  NclcOccW4 = NdimOccV  + M1T4 - (M2T4 - M3T4)
                  NdimOccW3 = NdimOccW4 + M1T3 - 2*(M2T3 - M3T3)
                  NclcOccW3 = NdimOccW4 + M1T3 - (M2T3 - M3T3)
                  NdimOccW2 = NdimOccW3 + M1T2 - 2*(M2T2 - M3T2)
                  NclcOccW2 = NdimOccW3 + M1T2 - (M2T2 - M3T2)
                  NdimOccW1 = NdimOccW2 + M1T1 - 2*(M2T1 - M3T1)
                  NclcOccW1 = NdimOccW2 + M1T1 - (M2T1 - M3T1)
!
                  NdimVirW4 = NdimVirV  + M1T4 - 2*M3T4
                  NclcVirW4 = NdimVirV  + M1T4 - M3T4
                  NdimVirW3 = NdimVirW4 + M1T3 - 2*M3T3
                  NclcVirW3 = NdimVirW4 + M1T3 - M3T3
                  NdimVirW2 = NdimVirW3 + M1T2 - 2*M3T2
                  NclcVirW2 = NdimVirW3 + M1T2 - M3T2
                  NdimVirW1 = NdimVirW2 + M1T1 - 2*M3T1
                  NclcVirW1 = NdimVirW2 + M1T1 - M3T1
!
                  NdimMx(N1234) = MAX(NdimW1, NdimW2, NdimW3, NdimW4)
                  NdimOccMx(N1234) = MAX(NdimOccW1, NdimOccW2, NdimOccW3, NdimOccW4)
                  NdimVirMx(N1234) = MAX(NdimVirW1, NdimVirW2, NdimVirW3, NdimVirW4)
                  NclcMx(N1234) = MAX(NclcW1,NclcW2,NclcW3,NclcW4)
                  NclcOccMx(N1234) = MAX(NclcOccW1, NclcOccW2, NclcOccW3, NclcOccW4)
                  NclcVirMx(N1234) = MAX(NclcVirW1, NclcVirW2, NclcVirW3, NclcVirW4)
                  GO TO 400

 400           CONTINUE
 300        CONTINUE
 200     CONTINUE
 100  CONTINUE
      !
      N1234tot = N1234
      !
      ! --- Identify the least memory-demanding one
      !      
      NdimMn = NdimMx(1)
      IDdim = 1
      DO N1234 = 1, N1234tot
         Ndim = NdimMx(N1234)
         IF (Ndim < NdimMn) THEN
            NdimMn = Ndim
            IDdim = N1234
         END IF
      END DO
      !
      NdimVirMn = NdimMn
      DO N1234 = 1, N1234tot
         IF (NdimMx(N1234) > NdimMn) CYCLE
         NdimVir = NdimVirMx(N1234)
         IF (NdimVir < NdimVirMn) THEN
            NdimVirMn = NdimVir
            IDdim = N1234
         END IF
      END DO
      !
      ! --- Identify the computationally most effective one
      !
      NclcMn = NclcMx(1)
      IDclc = 1
      DO N1234 = 1, N1234tot
         Nclc = NclcMx(N1234)
!!!
      IT1 = NString(1,N1234)
      IT2 = NString(2,N1234)
      IT3 = NString(3,N1234)
      IT4 = NString(4,N1234)
      WRITE(IOUT, '("Diagram ", 12I1, I2, " Max order: ", I2)') &
     &        M1(IT1), M2(IT1), M3(IT1), M1(IT2), M2(IT2), M3(IT2), &
     &        M1(IT3), M2(IT3), M3(IT3), M1(IT4), M2(IT4), M3(IT4), M13, &
     &        Nclc
!!!
         IF (Nclc < NclcMn) THEN
            NclcMn = Nclc
            IDclc = N1234
         END IF
      END DO
      !
      NclcVirMn = NclcMn
      DO N1234 = 1, N1234tot
         IF (NclcMx(N1234) > NclcMn) CYCLE
         NclcVir = NclcVirMx(N1234)
!!!
         IT1 = NString(1,N1234)
         IT2 = NString(2,N1234)
         IT3 = NString(3,N1234)
         IT4 = NString(4,N1234)
         WRITE(IOUT, '("Diagram ", 12I1, I2, " Max Vir. order: ", I2)') &
     &      M1(IT1), M2(IT1), M3(IT1), M1(IT2), M2(IT2), M3(IT2), &
     &      M1(IT3), M2(IT3), M3(IT3), M1(IT4), M2(IT4), M3(IT4), M13, &
     &      NclcVir
!!!
         IF (NclcVir < NclcVirMn) THEN
            NclcVirMn = NclcVir
            IDclc = N1234
         END IF
      END DO
      !
      IDuse = IDclc
      !
      IF (IDuse == 1) RETURN
      !
      ! --- New ordering
      !
      IT1 = NString(1,IDuse)
      IT2 = NString(2,IDuse)
      IT3 = NString(3,IDuse)
      IT4 = NString(4,IDuse)
      !
      M1(1) = M1o(IT1)
      M2(1) = M2o(IT1)
      M3(1) = M3o(IT1)
      !
      M1(2) = M1o(IT2)
      M2(2) = M2o(IT2)
      M3(2) = M3o(IT2)
      !
      IF (NCntT >= 3) THEN
         M1(3) = M1o(IT3)
         M2(3) = M2o(IT3)
         M3(3) = M3o(IT3)
         IF (NCntT == 4) THEN
            M1(4) = M1o(IT4)
            M2(4) = M2o(IT4)
            M3(4) = M3o(IT4)
         END IF
      END IF
      !
      WRITE(IOUT, '("T operators reordering: ", 12I1, I2, " -> ", 12I1, I2)') &
     &   M1o(1), M2o(1), M3o(1), M1o(2), M2o(2), M3o(2), &
     &   M1o(3), M2o(3), M3o(3), M1o(4), M2o(4), M3o(4), M13, &
     &   M1(1), M2(1), M3(1), M1(2), M2(2), M3(2), &
     &   M1(3), M2(3), M3(3), M1(4), M2(4), M3(4), M13
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CheckOrderTR(M1, M2, M3, M13, IOut)
      !************************************************************
      !
      !************************************************************
      IMPLICIT NONE
      !
      INTEGER :: M1(4), M2(4), M3(4), M1orig(4), M2orig(4), M3orig(4), M13
      INTEGER :: NDimV, I, N1234, IT1, IT2, IT3, IT4, N1234tot, &
     &   M1T1, M2T1, M3T1, M1T2, M2T2, M3T2, M1T3, M2T3, M3T3, M1T4, M2T4, M3T4, &
     &   NString(4, 24), NDimMx(24), NClcMx(24), NumT, &
     &   NDimW4, NDimW3, NDimW2, NDimW1, NClcW4, NClcW3, NClcW2, NClcW1, &
     &   IDDim, NDim, NDimMn, NClcMn, NClc, IDClc, IDuse, IOUT
      !
      ! --- Type of interaction vertex
      !
      SELECT CASE (M13)
         CASE (1, 2, 3, 12)
            NDimV = 2
         CASE (4, 5, 6, 7, 8, 9, 10, 11, 13)
            NDimV = 4
      END SELECT
      !
      ! --- Count T and R vertices
      !
      NumT = 0
      DO I = 1, 4
         IF (IABS(M1(I)) > 0) NumT = NumT + 1
      END DO
      IF (NumT <= 1) RETURN
      !
      ! --- Save original ordering
      !
      DO I = 1, 4
         M1orig(I) = M1(I)
         M2orig(I) = M2(I)
         M3orig(I) = M3(I)
      END DO
      !
      ! --- Examine (NumT)! ways of contraction (NumT = # of T and R operators)
      !
      N1234 = 0
      DO 100 IT1 = 1, NumT
         M1T1 = IABS(M1(IT1))
         M2T1 = IABS(M2(IT1))
         M3T1 = IABS(M3(IT1))
         DO 200 IT2 = 1, NumT
            IF  (IT2 == IT1) CYCLE
            M1T2 = IABS(M1(IT2))
            M2T2 = IABS(M2(IT2))
            M3T2 = IABS(M3(IT2))
            IF (NumT == 2) THEN
               N1234 = N1234 + 1
               NString(1,N1234) = IT1
               NString(2,N1234) = IT2
               NDimW2 = NDimV  + 2*M1T2 - 2*M2T2
               NClcW2 = NDimV  + 2*M1T2 - M2T2
               NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
               NClcW1 = NDimW2 + 2*M1T1 - M2T1
               NDimMx(N1234) = MAX(NDimW1,NDimW2)
               NClcMx(N1234) = MAX(NClcW1,NClcW2)
               GO TO 200
            END IF
            DO 300 IT3 = 1, NumT
               IF ((IT3 == IT1) .OR. (IT3 == IT2)) CYCLE
               M1T3 = IABS(M1(IT3))
               M2T3 = IABS(M2(IT3))
               M3T3 = IABS(M3(IT3))
               IF (NumT == 3) THEN
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NDimW3 = NDimV  + 2*M1T3 - 2*M2T3
                  NClcW3 = NDimV  + 2*M1T3 - M2T3
                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
                  NClcW2 = NDimW3 + 2*M1T2 - M2T2
                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
                  NClcW1 = NDimW2 + 2*M1T1 - M2T1
                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3)
                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3)
                  GO TO 300
               END IF
               DO 400 IT4 = 1, NumT
                  IF ((IT4 == IT1) .OR. (IT4 == IT2) .OR. (IT4 == IT3)) CYCLE
                  M1T4 = IABS(M1(IT4))
                  M2T4 = IABS(M2(IT4))
                  M3T4 = IABS(M3(IT4))
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NString(4,N1234) = IT4
                  NDimW4 = NDimV  + 2*M1T4 - 2*M2T4
                  NClcW4 = NDimV  + 2*M1T4 -   M2T4
                  NDimW3 = NDimW4 + 2*M1T3 - 2*M2T3
                  NClcW3 = NDimW4 + 2*M1T3 -   M2T3
                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
                  NClcW2 = NDimW3 + 2*M1T2 -   M2T2
                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
                  NClcW1 = NDimW2 + 2*M1T1 -   M2T1
                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3,NDimW4)
                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3,NClcW4)
                  GO TO 400

 400           CONTINUE
 300        CONTINUE
 200     CONTINUE
 100  CONTINUE
      !
      N1234tot = N1234
      !
      ! --- Identify the least memory-demanding one
      !      
      NDimMn = NDimMx(1)
      IDDim = 1
      DO N1234 = 1, N1234tot
         NDim = NDimMx(N1234)
         IF (NDim < NDimMn) THEN
            NDimMn = NDim
            IDDim = N1234
         END IF
      END DO
      !
      ! --- Identify the computationally most effective one
      !
      NClcMn = NClcMx(1)
      IDClc = 1
      DO N1234 = 1, N1234tot
         NClc = NClcMx(N1234)
         IF (NClc < NClcMn) THEN
            NClcMn = NClc
            IDClc = N1234
         END IF
      END DO
      !
      IDuse = IDClc   ! Choose computationally most effective one
      !
      IF (IDuse == 1) RETURN
      !
      ! --- New ordering
      !
      IT1 = NString(1,IDuse)
      IT2 = NString(2,IDuse)
      IT3 = NString(3,IDuse)
      IT4 = NString(4,IDuse)
      !
      M1(1) = M1orig(IT1)
      M2(1) = M2orig(IT1)
      M3(1) = M3orig(IT1)
      !
      M1(2) = M1orig(IT2)
      M2(2) = M2orig(IT2)
      M3(2) = M3orig(IT2)
      !
      IF (NumT >= 3) THEN
         M1(3) = M1orig(IT3)
         M2(3) = M2orig(IT3)
         M3(3) = M3orig(IT3)
         IF (NumT == 4) THEN
            M1(4) = M1orig(IT4)
            M2(4) = M2orig(IT4)
            M3(4) = M3orig(IT4)
         END IF
      END IF
      !
      WRITE(IOUT, '("T operators reordering: ", 12I2, I2, " -> ", 12I2, I2)') &
     &   M1orig(1), M2orig(1), M3orig(1), M1orig(2), M2orig(2), M3orig(2), &
     &   M1orig(3), M2orig(3), M3orig(3), M1orig(4), M2orig(4), M3orig(4), M13, &
     &   M1(1), M2(1), M3(1), M1(2), M2(2), M3(2), &
     &   M1(3), M2(3), M3(3), M1(4), M2(4), M3(4), M13
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Declaration(ICOD, TYPE, Complx)
        !************************************************************
        !     Declare local variables
        !************************************************************
      IMPLICIT NONE
      !
      LOGICAL:: Complx
      CHARACTER(*) :: TYPE
      INTEGER :: ICOD
      !
      ! ---REAL or COMPLEX-----
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "COMPLEX(8) :: WgtFac")')
      ELSE
         WRITE(ICOD, '(6X, "REAL(8) :: WgtFac")')
      END IF
      !
      ! --- INTEGER
      !
      WRITE(ICOD, '(6X, "INTEGER :: NumDig")')
      WRITE(ICOD, '(6X, "INTEGER :: NPwr")')
      WRITE(ICOD, '(6X, "INTEGER :: LevelT")')
      WRITE(ICOD, '(6X, "INTEGER :: LevelTOcc")')
      WRITE(ICOD, '(6X, "INTEGER :: LevelTVir")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeT")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeT_")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeG")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeG_")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV1")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV2")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV3")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV3_")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV4")')
      WRITE(ICOD, '(6X, "INTEGER :: NSizeV4_")')
      WRITE(ICOD, '(6X, "INTEGER :: nA")')
      WRITE(ICOD, '(6X, "INTEGER :: nI")')
      WRITE(ICOD, '(6X, "INTEGER :: nB")')
      WRITE(ICOD, '(6X, "INTEGER :: nJ")')
      WRITE(ICOD, '(6X, "INTEGER :: nC")')
      WRITE(ICOD, '(6X, "INTEGER :: nK")')
      WRITE(ICOD, '(6X, "INTEGER :: nD")')
      WRITE(ICOD, '(6X, "INTEGER :: nL")')
      WRITE(ICOD, '(6X, "INTEGER :: mAI")')
      WRITE(ICOD, '(6X, "INTEGER :: mB")')
      WRITE(ICOD, '(6X, "INTEGER :: mJ")')
      WRITE(ICOD, '(6X, "INTEGER :: mD")')
      WRITE(ICOD, '(6X, "INTEGER :: mL")')
      WRITE(ICOD, '(6X, "INTEGER :: mC")')
      WRITE(ICOD, '(6X, "INTEGER :: mK")')
      WRITE(ICOD, '(6X, "INTEGER :: nAo")')
      WRITE(ICOD, '(6X, "INTEGER :: nIo")')
      WRITE(ICOD, '(6X, "INTEGER :: nAt")')
      WRITE(ICOD, '(6X, "INTEGER :: nIt")')
      WRITE(ICOD, '(6X, "INTEGER :: nAn")')
      WRITE(ICOD, '(6X, "INTEGER :: nIn")')
      WRITE(ICOD, '(6X, "INTEGER :: mBJ")')
      WRITE(ICOD, '(6X, "INTEGER :: mCK")')
      WRITE(ICOD, '(6X, "INTEGER :: mAo")')
      WRITE(ICOD, '(6X, "INTEGER :: mIo")')
      WRITE(ICOD, '(6X, "INTEGER :: mAn")')
      WRITE(ICOD, '(6X, "INTEGER :: mIn")')
      WRITE(ICOD, '(6X, "INTEGER :: mAt")')
      WRITE(ICOD, '(6X, "INTEGER :: mIt")')
      IF (TRIM(TYPE) == 'LAMBDA') THEN
         WRITE(ICOD, '(6X, "INTEGER :: NSizeV1_")')
         WRITE(ICOD, '(6X, "INTEGER :: mA")')
         WRITE(ICOD, '(6X, "INTEGER :: mI")')
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Up2Lw(buf)
      ! **************************************************
      ! *  convert uppercase letters to lower-case ones  *
      ! *                 (A-Z)               (a-z)      *
      ! *    coded by Naoki MAEDA on 2000/10/26          *
      ! **************************************************
       CHARACTER :: buf*(*)
       INTEGER :: n2, i
       !
       n2 = LEN(buf)
       DO 200 i = 1, n2
       IF (ichar(buf(i:i)).GE.65 .and. ichar(buf(i:i)).LE.90) &
     &         buf(i:i)=char(ichar(buf(i:i))+32)
 200  CONTINUE
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiagJacobian(NDig, NMax, NRank, M1,  M2,  M3,  M13,  Mwp, &
     &   NDigJ, M1J, M2J, M3J, M13J, MwpJ, Diag)
      !**************************************************************************
      !     Generate diagrams for Jacobian times R amplitudes
      !
      !     Diag ... F: diagrams for Jacobian elements
      !              T: diagrams for only diagonal elements of Jacobian
      !**************************************************************************
      IMPLICIT NONE
      !
      LOGICAL Diag
      INTEGER NDig, NMax, NRank, NDigJ
      INTEGER M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      INTEGER M1J(4,*), M2J(4,*), M3J(4,*), M13J(*), MwpJ(*)
      !
      INTEGER IDig, IT, JT, M1T, M2T, M3T, M1T_old, M2T_old, M3T_old, IOUT
      !
      COMMON/IOFILE/IOUT
      !
      NDigJ = 0
      !
      DO IDig = 1, NDig
         M1T_old = 0
         M2T_old = 0
         M3T_old = 0
         DO IT = 1, 4
            IF (M1(IT, IDig) == 0) EXIT
            M1T = M1(IT,IDig)
            M2T = M2(IT,IDig)
            M3T = M3(IT,IDig)
            IF (M1T == M1T_old .AND. M2T == M2T_old .AND. M3T == M3T_old) CYCLE
            M1T_old = M1T
            M2T_old = M2T
            M3T_old = M3T
            !
            IF (Diag) THEN
               IF (M1T /= NRank) CYCLE
            END IF
            NDigJ = NDigJ + 1
            M1J(1,NDigJ) = -M1T
            M2J(1,NDigJ) = -M2T
            M3J(1,NDigJ) = -M3T
            DO JT = 2, IT
               M1J(JT,NDigJ) = M1(JT-1, IDig)
               M2J(JT,NDigJ) = M2(JT-1, IDig)
               M3J(JT,NDigJ) = M3(JT-1, IDig)
            END DO   ! JT
            DO JT = IT+1, 4
               M1J(JT,NDigJ) = M1(JT, IDig)
               M2J(JT,NDigJ) = M2(JT, IDig)
               M3J(JT,NDigJ) = M3(JT, IDig)
            END DO   ! JT
            M13J(NDigJ) = M13(IDig)
            !
            ! --- Weight Factor never exceeds 2 (there are no more than 1 pair of equivalent T)
            !
            MwpJ(NDigJ) = 1
            DO JT = 3, 4
               M1T = M1J(JT,NDigJ)
               M2T = M2J(JT,NDigJ)
               M3T = M3J(JT,NDigJ)
               IF (M1T == 0) EXIT
               IF (M1T == M1J(JT-1, NDigJ)) THEN
                  IF (M2T == M2J(JT-1, NDigJ)) THEN
                     IF (M3T == M3J(JT-1, NDigJ)) THEN
                        MwpJ(NDigJ) = MwpJ(NDigJ) + 1
                     END IF
                  END IF
               END IF
            END DO
            !
            ! --- Phase Factor is the same to the one from the parent diagram
            !
            IF (Mwp(IDig) < 0) MwpJ(NDigJ) = -MwpJ(NDigJ)
!Cc
!            WRITE(IOUT, '(I3, X, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, 2I3)') 
!     *         NDigJ,
!     *         M1J(1,NDigJ), M2J(1,NDigJ), M3J(1,NDigJ),
!     *         M1J(2,NDigJ), M2J(2,NDigJ), M3J(2,NDigJ),
!     *         M1J(3,NDigJ), M2J(3,NDigJ), M3J(3,NDigJ),
!     *         M1J(4,NDigJ), M2J(4,NDigJ), M3J(4,NDigJ),
!     *         M13J(NDigJ), MwpJ(NDigJ), IDig
!CC
            !
         END DO   ! IT
      END DO   ! IDig
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SortDiagJacobian(NDig, M1, M2, M3, M13, Mwp)
      !**************************************************************************
      !     Sort diagrams
      !**************************************************************************
      IMPLICIT NONE
      !
      INTEGER :: NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      INTEGER :: M1orig(4,NDig), M2orig(4,NDig), M3orig(4,NDig), M13orig(NDig), Mwp_orig(NDig)
      !
      LOGICAL :: Finish, NextR
      INTEGER, PARAMETER :: MFinish = 0
      INTEGER :: IDig, IT, ICnt
      INTEGER :: M1R, M2R, M3R, M1Rtmp, M2Rtmp, M3Rtmp, IOUT
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Copy 13-digit integers to temporal arrays
      !
      DO IDig = 1, NDig
         DO IT = 1, 4
            M1orig(IT, IDig) = M1(IT, IDig)
            M2orig(IT, IDig) = M2(IT, IDig)
            M3orig(IT, IDig) = M3(IT, IDig)
         END DO
         M13orig(IDig) = M13(IDig)
         Mwp_orig(IDig) = Mwp(IDig)
      END DO
      !
      ! --- Search unique R operators
      !
      IDig = 1
      M1Rtmp = M1(1,IDig)
      M2Rtmp = M2(1,IDig)
      M3Rtmp = M3(1,IDig)
      NextR = .FALSE.
      !
      Finish = .FALSE.
      ICnt = 1
 100  CONTINUE
      DO IDig = 2, NDig
         M1R = M1orig(1,IDig)
         M2R = M2orig(1,IDig)
         M3R = M3orig(1,IDig)
         IF (M1R == MFinish) GO TO 200
         IF (NextR) THEN
            M1Rtmp = M1R
            M2Rtmp = M2R
            M3Rtmp = M3R
            ICnt = ICnt + 1
            DO IT = 1, 4
               M1(IT,ICnt) = M1orig(IT,IDig)
               M2(IT,ICnt) = M2orig(IT,IDig)
               M3(IT,ICnt) = M3orig(IT,IDig)
            END DO
            M13(ICnt) = M13orig(IDig)
            Mwp(ICnt) = Mwp_orig(IDig)
            M1orig(1,IDig) = MFinish  ! The IDig-th diagram in the original ordering is done.
            NextR = .FALSE.
            GO TO 200
         END IF
         IF (M1R /= M1Rtmp .OR. M2R /= M2Rtmp .OR. M3R /= M3Rtmp) GO TO 200
         ICnt = ICnt + 1
         DO IT = 1, 4
            M1(IT,ICnt) = M1orig(IT,IDig)
            M2(IT,ICnt) = M2orig(IT,IDig)
            M3(IT,ICnt) = M3orig(IT,IDig)
         END DO
         M13(ICnt) = M13orig(IDig)
         Mwp(ICnt) = Mwp_orig(IDig)
         M1orig(1,IDig) = MFinish   ! The IDig-th diagram in the original ordering is done.
         !
 200     CONTINUE
         !
         Finish = (ICnt == NDig)
         IF  (Finish) EXIT
         IF (IDig == NDig) NextR = .TRUE.
      END DO   ! IDig
      !
      IF (.NOT. Finish) GO TO 100
      !
!      WRITE(IOUT, '("Sorted Jacobian diagrams:")')
!      DO IDig = 1, NDig
!         WRITE(IOUT, '(I3, X, 3I2, "/", 3I1, "/", 3I1, "/", 3I1, X, I2)') 
!     *      IDig,
!     *      M1(1,IDig), M2(1,IDig), M3(1,IDig),
!     *      M1(2,IDig), M2(2,IDig), M3(2,IDig),
!     *      M1(3,IDig), M2(3,IDig), M3(3,IDig),
!     *      M1(4,IDig), M2(4,IDig), M3(4,IDig),
!     *      M13(IDig)
!      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE WeightDiag(NDig, M1, M2, M3, Mwp)
      !**************************************************************************
      !     Define weight factors
      !**************************************************************************
      IMPLICIT NONE
      !
      INTEGER NDig
      INTEGER M1(4,*), M2(4,*), M3(4,*), Mwp(*)
      !
      INTEGER IDig, NPair, IT, JT, M1I, M2I, M3I, M1J, M2J, M3J, IOUT
      !
      COMMON/IOFILE/IOUT
      !
      DO IDig = 1, NDig
         NPair = 0
         DO IT = 1, 3
            M1I = M1(IT,IDig)
            M2I = M2(IT,IDig)
            M3I = M3(IT,IDig)            
            IF (M1I == 0) EXIT
            DO JT = IT+1, 4
               M1J = M1(JT,IDig)
               M2J = M2(JT,IDig)
               M3J = M3(JT,IDig)            
               IF (M1J == 0) EXIT
               IF (M1I /= M1J .OR. M2I /= M2J .OR. M3I /= M3J) CYCLE
               NPair = NPair + 1
            END DO
         END DO
         IF (Mwp(IDig) > 0) THEN
            Mwp(IDig) = NPair + 1
         ELSE
            Mwp(IDig) = -(NPair + 1)
         END IF
      END DO   ! IDig
      !
!      WRITE(IOUT, '("Weight Factors")')
!      DO IDig = 1, NDig
!         WRITE(IOUT, '(I3, X, 3I2, "/", 3I2, "/", 3I2, "/", 3I2, "/", X, I5)')
!     *      IDig, 
!     *      M1(1,IDig), M2(1,IDig), M3(1,IDig), 
!     *      M1(2,IDig), M2(2,IDig), M3(2,IDig), 
!     *      M1(3,IDig), M2(3,IDig), M3(3,IDig), 
!     *      M1(4,IDig), M2(4,IDig), M3(4,IDig), 
!     *      Mwp(IDig)
!      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiagLambdaPre(NDigTotal, NMax, NRankParent, NRankL, Constant, &
     &   M1P,  M2P,  M3P,  M13P,  NDigL, LArray, M1L, M2L, M3L, M13L)
        !**************************************************************************
        !     Generate lambda diagrams
        !
        !     M1L, M2L, M3L include the removed T vertices at the  leftmost position
        !     LArry contains the lambda vertices
        !**************************************************************************
      !
      USE BasicInformation, ONLY : LOff
      USE Vertex_Module, ONLY : NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      LOGICAL :: Constant
      INTEGER :: NDigTotal, NMax, NRankParent(*), NRankL, M1P(4,*), M2P(4,*), M3P(4,*), M13P(*)
      INTEGER :: NDigL, LArray(3,*), M1L(4,*), M2L(4,*), M3L(4,*), M13L(*)
      !
      LOGICAL :: Update, Found
      INTEGER :: M1Lold(4,1000), M2Lold(4,1000), M3Lold(4,1000), M13Lold(1000), M1L1, M2L1, M3L1
      INTEGER :: IDig, JDig, IT, JT, KT, LocTI, LocTJ, M1I, M2I, M3I, M1J, M2J, M3J
      INTEGER :: Lambda1, Lambda2, Lambda3, M1Ltemp, M2Ltemp, M3Ltemp
      INTEGER :: Num_T_Replaced, Num_T_Replaced0, Loc_T_Replaced(4)
      INTEGER :: Num_T_Removed, Num_T_Removed0, Loc_T_Removed(4)
      INTEGER :: NumExtPart_Rm, NumExtHole_Rm
      INTEGER :: Num_ExtPart_IT, Num_ExtHole_IT, Num_IntPart_IT, Num_IntHole_IT
      INTEGER :: IOUT
      !
      COMMON/IOFILE/IOUT
      !
      NDigL = 0
      DO IDig = 1, NDigTotal   ! Loop over the parent diagrams
         !
         WRITE(IOUT, '("Parent ", &
     &      I3, ": ", 3I2, X, 3I2, X, 3I2, X, 3I2, I3)') &
     &      IDig, &
     &      M1P(1,IDig), M2P(1,IDig), M3P(1,IDig), &
     &      M1P(2,IDig), M2P(2,IDig), M3P(2,IDig), &
     &      M1P(3,IDig), M2P(3,IDig), M3P(3,IDig), &
     &      M1P(4,IDig), M2P(4,IDig), M3P(4,IDig), &
     &      M13P(IDig)
         !
         ! --- # of T vertices which can be replaced by a lambda vertex
         !
         Num_T_Replaced = 0
         DO IT = 1, 4
            IF (M1P(IT,IDig) == NRankL) THEN
               Num_T_Replaced = Num_T_Replaced + 1
               Loc_T_Replaced(Num_T_Replaced) = IT
            END IF
         END DO
         !
         IF (Num_T_Replaced == 0) GO TO 1000
         !
         ! --- Check equivalence of these T vertices
         !
         IF (Num_T_Replaced > 1) THEN
            Num_T_Replaced0 = Num_T_Replaced
            DO IT = Num_T_Replaced0, 1, -1
               LocTI = Loc_T_Replaced(IT)
               M1I = M1P(LocTI,IDig)
               M2I = M2P(LocTI,IDig)
               M3I = M3P(LocTI,IDig)
               DO JT = IT - 1, 1, -1
                  LocTJ = Loc_T_Replaced(JT)
                  M1J = M1P(LocTJ,IDig)
                  M2J = M2P(LocTJ,IDig)
                  M3J = M3P(LocTJ,IDig)
                  IF ((M1I == M1J) .AND. (M2I == M2J) .AND. (M3I == M3J)) THEN
                     Num_T_Replaced = Num_T_Replaced - 1
                     Loc_T_Replaced(IT) = 0
                  END IF
               END DO
            END DO
         END IF
         !
         IF (Num_T_Replaced /= Num_T_Replaced0) THEN
            DO IT = 1, Num_T_Replaced
               IF (Loc_T_Replaced(IT) /= 0) CYCLE
 100           CONTINUE
               DO JT = IT, Num_T_Replaced0 - 1
                  Loc_T_Replaced(JT) = Loc_T_Replaced(JT+1)
               END DO
               IF (Loc_T_Replaced(IT) == 0) GO TO 100
            END DO
         END IF
         !
         WRITE(IOUT, '(" # of replaced T vertices: ", I3)') Num_T_Replaced
         WRITE(IOUT, *) (Loc_T_Replaced(IT), IT = 1, Num_T_Replaced)
         !
         ! --- Rank of Lambda vertex (de-excitation operator)
         !
!YA070212         Lambda1 = LOff + NRankParent(IDig)
!YA070212         Lambda2 = LOff + NExtPart(M13P(IDig)) + NExtHole(M13P(IDig))
!YA070212         Lambda3 = LOff + NExtPart(M13P(IDig))
         !
         ! --- Define lambda vertex
         ! --- Mark the T vetrices which will be removed
         !
         DO IT = 1, Num_T_Replaced
            NDigL = NDigL + 1
            LocTI = Loc_T_Replaced(IT)
            DO JT = 1, 4
               IF (JT == LocTI) THEN
                  NumExtPart_Rm = M1P(JT,IDig) - M3P(JT,IDig)
                  NumExtHole_Rm = M1P(JT,IDig) - (M2P(JT,IDig) - M3P(JT,IDig))
                  !
                  Lambda1 = NRankParent(IDig)
                  Lambda2 = (Lambda1 * 2) - (NumExtPart_Rm + NumExtHole_Rm)
                  Lambda3 = Lambda1 - NumExtPart_Rm
                  !
                  Lambda1 = Lambda1 + LOff
                  Lambda2 = Lambda2 + LOff
                  Lambda3 = Lambda3 + LOff
                  !
                  LArray(1,NDigL) = Lambda1
                  LArray(2,NDigL) = Lambda2
                  LArray(3,NDigL) = Lambda3
                  M1L(JT,NDigL) = -M1P(JT,IDig)
                  M2L(JT,NDigL) = -M2P(JT,IDig)
                  M3L(JT,NDigL) = -M3P(JT,IDig)
!YA063012                  M1L(JT,NDigL) = -Lambda1
!YA063012                  M2L(JT,NDigL) = -Lambda2
!YA063012                  M3L(JT,NDigL) = -Lambda3
               ELSE
                  M1L(JT,NDigL) = M1P(JT,IDig)
                  M2L(JT,NDigL) = M2P(JT,IDig)
                  M3L(JT,NDigL) = M3P(JT,IDig)
               END IF
               M13L(NDigL) = M13P(IDig)
            END DO
            !
            ! --- Move the marked T vertix to the leftmost position
            !
            DO KT = 1, 4
               IF (M1L(KT,NDigL) < 0) THEN
                  M1Ltemp = M1L(KT,NDigL)
                  M2Ltemp = M2L(KT,NDigL)
                  M3Ltemp = M3L(KT,NDigL)
                  DO JT = KT - 1, 1, -1
                     M1L(JT+1,NDigL) = M1L(JT,NDigL)
                     M2L(JT+1,NDigL) = M2L(JT,NDigL)
                     M3L(JT+1,NDigL) = M3L(JT,NDigL)
                  END DO
                  M1L(1,NDigL) = M1Ltemp
                  M2L(1,NDigL) = M2Ltemp
                  M3L(1,NDigL) = M3Ltemp
                  EXIT
               ELSE
                  CYCLE
               END IF
            END DO
         END DO
         !
 1000 CONTINUE
         !
!         IF (.NOT. Constant) CYCLE
         CYCLE
         !
         ! o Terms arising from 1 (no de-excitation) operator
         !
         ! --- # of T vertices which can be removed
         !
         Num_T_Removed = 0
         DO IT = 1, 4
            Num_ExtPart_IT = M1P(IT,IDig) - M3P(IT,IDig)
            Num_ExtHole_IT = M1P(IT,IDig) - (M2P(IT,IDig) - M3P(IT,IDig))
            Num_IntPart_IT = M3P(IT,IDig)
            Num_IntHole_IT = M2P(IT,IDig) - M3P(IT,IDig)
            IF ((NRankParent(IDig) == Num_ExtPart_IT) .AND. (NRankParent(IDig) == Num_ExtHole_IT)) THEN
               IF ((Num_IntPart_IT == NRankL) .AND. (Num_IntHole_IT == NRankL)) THEN
                  Num_T_Removed = Num_T_Removed + 1
                  Loc_T_Removed(Num_T_Removed) = IT
               END IF
            END IF
         END DO
         !
         IF (Num_T_Removed == 0) CYCLE
         !
         ! --- Check equivalence of these T vertices
         !
         IF (Num_T_Removed > 1) THEN
            Num_T_Removed0 = Num_T_Removed
            DO IT = Num_T_Removed0, 1, -1
               LocTI = Loc_T_Removed(IT)
               M1I = M1P(LocTI,IDig)
               M2I = M2P(LocTI,IDig)
               M3I = M3P(LocTI,IDig)
               DO JT = IT - 1, 1, -1
                  LocTJ = Loc_T_Removed(JT)
                  M1J = M1P(LocTJ,IDig)
                  M2J = M2P(LocTJ,IDig)
                  M3J = M3P(LocTJ,IDig)
                  IF ((M1I == M1J) .AND. (M2I == M2J) .AND. (M3I == M3J)) THEN
                     Num_T_Removed = Num_T_Removed - 1
                     Loc_T_Removed(IT) = 0
                  END IF
               END DO
            END DO
         END IF
         !
         IF (Num_T_Removed /= Num_T_Removed0) THEN
            DO IT = 1, Num_T_Removed
               IF (Loc_T_Removed(IT) /= 0) CYCLE
 200           CONTINUE
               DO JT = IT, Num_T_Removed0 - 1
                  Loc_T_Removed(JT) = Loc_T_Removed(JT+1)
               END DO
               IF (Loc_T_Removed(IT) == 0) GO TO 200
            END DO
         END IF
         !
         WRITE(IOUT, '(" # of removed T vertices: ", I3)') Num_T_Removed
         WRITE(IOUT, *) (Loc_T_Removed(IT), IT = 1, Num_T_Removed)
         !
         ! --- Rank of Lambda vertex (Constant)
         !
         Lambda1 = LOff + 0
         Lambda2 = LOff + 0
         Lambda3 = LOff + 0
         !
         ! --- Define lambda vertex
         ! --- Mark the T vetrices which will be removed
         !
         DO IT = 1, Num_T_Removed
            NDigL = NDigL + 1
            LocTI = Loc_T_Removed(IT)
            DO JT = 1, 4
               IF (JT == LocTI) THEN
                  LArray(1,NDigL) = Lambda1
                  LArray(2,NDigL) = Lambda2
                  LArray(3,NDigL) = Lambda3
!                  M1L(JT,NDigL) = -M1P(JT,IDig)
!                  M2L(JT,NDigL) = -M2P(JT,IDig)
!                  M3L(JT,NDigL) = -M3P(JT,IDig)
                  M1L(JT,NDigL) = -M3P(JT,IDig)
                  M2L(JT,NDigL) = -M2P(JT,IDig)
                  M3L(JT,NDigL) = -M3P(JT,IDig)
               ELSE
                  M1L(JT,NDigL) = M1P(JT,IDig)
                  M2L(JT,NDigL) = M2P(JT,IDig)
                  M3L(JT,NDigL) = M3P(JT,IDig)
               END IF
               M13L(NDigL) = M13P(IDig)
            END DO
            !
            ! --- Move the marked T vertix to the leftmost position
            !
            DO KT = 1, 4
               IF (M1L(KT,NDigL) < 0) THEN
                  M1Ltemp = M1L(KT,NDigL)
                  M2Ltemp = M2L(KT,NDigL)
                  M3Ltemp = M3L(KT,NDigL)
                  DO JT = KT - 1, 1, -1
                     M1L(JT+1,NDigL) = M1L(JT,NDigL)
                     M2L(JT+1,NDigL) = M2L(JT,NDigL)
                     M3L(JT+1,NDigL) = M3L(JT,NDigL)
                  END DO
                  M1L(1,NDigL) = M1Ltemp
                  M2L(1,NDigL) = M2Ltemp
                  M3L(1,NDigL) = M3Ltemp
                  EXIT
               ELSE
                  CYCLE
               END IF
            END DO
         END DO
         !
      END DO   ! End of loop over the parent diagrams
      !
      ! o Terms arising from constant (One) operator
      !
      IF (Constant) THEN
         !
         ! --- Diagram 000 000 000 000 3
         !
         IF (NRankL == 1) THEN
            NDigL = NDigL + 1
            LArray(1,NDigL) = LOff
            LArray(2,NDigL) = LOff
            LArray(3,NDigL) = LOff
            M1L(1,NDigL) = 1
            M2L(1,NDigL) = 2
            M3L(1,NDigL) = 1
            DO IT = 2, 4
               M1L(IT,NDigL) = 0
               M2L(IT,NDigL) = 0
               M3L(IT,NDigL) = 0
            END DO
            M13L(NDigL) = 3
            !
            ! --- Diagram 000 121 000 000 11
            !
            NDigL = NDigL + 1
            LArray(1,NDigL) = LOff
            LArray(2,NDigL) = LOff
            LArray(3,NDigL) = LOff
            M1L(1,NDigL) = 1
            M2L(1,NDigL) = 2
            M3L(1,NDigL) = 1
            M1L(2,NDigL) = 1
            M2L(2,NDigL) = 2
            M3L(2,NDigL) = 1
            DO IT = 3, 4
               M1L(IT,NDigL) = 0
               M2L(IT,NDigL) = 0
               M3L(IT,NDigL) = 0
            END DO
            M13L(NDigL) = 11
         END IF
         !
         IF (NRankL == 2) THEN
            NDigL = NDigL + 1
            LArray(1,NDigL) = LOff
            LArray(2,NDigL) = LOff
            LArray(3,NDigL) = LOff
            M1L(1,NDigL) = 2
            M2L(1,NDigL) = 4
            M3L(1,NDigL) = 2
            DO IT = 2, 4
               M1L(IT,NDigL) = 0
               M2L(IT,NDigL) = 0
               M3L(IT,NDigL) = 0
            END DO
            M13L(NDigL) = 11
         END IF
         !
      END IF
      !
      WRITE(IOUT, '("Lambda diagrams")')
      DO IDig = 1, NDigL
         WRITE(IOUT, '(X, I3, "(", 3I5, ")", X, 3I2, "/", 3I2, "/", 3I2, "/", 3I2, "/", I3)') &
     &      IDig, &
     &      LArray(1,IDig), LArray(2,IDig), LArray(3,IDig), &
     &      M1L(1,IDig), M2L(1,IDig), M3L(1,IDig), &
     &      M1L(2,IDig), M2L(2,IDig), M3L(2,IDig), &
     &      M1L(3,IDig), M2L(3,IDig), M3L(3,IDig), &
     &      M1L(4,IDig), M2L(4,IDig), M3L(4,IDig), &
     &      M13L(IDig)
      END DO
      FLUSH(IOUT)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiagIPRight(NRank, M1,  M2,  M3,  M13,  Mwp, &
     &   NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !**************************************************************************
      !     Generate diagrams for IP right diagrams
      !**************************************************************************
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: NRank
      INTEGER, INTENT(INOUT) :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      INTEGER, INTENT(OUT) :: NDigJ, M1J(4,*), M2J(4,*), M3J(4,*), M13J(*), MwpJ(*)
      !
      INTEGER, PARAMETER :: IDIGFILE = 99
      CHARACTER(LEN=10) :: CDum10
      CHARACTER(LEN=6) :: CDum6
      INTEGER :: IOUT
      INTEGER :: IDig, IT, M1R, M2R, M3R
      INTEGER :: NDum, NRankRead, NDigRead, IDigRead
      !
      COMMON/IOFILE/IOUT
      !
      NDigJ = 0
      !
      ! o Read EE Jacobian right diagrams
      !
      OPEN(UNIT=IDIGFILE, FILE="JRightDiagrams", STATUS="OLD")
      READ(IDIGFILE, '(I3)') NDum
      !
  100 CONTINUE
      !
      READ(IDIGFILE, '(A10)') CDum10
      IF (CDum10(1:6) /= 'NRank=') GO TO 100
      READ(CDum10, '(A6, I4)') CDum6, NRankRead
!!!      READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
!!!      IF (TRIM(CDum) /= 'NRank=') GO TO 100
      IF (NRankRead /= NRank) GO TO 100
      !
      READ(IDIGFILE, '(I4)') NDigRead
      !     
      DO IDigRead = 1, NDigRead
         READ(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &      NDum, &
     &      M1(1, IDigRead), &
     &      M2(1, IDigRead), &
     &      M3(1, IDigRead), &
     &      M1(2, IDigRead), &
     &      M2(2, IDigRead), &
     &      M3(2, IDigRead), &
     &      M1(3, IDigRead), &
     &      M2(3, IDigRead), &
     &      M3(3, IDigRead), &
     &      M1(4, IDigRead), &
     &      M2(4, IDigRead), &
     &      M3(4, IDigRead), &
     &      M13(IDigRead), Mwp(IDigRead)
      END DO
      !
      CLOSE(IDIGFILE)
      !
      NDigJ = 0
      DO IDig = 1, NDigRead
         DO IT = 1, 4
            IF (M1(IT,IDig) < 0) THEN
               M1R = M1(IT,IDig)   ! R vertex
               M2R = M2(IT,IDig)   ! R vertex
               M3R = M3(IT,IDig)   ! R vertex
               EXIT
            END IF
         END DO
         IF (ABS(M1R) > ABS(M3R)) THEN
            NDigJ = NDigJ + 1
            DO IT = 1, 4
               M1J(IT,NDigJ) = M1(IT,IDig)
               M2J(IT,NDigJ) = M2(IT,IDig)
               M3J(IT,NDigJ) = M3(IT,IDig)
            END DO
            M13J(NDigJ) = M13(IDig)
            MwpJ(NDigJ) = Mwp(IDig)
         END IF
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CheckOrderIPR(M1, M2, M3, M13, IOut)
        !************************************************************
        ! o Check size and computational cost of intermediates for 
        !   IP-EOM diagrams
        !   Note: This version is for single ionization only
        !************************************************************
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT) :: M1(4), M2(4), M3(4)
      INTEGER, INTENT(IN) :: M13, IOUT
      !
      LOGICAL :: R1, R2, R3, R4
      INTEGER :: M1Orig(4), M2Orig(4), M3Orig(4)
      INTEGER :: NDimV, I, N1234, IT1, IT2, IT3, IT4, N1234tot, &
     &   M1T1, M2T1, M3T1, M1T2, M2T2, M3T2, M1T3, M2T3, M3T3, M1T4, M2T4, M3T4, &
     &   NString(4, 24), NDimMx(24), NClcMx(24), NumT, &
     &   NDimW4, NDimW3, NDimW2, NDimW1, NClcW4, NClcW3, NClcW2, NClcW1, &
     &   IDDim, NDim, NDimMn, NClcMn, NClc, IDClc, IDUse
      !
      ! --- Type of interaction vertex
      !
      SELECT CASE (M13)
         CASE (1, 2, 3, 12)
            NDimV = 2
         CASE (4, 5, 6, 7, 8, 9, 10, 11, 13)
            NDimV = 4
      END SELECT
      !
      ! --- Count T and R vertices
      !
      NumT = 0
      DO I = 1, 4
         IF (IABS(M1(I)) > 0) NumT = NumT + 1
      END DO
      IF (NumT <= 1) RETURN
      !
      ! --- Save Original ordering
      !
      DO I = 1, 4
         M1Orig(I) = M1(I)
         M2Orig(I) = M2(I)
         M3Orig(I) = M3(I)
      END DO
      !
      ! --- Examine (NumT)! ways of contraction (NumT = # of T and R operators)
      !
      N1234 = 0
      DO 100 IT1 = 1, NumT
         M1T1 = IABS(M1(IT1))
         M2T1 = IABS(M2(IT1))
         M3T1 = IABS(M3(IT1))
         R1 = M1(IT1) < 0
         DO 200 IT2 = 1, NumT
            IF  (IT2 == IT1) CYCLE
            M1T2 = IABS(M1(IT2))
            M2T2 = IABS(M2(IT2))
            M3T2 = IABS(M3(IT2))
            R2 = M1(IT2) < 0
            IF (NumT == 2) THEN
               N1234 = N1234 + 1
               NString(1,N1234) = IT1
               NString(2,N1234) = IT2
               NDimW2 = NDimV  + 2*M1T2 - 2*M2T2
               NClcW2 = NDimV  + 2*M1T2 - M2T2
               IF (R2) THEN
                  NDimW2 = NDimW2 - 1
                  NClcW2 = NClcW2 - 1
               END IF
               NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
               NClcW1 = NDimW2 + 2*M1T1 - M2T1
               IF (R1) THEN
                  NDimW1 = NDimW1 - 1
                  NClcW1 = NClcW1 - 1
               END IF
               NDimMx(N1234) = MAX(NDimW1, NDimW2)
               NClcMx(N1234) = MAX(NClcW1, NClcW2)
               GO TO 200
            END IF
            DO 300 IT3 = 1, NumT
               IF ((IT3 == IT1) .OR. (IT3 == IT2)) CYCLE
               M1T3 = IABS(M1(IT3))
               M2T3 = IABS(M2(IT3))
               M3T3 = IABS(M3(IT3))
               R3 = M1(IT3) < 0
               IF (NumT == 3) THEN
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NDimW3 = NDimV  + 2*M1T3 - 2*M2T3
                  NClcW3 = NDimV  + 2*M1T3 - M2T3
                  IF (R3) THEN
                     NDimW3 = NDimW3 - 1
                     NClcW3 = NClcW3 - 1
                  END IF
                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
                  NClcW2 = NDimW3 + 2*M1T2 - M2T2
                  IF (R2) THEN
                     NDimW2 = NDimW2 - 1
                     NClcW2 = NClcW2 - 1
                  END IF
                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
                  NClcW1 = NDimW2 + 2*M1T1 - M2T1
                  IF (R1) THEN
                     NDimW1 = NDimW1 - 1
                     NClcW1 = NClcW1 - 1
                  END IF
                  NDimMx(N1234) = MAX(NDimW1, NDimW2, NDimW3)
                  NClcMx(N1234) = MAX(NClcW1, NClcW2, NClcW3)
                  GO TO 300
               END IF
               DO 400 IT4 = 1, NumT
                  IF (IT4 == IT1 .OR. IT4 == IT2 .OR. IT4 == IT3) CYCLE
                  M1T4 = IABS(M1(IT4))
                  M2T4 = IABS(M2(IT4))
                  M3T4 = IABS(M3(IT4))
                  R4 = M1(IT4) < 0
                  N1234 = N1234 + 1
                  NString(1,N1234) = IT1
                  NString(2,N1234) = IT2
                  NString(3,N1234) = IT3
                  NString(4,N1234) = IT4
                  NDimW4 = NDimV  + 2*M1T4 - 2*M2T4
                  NClcW4 = NDimV  + 2*M1T4 -   M2T4
                  IF (R4) THEN
                     NDimW4 = NDimW4 - 1
                     NClcW4 = NClcW4 - 1
                  END IF
                  NDimW3 = NDimW4 + 2*M1T3 - 2*M2T3
                  NClcW3 = NDimW4 + 2*M1T3 -   M2T3
                  IF (R3) THEN
                     NDimW3 = NDimW3 - 1
                     NClcW3 = NClcW3 - 1
                  END IF
                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
                  NClcW2 = NDimW3 + 2*M1T2 -   M2T2
                  IF (R2) THEN
                     NDimW2 = NDimW2 - 1
                     NClcW2 = NClcW2 - 1
                  END IF
                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
                  NClcW1 = NDimW2 + 2*M1T1 -   M2T1
                  IF (R1) THEN
                     NDimW1 = NDimW1 - 1
                     NClcW1 = NClcW1 - 1
                  END IF
                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3,NDimW4)
                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3,NClcW4)
                  GO TO 400

 400           CONTINUE
 300        CONTINUE
 200     CONTINUE
 100  CONTINUE
      !
      N1234tot = N1234
      !
      ! --- Identify the least memory-demanding one
      !      
      NDimMn = NDimMx(1)
      IDDim = 1
      DO N1234 = 1, N1234tot
         NDim = NDimMx(N1234)
         IF (NDim < NDimMn) THEN
            NDimMn = NDim
            IDDim = N1234
         END IF
      END DO
      !
      ! --- Identify the computationally most effective one
      !
      NClcMn = NClcMx(1)
      IDClc = 1
      DO N1234 = 1, N1234tot
         NClc = NClcMx(N1234)
         IF (NClc < NClcMn) THEN
            NClcMn = NClc
            IDClc = N1234
         END IF
      END DO
      !
      IDUse = IDClc   ! Choose computationally most effective one
      !
      IF (IDUse == 1) RETURN
      !
      ! --- New ordering
      !
      IT1 = NString(1,IDUse)
      IT2 = NString(2,IDUse)
      IT3 = NString(3,IDUse)
      IT4 = NString(4,IDUse)
      !
      M1(1) = M1Orig(IT1)
      M2(1) = M2Orig(IT1)
      M3(1) = M3Orig(IT1)
      !
      M1(2) = M1Orig(IT2)
      M2(2) = M2Orig(IT2)
      M3(2) = M3Orig(IT2)
      !
      IF (NumT >= 3) THEN
         M1(3) = M1Orig(IT3)
         M2(3) = M2Orig(IT3)
         M3(3) = M3Orig(IT3)
         IF (NumT == 4) THEN
            M1(4) = M1Orig(IT4)
            M2(4) = M2Orig(IT4)
            M3(4) = M3Orig(IT4)
         END IF
      END IF
      !
      WRITE(IOUT, '("T operators reordering: ",12I2,I2," -> ",12I2,I2)') &
     &   M1Orig(1), M2Orig(1), M3Orig(1), M1Orig(2), M2Orig(2), M3Orig(2), &
     &   M1Orig(3), M2Orig(3), M3Orig(3), M1Orig(4), M2Orig(4), M3Orig(4), M13, &
     &   M1(1), M2(1), M3(1), M1(2), M2(2), M3(2), &
     &   M1(3), M2(3), M3(3), M1(4), M2(4), M3(4), M13
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!_Temp      SUBROUTINE CheckOrderTL(M1,M2,M3,M13,M1L,M2L,M3L,IOUT)
!_TempC************************************************************
!_TempC
!_TempC************************************************************
!_Temp      IMPLICIT NONE
!_TempC
!_Temp      INTEGER :: M1(4), M2(4), M3(4), M13, M1L(5), M2L(5), M3L(5)
!_TempC
!_Temp      CHARACTER*1 VerTyp(5)
!_Temp      CHARACTER*5 DigString
!_Temp      LOGICAL HaveV, HaveT, HaveL, HaveVL, HaveVT, HaveTL, 
!_Temp     *     HaveV_Only, HaveL_Only, HaveT_Only
!_Temp      INTEGER :: M1Lorig(5), M2Lorig(5), M3Lorig(5)
!_Temp      INTEGER :: NDimV,I,N1234,IT1,IT2,IT3,IT4,N1234tot, M13orig
!_Temp     *   M1T1,M2T1,M3T1, M1T2,M2T2,M3T2, M1T3,M2T3,M3T3, M1T4,M2T4,M3T4,
!_Temp     *   NString(4,24),NDimMx(24),NClcMx(24),NumT,
!_Temp     *   NDimW4,NDimW3,NDimW2,NDimW1, NClcW4,NClcW3,NClcW2,NClcW1,
!_Temp     *   IDDim,NDim,NDimMn,NClcMn,NClc,IDClc,IDuse,IOUT
!_TempC
!_TempC     --- Type of interaction vertex
!_TempC
!_Temp      SELECT CASE (M13)
!_Temp         CASE (1)
!_Temp            NumI_V = 0
!_Temp            NumA_V = 1
!_Temp            NumK_V = 0
!_Temp            NumC_V = 1
!_Temp         CASE (2)
!_Temp            NumI_V = 1
!_Temp            NumA_V = 0
!_Temp            NumK_V = 1
!_Temp            NumC_V = 0
!_Temp         CASE (3)
!_Temp            NumI_V = 0
!_Temp            NumA_V = 0
!_Temp            NumK_V = 1
!_Temp            NumC_V = 1
!_Temp         CASE (4)
!_Temp            NumI_V = 0
!_Temp            NumA_V = 2
!_Temp            NumK_V = 0
!_Temp            NumC_V = 2
!_Temp         CASE (5)
!_Temp            NumI_V = 2
!_Temp            NumA_V = 0
!_Temp            NumK_V = 2
!_Temp            NumC_V = 0
!_Temp         CASE (6)
!_Temp            NumI_V = 1
!_Temp            NumA_V = 1
!_Temp            NumK_V = 1
!_Temp            NumC_V = 1
!_Temp         CASE (7)
!_Temp            NumI_V = 0
!_Temp            NumA_V = 1
!_Temp            NumK_V = 1
!_Temp            NumC_V = 2
!_Temp         CASE (8)
!_Temp            NumI_V = 1
!_Temp            NumA_V = 0
!_Temp            NumK_V = 2
!_Temp            NumC_V = 1
!_Temp         CASE (9)
!_Temp            NumI_V = 1
!_Temp            NumA_V = 2
!_Temp            NumK_V = 0
!_Temp            NumC_V = 1
!_Temp         CASE (10)
!_Temp            NumI_V = 2
!_Temp            NumA_V = 1
!_Temp            NumK_V = 1
!_Temp            NumC_V = 0
!_Temp         CASE (11)
!_Temp            NumI_V = 0
!_Temp            NumA_V = 0
!_Temp            NumK_V = 2
!_Temp            NumC_V = 2
!_Temp         CASE (12)
!_Temp            NumI_V = 1
!_Temp            NumA_V = 1
!_Temp            NumK_V = 0
!_Temp            NumC_V = 0
!_Temp         CASE (13)
!_Temp            NumI_V = 2
!_Temp            NumA_V = 2
!_Temp            NumK_V = 0
!_Temp            NumC_V = 0
!_Temp      END SELECT
!_TempC
!_TempC     --- Initial ordering
!_TempC
!_Temp      DO IT = 1, 4
!_Temp         M1Lorig(IT) = M1(IT)
!_Temp         M2Lorig(IT) = M2(IT)
!_Temp         M3Lorig(IT) = M3(IT)
!_Temp      END DO
!_Temp      M1Lorig(5) = M13
!_Temp      M2Lorig(5) = 0
!_Temp      M3Lorig(5) = 0
!_TempC
!_TempC     --- Eliminate 000 vertices
!_TempC
!_Temp      NumVert = 0
!_Temp      DO IT = 1, 5
!_Temp         IF (M1Lorig(IT) == 0) THEN
!_Temp            M1Lorig(IT) = M1Lorig(5)
!_Temp            M2Lorig(IT) = M2Lorig(5)
!_Temp            M3Lorig(IT) = M3Lorig(5)
!_Temp            NumVert = NumVert + 1
!_Temp            EXIT
!_Temp         ELSE
!_Temp            NumVert = NumVert + 1
!_Temp         END IF
!_Temp      END DO
!_Temp      DO IT = NumVert + 1, 5
!_Temp         M1Lorig(IT) = 0
!_Temp         M2Lorig(IT) = 0
!_Temp         M3Lorig(IT) = 0
!_Temp      END DO
!_TempC
!_Temp      IF (NumVert <= 2) RETURN
!_TempC
!_TempC     --- Vertex type
!_TempC
!_Tempc      DO IT = 1, NumVert
!_Tempc         IF (M1L(IT) < 0) THEN
!_Tempc            VerTyp(IT) = "L"
!_Tempc         ELSE IF ((M2L(IT) == 0) .AND. (M3L(IT) == 0)) THEN
!_Tempc            VerTyp(IT) = "V"
!_Tempc         ELSE
!_Tempc            VerTyp(IT) = "T"
!_Tempc        END IF
!_Tempc      END DO
!_TempC
!_TempC     --- Examine all (NumVert)! possible ways of contraction (NumVert = # of vertices)
!_TempC     
!_Temp      N12345 = 0
!_Temp      HaveV = .FALSE.
!_Temp      HaveT = .FALSE.
!_Temp      HaveL = .FALSE.
!_TempC
!_TempC     --- Rightmost vertex
!_TempC         ^^^^^^^^^
!_TempC
!_Temp      DO IT1 = 1, NumVert
!_Temp         M1V1 = IABS(M1L(IT1))
!_Temp         M2V1 = IABS(M2L(IT1))
!_Temp         M3V1 = IABS(M3L(IT1))
!_TempC
!_TempC        --- Vertex type
!_TempC
!_Temp         IF (M1V1 < 0) THEN
!_Temp            CYCLE
!_Temp         ELSE IF ((M2V1 == 0) .AND. (M3V1 == 0)) THEN
!_Temp            VerTyp(1) = "V"
!_Temp            HaveV = .TRUE.
!_Temp            NumI(1) = NumI_V
!_Temp            NumA(1) = NumA_V
!_Temp            NumK(1) = NumK_V
!_Temp            NumC(1) = NumC_V
!_Temp         ELSE
!_Temp            VerTyp(1) = "T"
!_Temp            HaveT = .TRUE.
!_Temp            NumI(1) = M1V1 - (M2V1 - M3V1)
!_Temp            NumA(1) = M1V1 - M3V1
!_Temp            NumK(1) = M2V1 - M3V1
!_Temp            NumC(1) = M3V1
!_Temp         END IF
!_TempC
!_Temp         DO IT2 = 1, NumVert
!_Temp            IF  (IT2 == IT1) CYCLE
!_Temp            M1V2 = IABS(M1L(IT2))
!_Temp            M2V2 = IABS(M2L(IT2))
!_Temp            M3V2 = IABS(M3L(IT2))
!_TempC
!_TempC           --- Vertex type
!_TempC
!_Temp            IF (M1V2 < 0) THEN
!_Temp               VerTyp(2) = "L"
!_Temp               HaveL = .TRUE.
!_Temp               NumI(2) = M1V2 - NumI(1)
!_Temp               NumA(2) = M1V2 - NumA(1)
!_Temp               NumK(2) = NumK(1)
!_Temp               NumC(2) = NumC(1)
!_Temp            ELSE IF ((M2V2 == 0) .AND. (M3V2 == 0)) THEN
!_Temp               VerTyp(2) = "V"
!_Temp               HaveV = .TRUE.
!_Temp               NumI(2) = NumI(1) - NumI_V
!_Temp               NumA(2) = NumA(1) - NumA_V
!_Temp               NumK(2) = NumK_V - NumK(1)
!_Temp               NumC(2) = NumC_V - NumC(1)
!_Temp            ELSE
!_Temp               VerTyp(2) = "T"
!_Temp               HaveT = .TRUE.
!_Temp               IF (.NOT. HaveL) THEN
!_Temp                  NumI(2) = 
!_Temp                  NumA(2) = 
!_Temp                  NumK(2) = 
!_Temp                  NumC(2) = 
!_Temp               ELSE IF (.NOT. HaveV) THEN
!_Temp                  NumI(2) = NumI(1) - (M1V2 - (M2V2 - M3V2))
!_Temp                  NumA(2) = NumA(1) - (M1V2 - M3V2)
!_Temp                  NumK(2) = NumK(1) + (M1V2 - (M2V2 - M3V2))
!_Temp                  NumC(2) = NumC(1) + (M1V2 - M3V2)
!_Temp               ELSE
!_Temp                  NumI(2) = NumI(1) - (M1V2 - (M2V2 - M3V2))
!_Temp                  NumA(2) = NumA(1) - (M1V2 - M3V2)
!_Temp                  NumK(2) = NumK(1) + (M1V2 - (M2V2 - M3V2))
!_Temp                  NumC(2) = NumC(1) + (M1V2 - M3V2)
!_Temp               END IF
!_Temp            END IF
!_TempC
!_Temp            DO IT3 = 1, NumVert
!_Temp               IF (IT3 == IT1 .OR. IT3 == IT2) CYCLE
!_Temp               M1V3 = IABS(M1L(IT3))
!_Temp               M2V3 = IABS(M2L(IT3))
!_Temp               M3V3 = IABS(M3L(IT3))
!_TempC
!_TempC              --- Vertex type
!_TempC
!_Temp               IF (M1V3 < 0) THEN
!_Temp                  VerTyp(3) = "L"
!_Temp                  HaveL = .TRUE.
!_Temp               ELSE IF ((M2V3 == 0) .AND. (M3V3 == 0)) THEN
!_Temp                  VerTyp(3) = "V"
!_Temp                  HaveV = .TRUE.
!_Temp               ELSE
!_Temp                  VerTyp(3) = "T"
!_Temp                  HaveT = .TRUE.
!_Temp               END IF
!_Temp               HaveVL = HaveV .AND. HaveL
!_Temp               HaveVT = HaveV .AND. HaveT
!_Temp               HaveTL = HaveT .AND. HaveL
!_Temp               HaveV_Only = HaveV .AND. (.NOT. HaveT) .AND. 
!_Temp     *                                  (.NOT. HaveL)
!_Temp               HaveL_Only = HaveL .AND. (.NOT. HaveT) .AND. 
!_Temp     *                                  (.NOT. HaveV)
!_Temp               HaveT_Only = HaveT .AND. (.NOT. HaveV) .AND. 
!_Temp     *                                  (.NOT. HaveL)
!_TempC
!_Temp               IF (NumVert == 3) THEN
!_Temp                  N12345 = N12345 + 1
!_Temp                  NString(1,N12345) = IT1
!_Temp                  NString(2,N12345) = IT2
!_Temp                  NString(3,N12345) = IT3
!_Temp                  
!_Temp
!_Temp                  NDimW3 = NDimV  + 2*M1T3 - 2*M2T3
!_Temp                  NClcW3 = NDimV  + 2*M1T3 - M2T3
!_Temp                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
!_Temp                  NClcW2 = NDimW3 + 2*M1T2 - M2T2
!_Temp                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
!_Temp                  NClcW1 = NDimW2 + 2*M1T1 - M2T1
!_Temp                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3)
!_Temp                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3)
!_Temp                  GO TO 300
!_Temp               END IF
!_Temp
!_TempCCCCCCCCC
!_TempC
!_TempC     --- Type of interaction vertex
!_TempC
!_Temp      SELECT CASE (M13)
!_Temp         CASE (1,2,3,12)
!_Temp            NDimV = 2
!_Temp         CASE (4,5,6,7,8,9,10,11,13)
!_Temp            NDimV = 4
!_Temp      END SELECT
!_TempC
!_TempC     --- Count T and L vertices
!_TempC
!_Temp      NumT = 0
!_Temp      DO I = 1, 4
!_Temp         IF (IABS(M1(I)) > 0) NumT = NumT + 1
!_Temp      END DO
!_Temp      IF (NumT <= 1) RETURN
!_TempC
!_TempC     --- Save original ordering
!_TempC
!_Temp      DO I = 1, 4
!_Temp         M1orig(I) = M1(I)
!_Temp         M2orig(I) = M2(I)
!_Temp         M3orig(I) = M3(I)
!_Temp      END DO
!_TempC
!_TempC     --- Examine (NumT)! ways of contraction (NumT = # of T and R operators)
!_TempC
!_Temp      N1234 = 0
!_Temp      DO 100 IT1 = 1, NumT
!_Temp         M1T1 = IABS(M1(IT1))
!_Temp         M2T1 = IABS(M2(IT1))
!_Temp         M3T1 = IABS(M3(IT1))
!_Temp         DO 200 IT2 = 1, NumT
!_Temp            IF  (IT2 == IT1) CYCLE
!_Temp            M1T2 = IABS(M1(IT2))
!_Temp            M2T2 = IABS(M2(IT2))
!_Temp            M3T2 = IABS(M3(IT2))
!_Temp            IF (NumT == 2) THEN
!_Temp               N1234 = N1234 + 1
!_Temp               NString(1,N1234) = IT1
!_Temp               NString(2,N1234) = IT2
!_Temp               NDimW2 = NDimV  + 2*M1T2 - 2*M2T2
!_Temp               NClcW2 = NDimV  + 2*M1T2 - M2T2
!_Temp               NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
!_Temp               NClcW1 = NDimW2 + 2*M1T1 - M2T1
!_Temp               NDimMx(N1234) = MAX(NDimW1,NDimW2)
!_Temp               NClcMx(N1234) = MAX(NClcW1,NClcW2)
!_Temp               GO TO 200
!_Temp            END IF
!_Temp            DO 300 IT3 = 1, NumT
!_Temp               IF (IT3 == IT1 .OR. IT3 == IT2) CYCLE
!_Temp               M1T3 = IABS(M1(IT3))
!_Temp               M2T3 = IABS(M2(IT3))
!_Temp               M3T3 = IABS(M3(IT3))
!_Temp               IF (NumT == 3) THEN
!_Temp                  N1234 = N1234 + 1
!_Temp                  NString(1,N1234) = IT1
!_Temp                  NString(2,N1234) = IT2
!_Temp                  NString(3,N1234) = IT3
!_Temp                  NDimW3 = NDimV  + 2*M1T3 - 2*M2T3
!_Temp                  NClcW3 = NDimV  + 2*M1T3 - M2T3
!_Temp                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
!_Temp                  NClcW2 = NDimW3 + 2*M1T2 - M2T2
!_Temp                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
!_Temp                  NClcW1 = NDimW2 + 2*M1T1 - M2T1
!_Temp                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3)
!_Temp                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3)
!_Temp                  GO TO 300
!_Temp               END IF
!_Temp               DO 400 IT4 = 1, NumT
!_Temp                  IF (IT4 == IT1 .OR. IT4 == IT2 .OR. IT4 == IT3) CYCLE
!_Temp                  M1T4 = IABS(M1(IT4))
!_Temp                  M2T4 = IABS(M2(IT4))
!_Temp                  M3T4 = IABS(M3(IT4))
!_Temp                  N1234 = N1234 + 1
!_Temp                  NString(1,N1234) = IT1
!_Temp                  NString(2,N1234) = IT2
!_Temp                  NString(3,N1234) = IT3
!_Temp                  NString(4,N1234) = IT4
!_Temp                  NDimW4 = NDimV  + 2*M1T4 - 2*M2T4
!_Temp                  NClcW4 = NDimV  + 2*M1T4 -   M2T4
!_Temp                  NDimW3 = NDimW4 + 2*M1T3 - 2*M2T3
!_Temp                  NClcW3 = NDimW4 + 2*M1T3 -   M2T3
!_Temp                  NDimW2 = NDimW3 + 2*M1T2 - 2*M2T2
!_Temp                  NClcW2 = NDimW3 + 2*M1T2 -   M2T2
!_Temp                  NDimW1 = NDimW2 + 2*M1T1 - 2*M2T1
!_Temp                  NClcW1 = NDimW2 + 2*M1T1 -   M2T1
!_Temp                  NDimMx(N1234) = MAX(NDimW1,NDimW2,NDimW3,NDimW4)
!_Temp                  NClcMx(N1234) = MAX(NClcW1,NClcW2,NClcW3,NClcW4)
!_Temp                  GO TO 400
!_Temp
!_Temp 400           CONTINUE
!_Temp 300        CONTINUE
!_Temp 200     CONTINUE
!_Temp 100  CONTINUE
!_TempC
!_Temp      N1234tot = N1234
!_TempC
!_TempC     --- Identify the least memory-demanding one
!_TempC      
!_Temp      NDimMn = NDimMx(1)
!_Temp      IDDim = 1
!_Temp      DO N1234 = 1, N1234tot
!_Temp         NDim = NDimMx(N1234)
!_Temp         IF (NDim < NDimMn) THEN
!_Temp            NDimMn = NDim
!_Temp            IDDim = N1234
!_Temp         END IF
!_Temp      END DO
!_TempC
!_TempC     --- Identify the computationally most effective one
!_TempC
!_Temp      NClcMn = NClcMx(1)
!_Temp      IDClc = 1
!_Temp      DO N1234 = 1, N1234tot
!_Temp         NClc = NClcMx(N1234)
!_Temp         IF (NClc < NClcMn) THEN
!_Temp            NClcMn = NClc
!_Temp            IDClc = N1234
!_Temp         END IF
!_Temp      END DO
!_TempC
!_Temp      IDuse = IDClc   ! Choose computationally most effective one
!_TempC
!_Temp      IF (IDuse == 1) RETURN
!_TempC
!_TempC     --- New ordering
!_TempC
!_Temp      IT1 = NString(1,IDuse)
!_Temp      IT2 = NString(2,IDuse)
!_Temp      IT3 = NString(3,IDuse)
!_Temp      IT4 = NString(4,IDuse)
!_TempC
!_Temp      M1(1) = M1orig(IT1)
!_Temp      M2(1) = M2orig(IT1)
!_Temp      M3(1) = M3orig(IT1)
!_TempC
!_Temp      M1(2) = M1orig(IT2)
!_Temp      M2(2) = M2orig(IT2)
!_Temp      M3(2) = M3orig(IT2)
!_TempC
!_Temp      IF (NumT >= 3) THEN
!_Temp         M1(3) = M1orig(IT3)
!_Temp         M2(3) = M2orig(IT3)
!_Temp         M3(3) = M3orig(IT3)
!_Temp         IF (NumT == 4) THEN
!_Temp            M1(4) = M1orig(IT4)
!_Temp            M2(4) = M2orig(IT4)
!_Temp            M3(4) = M3orig(IT4)
!_Temp         END IF
!_Temp      END IF
!_TempC
!_Temp      WRITE(IOUT, '("T operators reordering: ", 12I2, I2," -> ", 12I2, I2)') 
!_Temp     *  M1orig(1), M2orig(1), M3orig(1), M1orig(2), M2orig(2), M3orig(2),
!_Temp     *  M1orig(3), M2orig(3), M3orig(3), M1orig(4), M2orig(4), M3orig(4), M13,
!_Temp     *  M1(1), M2(1), M3(1), M1(2), M2(2), M3(2),
!_Temp     *  M1(3), M2(3), M3(3), M1(4), M2(4), M3(4), M13
!_TempC
!_Temp      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MakeSrcArrangeV1(nC, nK, nAn, nIn, RtnName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Generate the routine which 
        ! 1. Rearranges the intermedeate Array_(CKAnIn) --> Array_(AI)
        ! 2. Add Array_ to Array(A'I')
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx, PreFixSrc
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: nC, nK, nAn, nIn
      CHARACTER(LEN=255), INTENT(OUT) :: RtnName
      !
      CHARACTER(LEN=2) a21, a22, a23, a24
      CHARACTER(LEN=255) BasName, RtnName_lw
      INTEGER :: IOUT, ICOD1, ICOD2
      INTEGER :: nA, nI, iA, iI, iAn, iIn, iC, iK
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      nA = nAn + nC
      nI = nIn + nK
      !
      ! --- Name of the SUBROUTINE
      !
      BasName = TRIM(PreFixSrc)//"ReArrangeV1"
      !
      CALL Int2Char(nC, a21, "0")
      CALL Int2Char(nK, a22, "0")
      CALL Int2Char(nAn, a23, "0")
      CALL Int2Char(nIn, a24, "0")
!!!
!      WRITE(*, *) 'nC, nK, nAn, nIn:', nC, nK, nAn, nIn
!      WRITE(*, '(A2,X,A2,X,A2,X,A2)') a21, a22, a23, a24
!!!
      RtnName = trim(BasName)//"_C"//a21//"K"//a22//"An"//a23//"In"//a24
      !
      RtnName =TRIM(RtnName)//"_AutoGen"
      !
      ! --- Convert to lower CASE for filename
      !
      RtnName_lw = RtnName
      CALL Up2Lw(RtnName_lw)
      !
      OPEN(UNIT=ICOD2, FILE=TRIM(RtnName_lw)//".f90", STATUS='UNKNOWN')
      REWIND(ICOD2)
      !
      ! --- Header
      !
      WRITE(ICOD2, '(6X, "SUBROUTINE ",$)')
      WRITE(ICOD2, *) TRIM(RtnName)//"  &"
      WRITE(ICOD2, *) "    &   "//"(V, V_, nC, nK, nAn, nIn, mC, mK, mAn, mIn, mA, mI)"
      WRITE(ICOD2, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!", /, &
     &   "!", 5X, "Re-arranges V_ array in the form (C,K,An,In)", X, &
     &   "to the form (A,I)", /, &
     &   "!", 5X, "and Add to V(A,I)", /, &
     &   "!")')
      !
      WRITE(ICOD2, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      !
      ! --- Declaration of local variables
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(6X, "IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL :: COIN", /, &
     &      6X, "COMPLEX(8):: V(mA,mI), V_(mC,mK,mAn,mIn)", /, &
     &      6X, "INTEGER:: NStringC(nC), NStringK(nK), NStringAn(nAn)", &
     &      ", NStringIn(nIn)",/, &
     &      6X, "INTEGER:: NStringA(nAn+nC), NStringI(nIn+nK)", /)')
      ELSE
         WRITE(ICOD2, '(6X,"IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL :: COIN",/, &
     &      6X, "REAL(8):: V(mA,mI), V_(mC,mK,mAn,mIn)", /, &
     &      6X, "INTEGER:: NStringC(nC), NStringK(nK), NStringAn(nAn)", &
     &      ", NStringIn(nIn)",/, &
     &      6X, "INTEGER:: NStringA(nAn+nC), NStringI(nIn+nK)", /)')
      END IF
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "nA = nAn + nC")')
      WRITE(ICOD2, '(6X, "nI = nIn + nK")')
      WRITE(ICOD2, '("!")')
      !
      ! --- Loop over In
      !
      IF (nIn /= 0) THEN
         WRITE(ICOD2, '(6X, "iIn00 = 0")')
         DO iIn = 1, nIn
            CALL Int2Char(iIn, a21, "0")
            CALL Int2Char((iIn-1), a22, "0")
            WRITE(ICOD2, '(6X, "DO ", A5, " = ", A9, ", NOc", A9, I2)') &
     &         "iIn"//a21, "iIn"//a22//" + 1", " - nIn + ", iIn
            WRITE(ICOD2, '(9X, "NStringIn(", I2, ") = ", A5)') iIn, "iIn"//a21
         END DO
         WRITE(ICOD2, '(9X, "CALL CCLib_StringAddress(NStringIn, nIn, ""Occ"", IAddIn)")')
      ELSE
         WRITE(ICOD2, '(9X, "IAddIn = 1")')
      END IF
      !
      ! --- Loop over An
      !
      IF (nAn /= 0) THEN
         WRITE(ICOD2, '(9X, "iAn00 = 0")')
         DO iAn = 1, nAn
            CALL Int2Char(iAn, a21, "0")
            CALL Int2Char((iAn-1), a22, "0")
            WRITE(ICOD2, '(9X, "DO ", A5, " = ", A9, ", NVr", A9, I2)') &
     &         "iAn"//a21, "iAn"//a22//" + 1", " - nAn + ", iAn
            WRITE(ICOD2, '(12X, "NStringAn(", I2, ") = ", A5)') iAn, "iAn"//a21
         END DO
         WRITE(ICOD2, '(12X, "CALL CCLib_StringAddress(NStringAn, nAn, ""Vir"", IAddAn)")')
      ELSE
         WRITE(ICOD2, '(12X, "IAddAn = 1")')
      END IF
      !
      ! --- Loop over K
      !
      IF (nK /= 0) THEN
         WRITE(ICOD2, '(12X, "iK00 = 0")')
         DO iK = 1, nK
            CALL Int2Char(iK, a21, "0")
            CALL Int2Char((iK-1), a22, "0")
            WRITE(ICOD2, '(12X, "DO ", A4, " = ", A8, ", NOc", A8, I2)') &
     &         "iK"//a21, "iK"//a22//" + 1", " - nK + ", iK
            WRITE(ICOD2, '(15X, "NStringK(", I2, ") = ", A4)') iK, "iK"//a21
         END DO
         WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringK, nK, ""Occ"", IAddK)")')
         !
         ! --- String K.In
         !
         IF (nIn /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string K.In", &
     &         X, 3("-"), /, "!")')
            WRITE(ICOD2, '(15X,"COIN = .FALSE.")')
            WRITE(ICOD2, &
     &         '(15X, "CALL CCLib_MergeStrings(NStringK, NStringIn," &
     &         " NStringI, nK, nIn, ISgnI, COIN)")')
            WRITE(ICOD2, '(15X,"IF (COIN) CYCLE")')
            WRITE(ICOD2, '(15X,  &
     &         "CALL CCLib_StringAddress(NStringI, nK+nIn, ""Occ"", IAddI)")')
         ELSE
            WRITE(ICOD2, '(15X, "ISgnI = 1", /, &
     &         15X, "IAddI = IAddK")')
         END IF
         !
      ELSE
         !
         WRITE(ICOD2, '(9X, "IAddK = 1")')
         !
         IF (nIn /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string K.In", X, &
     &         3("-"), /, "!")')
            WRITE(ICOD2, '(15X, "ISgnI = 1", /, &
     &         15X, "IAddI = IAddIn")')
         ELSE
            WRITE(ICOD2, '(15X, "ISgnI = 1", /, &
     &         15X, "IAddI = 1")')
         END IF
         !
      END IF
      !
      ! --- Loop over C
      !
      IF (nC /= 0) THEN
         WRITE(ICOD2, '(15X, "iC00 = 0")')
         DO iC = 1, nC
            CALL Int2Char(iC, a21, "0")
            CALL Int2Char((iC-1), a22, "0")
            WRITE(ICOD2, '(15X, "DO ", A4, " = ", A8, ", NVr", A8, I2)') &
     &         "iC"//a21, "iC"//a22//" + 1", " - nC + ", iC
            WRITE(ICOD2, '(15X, "NStringC(", I2, ") = ", A4)') iC, "iC"//a21
         END DO
         WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringC, nC, ""Vir"", IAddC)")')
         !
         ! --- String C.An
         !
         IF (nAn /= 0) THEN
            WRITE(ICOD2, '("!", /, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string C.An", &
     &         X, 3("-"), /, "!")')
            WRITE(ICOD2, '(18X, "COIN = .FALSE.")')
            WRITE(ICOD2, &
     &         '(18X, "CALL CCLib_MergeStrings(NStringC, NStringAn," &
     &         " NStringA, nC, nAn, ISgnA, COIN)")')
            WRITE(ICOD2, '(18X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '(18X, &
     &         "CALL CCLib_StringAddress(NStringA, nC+nAn, ""Vir"", IAddA)")')
         ELSE
            WRITE(ICOD2, '(18X, "ISgnA = 1", /, &
     &         18X, "IAddA = IAddC")')
         END IF
         !
      ELSE
         !
         WRITE(ICOD2, '(18X, "IAddC = 1")')
         !
         IF (nAn /= 0) THEN
            WRITE(ICOD2, '("!",/, &
     &         "!", 5X, 3("-"), X, "Get sign and Address of string C.An", X, &
     &         3("-"), /, "!")')
            WRITE(ICOD2, '(18X, "ISgnA = 1", /, &
     &         18X, "IAddA = IAddAn")')
         ELSE
            WRITE(ICOD2, '(18X, "ISgnA = 1", /, &
     &         18X, "IAddA = 1")')
         END IF
         !
      END IF
      !
      ! --- Rearrange the intermediate
      !
      WRITE(ICOD2, '("!", /, &
     &   "!", 5X, 3("-"), X, "Rearrange the intermediate", X, &
     &   3("-"), /, "!")')
      IF (Complx) THEN
         WRITE(ICOD2, '(18X, &
     &      "V(IAddA,IAddI) = V(IAddA,IAddI) + V_(IAddC,IAddK,IAddAn,IAddIn) *", &
     &      " DCMPLX(ISgnA * ISgnI)")')
      ELSE
         WRITE(ICOD2, '(18X, &
     &      "V(IAddA,IAddI) = V(IAddA,IAddI) + V_(IAddC,IAddK,IAddAn,IAddIn) *", &
     &      " DBLE(ISgnA * ISgnI)")')
      END IF
      !
      ! --- Close loops
      !
      DO iC = nC, 1, -1
         IF (iC == nC) THEN
            WRITE(ICOD2, '(15X, "END DO   ! Internal particles")')
         ELSE
            WRITE(ICOD2, '(15X, "END DO")')
         END IF
      END DO
      DO iK = nK, 1, -1
         IF (iK == nK) THEN
            WRITE(ICOD2, '(12X, "END DO   ! Internal holes")')
         ELSE
            WRITE(ICOD2, '(12X, "END DO")')
         END IF
      END DO
      DO iAn = 1,nAn
         IF (iAn == 1) THEN
            WRITE(ICOD2, '(9X, "END DO   ! External particles")')
         ELSE
            WRITE(ICOD2, '(9X, "END DO")')
         END IF
      END DO
      DO iIn = 1, nIn
         IF (iIn == 1) THEN
            WRITE(ICOD2, '(6X, "END DO   ! External holes")')
         ELSE
            WRITE(ICOD2, '(6X, "END DO")')
         END IF
      END DO
      !
      ! --- End of the SUBROUTINE
      !
      WRITE(ICOD2, '("!",/, 6X, "END SUBROUTINE")')
      !
      CLOSE(ICOD2)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveParentDiagrams(NDig, M1, M2, M3, M13, Mwp)
      !
      USE BasicInformation, ONLY : NMax, NRank
      !
      IMPLICIT NONE
      !
      INTEGER :: NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      !     o Save parent diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
      ELSE   ! NRank = NMax
         OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="OLD")   ! ParentDiagram file
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_GS(NDig, M1, M2, M3, M13, Mwp)
      !
      USE BasicInformation, ONLY : NMax, NRank, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      !     o Generate programs for ground-state CC amplitude equations
      !
      IMPLICIT NONE
      !
      INTEGER :: NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=255) :: Name
      INTEGER :: IDig, I
      INTEGER :: IOUT, ICOD
      INTEGER :: MPhase, MWeight, NumT
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Generating programs for CC amplitude ...")')
      !
      ! --- Name of SUBROUTINE
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
      END SELECT
      SELECT CASE (NRank)
         CASE (1)
            Name = TRIM(Name)//"_t1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_t2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_t3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_t4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_t5_autogen.f90"
      END SELECT
      !
      IF (Complx) THEN
         PreFixFile = "socc_"
      ELSE
         PreFixFile = "cc_"
      END IF
      Name = TRIM(PreFixFile)//Name
      !
      ! --- Open log file and program file
      !
      OPEN(UNIT=IOUT, FILE='CCeq_log', STATUS='UNKNOWN')   ! log file
      OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
      !
      ! --- Subrutine header
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
      END SELECT
      !
      IF (Complx) THEN
         PreFixSrc = "SOCC_"
      ELSE
         PreFixSrc = "CC_"
      END IF
      Name = TRIM(PreFixSrc)//Name
      !
      WRITE(c1, '(I1)') NRank
      Name = TRIM(Name)//"_T"//TRIM(c1)//"_AutoGen"
      !
      ! --- Header and Modules
      !
      WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
      WRITE(ICOD, *) TRIM(Name)
      WRITE(ICOD, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program",/, &
     &   "!", /, &
     &   6X, "USE CC_Module, ONLY : NMax, NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD, '( &
     &      6X, "USE CC_Constant_Module, ONLY : OneC, Half")')
      ELSE
         WRITE(ICOD, '( &
     &      6X, "USE CC_Constant_Module, ONLY : One, Half")')
      END IF
      WRITE(ICOD, '( &
     &   "!", /, &
     &   6X, "IMPLICIT NONE",/, &
     &   "!")')
      !
      ! --- Large arrays
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "COMPLEX(8), ALLOCATABLE :: V1(:), T(:), T_(:), G(:), G_(:)")')
      ELSE
         WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "REAL(8), ALLOCATABLE :: V1(:), T(:), T_(:), G(:), G_(:)")')
      END IF
      !
      ! --- Constants
      !
!      WRITE(ICOD, '(6X,"REAL(8), PARAMETER ::",X,
!     *   "Zero = 0.0D+00, Half = 0.5D+00")')
      !
      ! --- Local variables
      !
      CALL Declaration(ICOD, 'MAIN', Complx)
      !
      ! --- Diagram generation
      !
      WRITE(IOUT, '(/, X, "Generate the diagrams in CC(", I2, ")-T(", I2, ") eq.:")') NMax, NRank
      CALL GenDiag(MaxDig, NDig, NMax, NRank, M1, M2, M3, M13, NoT1, MaxOrder, OrderCut)
      !
      ! --- Rearrange the diagrams in the descending order
      !
      CALL Descend(NDig, M1, M2, M3)
      !
      ! --- Check size of intermediates; change T vertex ordering if neccessary
      !
      DO IDig = 1, NDig
         CALL CheckOrderT(M1(1,IDig), M2(1,IDig), M3(1,IDig), M13(IDig), IOUT)
      END DO
      !
      ! --- Sort the diagrams for factorization
      !
      WRITE(IOUT, '(/, X, "Sort diagrams:")')
      CALL SortDiag(NDig, M1, M2, M3, M13)
      !
      ! --- Determine weight and phase factors of each diagram
      !
      WRITE(IOUT, '(/, X, "Weight and phase factors:")')
      DO IDig = 1, NDig
!         CALL Define_Phase(MaxOp, NRank, M1(1:4,IDig), M2(1:4,IDig), M3(1:4,IDig), M13(IDig), &
!     &      MPhase, IOUT)
         CALL Define_Phase2(MaxOp, NRank, M1(1:4,IDig), M2(1:4,IDig), M3(1:4,IDig), M13(IDig), &
     &      'EXCT', MPhase, IOUT)
         CALL Define_Weight(M1(1:4,IDig), M2(1:4,IDig), M3(1:4,IDig), MWeight)
         MwP(IDig) = MPhase * MWeight
      END DO
!      CALL Wgt_Phse(M1, M2, M3, M13, Mwp, NDig)
      !
      ! --- Punch out the diagrams
      !
      WRITE(IOUT, '(/, "Generated diagrams")')
      DO I = 1, NDig
         WRITE(IOUT, '(X, I3, X, 12I1, I2, X, I3)') &
     &      I, &
     &      M1(1,I), M2(1,I), M3(1,I), &
     &      M1(2,I), M2(2,I), M3(2,I), &
     &      M1(3,I), M2(3,I), M3(3,I), &
     &      M1(4,I), M2(4,I), M3(4,I), &
     &      M13(I), Mwp(I)
      END DO
      !
      ! --- Save parent diagrams for later use
      !
      CALL SaveParentDiagrams(NDig, M1, M2, M3, M13, Mwp)
      !
      ! --- Declare misc. arrays
      !
      WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDig
      !
      ! --- Declare CCLib_NComb function
      !
      WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
      !
      ! --- Initialize weight and phase factors
      !
      WRITE(ICOD, '("!", /, "!", 5X, "o Weight and phase factors", /, "!")')
      DO IDig = 1, NDig
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, 3I2, "/", &
     &      3I2, "/", 3I2, "/", 3I2, I3)') &
     &      IDig, Mwp(IDig), &
     &      M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &      M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &      M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &      M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &      M13(IDig)
      END DO
      !
      ! --- Factorization of diagrams
      ! --- Start to generate the program
      !
      WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, ")-T(", I2, &
     &     ") equation", X, 5("-"), /, "!")') NMax, NRank
      !
      ! --- Make arrays for string addresses
      !
      WRITE(ICOD, '("!", /, "!     o Prepare arc weight arrays", /, &
     &     "!", /, 6X, "CALL CCLib_InitArcWgt", /)')
      !
      ! --- Factorize the diagrams and generate the main frame of program
      !
      CALL GenFactorize(NRank, NDig, M1, M2, M3, M13, Mwp, Facto, Complx)
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, 5("-"), X, "End of residue calculation", X, &
     &   5("-"), /, "!")')
      !
      WRITE(ICOD, '(6X, "END SUBROUTINE")')
      !
      CLOSE(IOUT)
      CLOSE(ICOD)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_JRight(NDig, M1, M2, M3, M13, Mwp, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      USE BasicInformation, ONLY : NMax, NRank, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      !     o Generate programs for CC Jacobian right eigenequations
      !
      INTEGER :: NDig, NDigJ
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*),  M1J(4,*), M2J(4,*), M3J(4,*), M13J(*), MwpJ(*)
      !
      CHARACTER(LEN=255) :: Name
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=2) :: c2
      INTEGER :: IDig, I
      INTEGER :: MPhase, MWeight, NumT
      INTEGER :: ICOD, IOUT
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Generating programs for CC Jacobian ...")')
      !
      ! --- Name of SUBROUTINE
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
      END SELECT
      Name = TRIM(Name)//"_jacobr"
      SELECT CASE (NRank)
         CASE (1)
            Name = TRIM(Name)//"_bra1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_bra2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_bra3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_bra4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_bra5_autogen.f90"
      END SELECT
      !
      IF (Complx) THEN
         PreFixFile = "socclr_"
      ELSE
         PreFixFile = "cclr_"
      END IF
      Name = TRIM(PreFixFile)//Name
      !
      ! --- Open log file and program file
      !
      CALL Int2Char(NRank, c2, "0")
      OPEN(UNIT=IOUT, FILE='CC_JacobR'//c2//'_log', STATUS='UNKNOWN')   ! log file
      OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
      !
      ! --- Generate diagrams
      !
      CALL GenDiagJacobian(NDig, NMax, NRank, M1, M2, M3, M13, Mwp, &
     &     NDigJ, M1J, M2J, M3J, M13J, MwpJ, .FALSE.)
      !
      ! --- Check size of intermediates
      !
      DO IDig = 1, NDigJ
         CALL CheckOrderTR(M1J(1,IDig), M2J(1,IDig), M3J(1,IDig), M13J(IDig), IOUT)
      END DO
      !
      ! --- Sort diagrams according to R operators
      !
!!!      CALL SortDiagJacobian(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      CALL SortDiagGen(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Weight and phase factors
      !
      DO IDig = 1, NDigJ
!         CALL Define_Phase(MaxOp, NRank, M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), M13J(IDig), &
!     &      MPhase, IOUT)
         CALL Define_Phase2(MaxOp, NRank, M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), M13J(IDig), &
     &      'EXCT', MPhase, IOUT)
         CALL Define_Weight(M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), MWeight)
         MwPJ(IDig) = MPhase * MWeight
      END DO
!      CALL Wgt_Phse(M1J, M2J, M3J, M13J, MwpJ, NDigJ)
      !
      ! --- Save Jacobian right diagrams
      !
      CALL SaveJRightDiagrams(NMax, NRank, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Punch out the Jacobian diagrams
      !
      WRITE(IOUT, '("Generated Diagrams and Weight Factors")')
      DO I = 1, NDigJ
         WRITE(IOUT, '(X, I3, X, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, 3X, I2)') &
     &      I, &
     &      M1J(1,I), M2J(1,I), M3J(1,I), &
     &      M1J(2,I), M2J(2,I), M3J(2,I), &
     &      M1J(3,I), M2J(3,I), M3J(3,I), &
     &      M1J(4,I), M2J(4,I), M3J(4,I), &
     &      M13J(I), &
     &      MwpJ(I)
      END DO
      !
      !     o Generate programs
      !
      ! --- Subroutine header
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
      END SELECT
      !
      Name = TRIM(Name)//"_JacobR"
      !
      WRITE(c1, '(I1)') NRank
      Name = TRIM(Name)//"_Bra"//TRIM(c1)//"_AutoGen"
      !
      IF (Complx) THEN
         PreFixSrc = "SOCCLR_"
      ELSE
         PreFixSrc = "CCLR_"
      END IF
      Name = TRIM(PreFixSrc)//Name
      !
      ! --- Print header
      !
      WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
      WRITE(ICOD, *) TRIM(Name)
      WRITE(ICOD, '("!", /, "!", 5X, "o This is an automatically generated program", /, "!")')
      !
      ! --- Modules
      !
      WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
      ELSE
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
      END IF
      WRITE(ICOD, '("!", /, 6X, "IMPLICIT NONE",/, "!")')
      !
      ! --- Large arrays
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "COMPLEX(8), ALLOCATABLE :: V1(:), T(:), T_(:), R(:), R_(:), G(:), G_(:)")')
      ELSE
         WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "REAL(8), ALLOCATABLE :: V1(:), T(:), T_(:), R(:), R_(:), G(:), G_(:)")')
      END IF
      !
      ! --- Local variables
      !
      CALL Declaration(ICOD, 'MAIN', Complx)
      !
      ! --- Declare misc. arrays
      !
      WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigJ
      !
      ! --- Declare CCLib_NComb function
      !
      WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
      !
      ! --- Initialize weight and phase factors
      !
      WRITE(ICOD, '("!", /, &
     &     "!", 5X, "o Weight and phase factors", /, &
     &     "!")')
      DO IDig = 1, NDigJ
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &      3I2, "/", 3I2, "/", 3I2, "/", 3I2, I3)') &
     &      IDig, MwpJ(IDig), &
     &      M1J(1,IDig), M2J(1,IDig), M3J(1,IDig), &
     &      M1J(2,IDig), M2J(2,IDig), M3J(2,IDig), &
     &      M1J(3,IDig), M2J(3,IDig), M3J(3,IDig), &
     &      M1J(4,IDig), M2J(4,IDig), M3J(4,IDig), &
     &      M13J(IDig)
      END DO
      !
      ! --- Factorization of diagrams
      ! --- Start to generate the code
      !
      WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, ")-T(", I2, &
     &     ") equation", X, 5("-"), /, "!")') NMax, NRank
      !
      ! --- Make arrays for string Addresses
      !
      WRITE(ICOD, '("!", /, &
     &     "!     o Prepare arc weight arrays", /, &
     &     "!", /, &
     &     6X, "CALL CCLib_InitArcWgt", /)')
      !
      ! --- Factorize the diagrams and generate the main frame of program
      !
      CALL GenFactorize(NRank, NDigJ, M1J, M2J, M3J, M13J, MwpJ, Facto, Complx)
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, 5("-"), X, "End of Jacobian calculation", X, &
     &   5("-"), /, "!")')
      !
      WRITE(ICOD, '(6X, "END SUBROUTINE")')
      !
      CLOSE(IOUT)
      CLOSE(ICOD)
      !
      WRITE(*, '("Generation of Jacobian Right programss done.")')
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_JDiagonal(NDig, M1, M2, M3, M13, Mwp, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
        !
      USE BasicInformation, ONLY : NMax, NRank, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto
      !
      INTEGER :: NDig, NDigJ
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), M1J(4,*), M2J(4,*), M3J(4,*), M13J(*)
      !
      CHARACTER(LEN=255) :: Name
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=2) :: c2
      INTEGER :: IDig, I
      !
      COMMON/IOFILE/IOUT, ICOD
      
      WRITE(*, '("Generating programs for CC Jacobian diagonal elements ...")')
      !
      ! --- Name of SUBROUTINE
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
      END SELECT
      Name = TRIM(Name)//"_jacobdiag"
      SELECT CASE (NRank)
         CASE (1)
            Name = TRIM(Name)//"_bra1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_bra2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_bra3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_bra4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_bra5_autogen.f90"
      END SELECT
      !
      IF (Complx) THEN
         PreFixFile = "socclr_"
      ELSE
         PreFixFile = "cclr_"
      END IF
      Name = TRIM(PreFixFile)//Name
      !
      ! --- Open log file and program file
      !
      CALL Int2Char(NRank, c2, "0")
      OPEN(UNIT=IOUT, FILE='CC_JacobDiag'//c2//'_log', STATUS='UNKNOWN')   ! log file
      OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
      !
      ! --- Generate diagrams (phase factor is determined simultaneously)
      !
      CALL GenDiagJacobian(NDig, NMax, NRank, M1, M2, M3, M13, Mwp, &
     &     NDigJ, M1J, M2J, M3J, M13J, MwpJ, .TRUE.)
      !
      ! --- Sort diagrams according to R operators
      !
!!!      CALL SortDiagJacobian(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      CALL SortDiagGen(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Weight and phase factors
      !
      CALL Wgt_Phse(M1J, M2J, M3J, M13J, MwpJ, NDigJ)
      !
      ! --- Check size of intermediates
      !
      DO IDig = 1, NDigJ
!!!         CALL CheckOrderTR(M1J(1,IDig), M2J(1,IDig), M3J(1,IDig), M13(IDig), NRank)
      END DO
      !
      ! --- Punch out the Jacobian diagrams
      !
      WRITE(IOUT, '("Generated Diagrams and Weight Factors")')
      DO I = 1, NDigJ
         WRITE(IOUT, '(X, I3, X, 3I2, X, 3I1, X, 3I1, X, 3I1, X, &
     &      I2, 3X, I2)') &
     &      I, &
     &      M1J(1,I), M2J(1,I), M3J(1,I), &
     &      M1J(2,I), M2J(2,I), M3J(2,I), &
     &      M1J(3,I), M2J(3,I), M3J(3,I), &
     &      M1J(4,I), M2J(4,I), M3J(4,I), &
     &      M13J(I), MwpJ(I)
      END DO
      !
      !     o Generate programs
      !
      ! --- Subroutine header
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
      END SELECT
      !
      Name = TRIM(Name)//"_JacobDiag"
      !
      WRITE(c1, '(I1)')NRank
      Name = TRIM(Name)//"_Bra"//TRIM(c1)//"_AutoGen"
      !
      IF (Complx) THEN
         PreFixSrc = "SOCCLR_"
      ELSE
         PreFixSrc = "CCLR_"
      END IF
      Name = TRIM(PreFixSrc)//Name
      !
      ! --- Print header
      !
      WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
      WRITE(ICOD, *) TRIM(Name)
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!")')
      !
      ! --- Modules
      !
      WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
      ELSE
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
      END IF
      WRITE(ICOD, '( &
     &   "!", /, &
     &   6X, "IMPLICIT NONE",/, &
     &   "!")')
      !
      ! --- Large arrays
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &      "COMPLEX(8), ALLOCATABLE :: V1(:), T(:), R(:), G(:), G_(:)")')
      ELSE
         WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &      "REAL(8), ALLOCATABLE :: V1(:), T(:), R(:), G(:), G_(:)")')
      END IF
      !
      ! --- Local variables
      !
      CALL Declaration(ICOD, 'MAIN', Complx)
      !
      ! --- Declare misc. arrays
      !
      WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigJ
      !
      ! --- Declare CCLib_NComb function
      !
      WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
      !
      ! --- Initialize weight and phase factors
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, "o Weight and phase factors", /, &
     &   "!")')
      DO IDig = 1, NDigJ
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5)') IDig, MwpJ(IDig)
      END DO
      !
      ! --- Factorization of diagrams
      ! --- Start to generate the code
      !
      WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, ")-T(", I2, &
     &   ") equation", X, 5("-"), /, "!")') NMax, NRank
      !
      ! --- Make arrays for string Addresses
      !
      WRITE(ICOD, '("!", /, &
     &   "!     o Prepare arc weight arrays", /, &
     &   "!", /, &
     &   6X, "CALL CCLib_InitArcWgt", /)')
      !
      ! --- Factorize the diagrams and generate the main frame of program
      !
      CALL GenFactorize(NRank, NDigJ, M1J, M2J, M3J, M13J, MwpJ, Facto, Complx)
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, 5("-"), X, "End of Jacobian calculation", X, &
     &   5("-"), /, "!")')
      !
      WRITE(ICOD, '(6X, "END SUBROUTINE")')
      !
      CLOSE(IOUT)
      CLOSE(ICOD)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_L(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
        !
      USE BasicInformation, ONLY : NMax, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      INTEGER :: NDigL
      INTEGER :: M1P(4,*), M2P(4,*), M3P(4,*), M13P(*), M1L(4,*), M2L(4,*), M3L(4,*), M13L(*), MwpL(*)
      !
      CHARACTER(LEN=255) :: Name, ANameL
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=2) :: c2
      LOGICAL :: Constant
      INTEGER, PARAMETER :: IDIGFILE = 99
      INTEGER :: IAdd, NDigTotal, NRankRead, NDigRead, IDigRead, IDum, NRank, NRankL, I, IDig, IT
      INTEGER :: IOUT, ICOD
      INTEGER :: NRankParent(MaxDig)
      INTEGER :: LArray(3,MaxDig)
      INTEGER :: NDum, NumT, L1
      INTEGER :: MPhase, MWeight
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Start generating Lambda equations")')
      !
      ! --- Read parent diagrams
      !
      OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="OLD")
      READ(IDIGFILE, '(I3)') NDum
      !
      IAdd = 0
      NDigTotal = 0
      DO NRank = 1, NMax
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !     
         DO IDigRead = 1, NDigRead
            READ(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2)') &
     &         IDum, &
     &         M1P(1, IDigRead+IAdd), &
     &         M2P(1, IDigRead+IAdd), &
     &         M3P(1, IDigRead+IAdd), &
     &         M1P(2, IDigRead+IAdd), &
     &         M2P(2, IDigRead+IAdd), &
     &         M3P(2, IDigRead+IAdd), &
     &         M1P(3, IDigRead+IAdd), &
     &         M2P(3, IDigRead+IAdd), &
     &         M3P(3, IDigRead+IAdd), &
     &         M1P(4, IDigRead+IAdd), &
     &         M2P(4, IDigRead+IAdd), &
     &         M3P(4, IDigRead+IAdd), &
     &         M13P(IDigRead+IAdd)
            NRankParent(IDigRead+IAdd) = NRankRead
            NDigTotal = NDigTotal + 1
         END DO
         IAdd = IAdd + NDigRead
      END DO
      !
      CLOSE(IDIGFILE)
      !     
      WRITE(*, '("Total # of parent diagrams = ", I6)') NDigTotal
      !     
      ! --- Loop over NRankL
      !     
      DO NRankL = 1, NMax
         !     
!         IF ((NMax == 2) .AND. NoT1 .AND. (NRankL == 1)) CYCLE   ! CCD
         !     
         WRITE(*, '("Generating programs for CC Lambda vectors for NRankL =", &
     &      I3, " ...")') NRankL
         !     
         ! --- Name of SUBROUTINE
         !
         SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
         END SELECT
         Name = TRIM(Name)//"_lambda"
         SELECT CASE (NRankL)
         CASE (1)
            Name = TRIM(Name)//"_ket1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_ket2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_ket3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_ket4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_ket5_autogen.f90"
         END SELECT
         !     
         IF (Complx) THEN
            PreFixFile = "socclr_"
         ELSE
            PreFixFile = "cclr_"
         END IF
         Name = TRIM(PreFixFile)//Name
         !     
         ! --- Open log file and program file
         !     
         CALL Int2Char(NRankL, c2, "0")
         OPEN(UNIT=IOUT, FILE='CC_Lambda'//c2//'_log', STATUS='UNKNOWN')   ! log file
         OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
         !     
         ! --- Pareparation for Lambda diagrams generation
         ! * The resulting diagrams include the removed T vertices at the leftmost position
         ! * Lambda vertices are saved in LArray(1:3,1:NDigL)
         ! --- For the case where the closing vertex = 1:
         ! * LArray contains (0,0,0)
         ! * The removd diagram will be replaced by a T vertex with no external lines
         !     
         Constant = .TRUE.   ! Lambda operator involves a constant term (One)
         CALL GenDiagLambdaPre(NDigTotal, NMax, NRankParent, NRankL, Constant, &
     &        M1P, M2P, M3P, M13P, NDigL, LArray, M1L, M2L, M3L, M13L)
         !     
         ! --- Check size of intermediates
         !     
         DO IDig = 1, NDigL
!_Temp            CALL CheckOrderTL(M1L(1,IDig),M2L(1,IDig),M3L(1,IDig),
!_TEmp     *           M13L(IDig),NRankL)
         END DO
         !     
         ! --- Weight and phase factors
         !     
         WRITE(IOUT, '(/, X, "Weight and phase factors:")')
         DO IDig = 1, NDigL
            L1 = LArray(1,IDig)
            CALL Define_Phase(MaxOp, L1, M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), M13L(IDig), &
     &         MPhase, IOUT)
            !
            ! --- Replace the marked T vertex by Lambda vertex
            ! IF there is no de-excitation, shift T vertices to the left
            !
!            IF (LArray(1,IDig) /= 0) THEN
            M1L(1,IDig) = LArray(1,IDig)
            M2L(1,IDig) = LArray(2,IDig)
            M3L(1,IDig) = LArray(3,IDig)
!            ELSE
!               DO IT = 2, 4
!                  M1L(IT-1,IDig) = M1L(IT,IDig)
!                  M2L(IT-1,IDig) = M2L(IT,IDig)
!                  M3L(IT-1,IDig) = M3L(IT,IDig)
!               END DO
!               M1L(4,IDig) = 0
!               M2L(4,IDig) = 0
!               M3L(4,IDig) = 0
!            END IF
            !
            CALL Define_Weight(M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), MWeight, IOUT)
            MwpL(IDig) = MPhase * MWeight
         END DO
         !     
         ! --- Sort diagrams for factorization
         !     
!!!         CALL SortDiagJacobian(NDigL, M1L, M2L, M3L, M13L, MwpL)
         CALL SortDiagGen(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !
         ! --- Eliminate redundancy
         !
         CALL CheckRedundancy(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! --- Punch out the Lambda Diagrams
         !     
         WRITE(IOUT, '("Generated Lambda Diagrams and Weight Factors")')
         DO I = 1, NDigL
            WRITE(IOUT, '(X, I3, X, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, 3X, I2)') &
     &         I, &
     &         M1L(1,I), M2L(1,I), M3L(1,I), &
     &         M1L(2,I), M2L(2,I), M3L(2,I), &
     &         M1L(3,I), M2L(3,I), M3L(3,I), &
     &         M1L(4,I), M2L(4,I), M3L(4,I), &
     &         M13L(I), MwpL(I)
         END DO
         !
         ! o Save Lambda diagrams
         !
         CALL SaveLambdaDiagrams(NMax, NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! o Generate programs
         !     
         ! --- Subroutine Name
         !     
         SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
         END SELECT
         !     
         Name = TRIM(Name)//"_Lambda"
         !     
         WRITE(c1, '(I1)') NRankL
         Name = TRIM(Name)//"_Ket"//TRIM(c1)//"_AutoGen"
         !     
         IF (Complx) THEN
            PreFixSrc = "SOCCLR_"
         ELSE
            PreFixSrc = "CCLR_"
         END IF
         Name = TRIM(PreFixSrc)//Name
         !     
         ! --- Print header
         !     
         WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
         WRITE(ICOD, *) TRIM(Name)//"(AmpFileT, AmpFileR, AmpFileL)"   ! Name of amplitude files
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o This is an automatically generated program", /, &
     &      "!")')
         !     
         ! --- Modules
         !     
         WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
         ELSE
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
         END IF
         WRITE(ICOD, '( &
     &      "!", /, &
     &      6X, "IMPLICIT NONE", /, &
     &      "!")')
         !
         ! --- Input variable
         !
         WRITE(ICOD, '(6X, "CHARACTER(*), INTENT(IN) :: AmpFileT, AmpFileR, AmpFileL")')
         WRITE(ICOD, '("!")')
         !     
         ! --- Large arrays
         !     
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "COMPLEX(8), ALLOCATABLE :: V1(:), V1_(:), T(:), L(:), G(:), G_(:)")')
         ELSE
            WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "REAL(8), ALLOCATABLE :: V1(:), V1_(:), T(:), L(:), G(:), G_(:)")')
         END IF
         !
         ! --- Local variables
         !
         CALL Declaration(ICOD, 'LAMBDA', Complx)
         !     
         ! --- Declare misc. arrays
         !    
         WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigL
         !     
         ! --- Declare CCLib_NComb function
         !     
         WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
         !     
         ! --- Initialize weight and phase factors
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o Weight and phase factors", /, &
     &      "!")')
      DO IDig = 1, NDigL
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &      3I2, "/", 3I2, "/", 3I2, "/", 3I2, I3)') &
     &      IDig, MwpL(IDig), &
     &      M1L(1,IDig), M2L(1,IDig), M3L(1,IDig), &
     &      M1L(2,IDig), M2L(2,IDig), M3L(2,IDig), &
     &      M1L(3,IDig), M2L(3,IDig), M3L(3,IDig), &
     &      M1L(4,IDig), M2L(4,IDig), M3L(4,IDig), &
     &      M13L(IDig)
      END DO
      !     
      ! --- Factorization of diagrams
      ! --- Start to generate the code
      !     
         WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, &
     &      ")-Lambda(", I2, ") equation", X, 5("-"), /, "!")') &
     &      NMax, NRankL
         !     
         ! --- Make arrays for string Addresses
         !     
         WRITE(ICOD, '("!", /, &
     &      "!     o Prepare arc weight arrays", /, &
     &      "!", /, &
     &      6X, "CALL CCLib_InitArcWgt", /)')
         !     
         ! --- Factorize the diagrams and generate the main frame of program
         !     
         ANameL = 'Lambda'
!         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx, TRIM(ANameL))
         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx)
         !     
         WRITE(ICOD, '("        	!", /, &
     &      "	!", 5X, 5("-"), X, "End of Lambda calculation", X, &
     &      5("-"), /, "	!")')
         !     
         WRITE(ICOD, '(6X, "END SUBROUTINE")')
         !     
         CLOSE(IOUT)
         CLOSE(ICOD)
         !
         ! --- End generating Lambda programs
         !
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_JLeft(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
        !
      USE BasicInformation, ONLY : NMax, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      INTEGER :: NDigL
      INTEGER :: M1P(4,*), M2P(4,*), M3P(4,*), M13P(*), M1L(4,*), M2L(4,*), M3L(4,*), M13L(*), MwpL(*)
      !
      CHARACTER(LEN=255) :: Name, ANameL
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=2) :: c2
      LOGICAL :: Constant
      INTEGER, PARAMETER :: IDIGFILE = 99
      INTEGER :: IAdd, NDigTotal, NRankRead, NDigRead, IDigRead, IDum, NRank, NRankL, I, IDig, IT
      INTEGER :: IOUT, ICOD
      INTEGER :: NRankParent(MaxDig)
      INTEGER :: LArray(3,MaxDig)
      INTEGER :: NDum, NumT, L1
      INTEGER :: MPhase, MWeight
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Start generating Jacobian Left equations")')
      !
      ! --- Read parent diagrams
      !
      OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="OLD")
      READ(IDIGFILE, '(I3)') NDum
      !
      IAdd = 0
      NDigTotal = 0
      DO NRank = 1, NMax
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !     
         DO IDigRead = 1, NDigRead
            READ(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2)') &
     &         IDum, &
     &         M1P(1, IDigRead+IAdd), &
     &         M2P(1, IDigRead+IAdd), &
     &         M3P(1, IDigRead+IAdd), &
     &         M1P(2, IDigRead+IAdd), &
     &         M2P(2, IDigRead+IAdd), &
     &         M3P(2, IDigRead+IAdd), &
     &         M1P(3, IDigRead+IAdd), &
     &         M2P(3, IDigRead+IAdd), &
     &         M3P(3, IDigRead+IAdd), &
     &         M1P(4, IDigRead+IAdd), &
     &         M2P(4, IDigRead+IAdd), &
     &         M3P(4, IDigRead+IAdd), &
     &         M13P(IDigRead+IAdd)
            NRankParent(IDigRead+IAdd) = NRankRead
            NDigTotal = NDigTotal + 1
         END DO
         IAdd = IAdd + NDigRead
      END DO
      !
      CLOSE(IDIGFILE)
      !     
      WRITE(*, '("Total # of parent diagrams = ", I6)') NDigTotal
      !     
      ! --- Loop over NRankL
      !     
      DO NRankL = 1, NMax
         !     
!         IF ((NMax == 2) .AND. NoT1 .AND. (NRankL == 1)) CYCLE   ! CCD
         !     
         WRITE(*, '("Generating programs for CC Jacobian left eigenvectors for NRankL =", &
     &      I3, " ...")') NRankL
         !     
         ! --- Name of SUBROUTINE
         !
         SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
         END SELECT
         Name = TRIM(Name)//"_jacobl"
         SELECT CASE (NRankL)
         CASE (1)
            Name = TRIM(Name)//"_ket1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_ket2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_ket3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_ket4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_ket5_autogen.f90"
         END SELECT
         !     
         IF (Complx) THEN
            PreFixFile = "socclr_"
         ELSE
            PreFixFile = "cclr_"
         END IF
         Name = TRIM(PreFixFile)//Name
         !     
         ! --- Open log file and program file
         !     
         CALL Int2Char(NRankL, c2, "0")
         OPEN(UNIT=IOUT, FILE='CC_Lambda'//c2//'_log', STATUS='UNKNOWN')   ! log file
         OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
         !     
         ! --- Pareparation for Lambda diagrams generation
         ! * The resulting diagrams include the removed T vertices at the leftmost position
         ! * Lambda vertices are saved in LArray(1:3,1:NDigL)
         ! --- For the case where the closing vertex = 1:
         ! * LArray contains (0,0,0)
         ! * The removd diagram will be replaced by a T vertex with no external lines
         !     
         Constant = .FALSE.   ! Jacobian left operator involves no constant terms
         CALL GenDiagLambdaPre(NDigTotal, NMax, NRankParent, NRankL, Constant, &
     &        M1P, M2P, M3P, M13P, NDigL, LArray, M1L, M2L, M3L, M13L)
         !     
         ! --- Check size of intermediates
         !     
         DO IDig = 1, NDigL
!_Temp            CALL CheckOrderTL(M1L(1,IDig),M2L(1,IDig),M3L(1,IDig),
!_TEmp     *           M13L(IDig),NRankL)
         END DO
         !     
         ! --- Weight and phase factors
         !     
         WRITE(IOUT, '(/, X, "Weight and phase factors:")')
         DO IDig = 1, NDigL
            L1 = LArray(1,IDig)
            CALL Define_Phase(MaxOp, L1, M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), M13L(IDig), &
     &         MPhase, IOUT)
            !
            ! --- Replace the marked T vertex by Lambda vertex
            ! IF there is no de-excitation, shift T vertices to the left
            !
!            IF (LArray(1,IDig) /= 0) THEN
            M1L(1,IDig) = LArray(1,IDig)
            M2L(1,IDig) = LArray(2,IDig)
            M3L(1,IDig) = LArray(3,IDig)
!            ELSE
!               DO IT = 2, 4
!                  M1L(IT-1,IDig) = M1L(IT,IDig)
!                  M2L(IT-1,IDig) = M2L(IT,IDig)
!                  M3L(IT-1,IDig) = M3L(IT,IDig)
!               END DO
!               M1L(4,IDig) = 0
!               M2L(4,IDig) = 0
!               M3L(4,IDig) = 0
!            END IF
            !
            CALL Define_Weight(M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), MWeight, IOUT)
            MwpL(IDig) = MPhase * MWeight
         END DO
         !     
         ! --- Sort diagrams for factorization
         !     
!!!         CALL SortDiagJacobian(NDigL, M1L, M2L, M3L, M13L, MwpL)
         CALL SortDiagGen(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !
         ! --- Eliminate redundancy
         !
         CALL CheckRedundancy(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! --- Punch out the Jacobian left diagrams
         !     
         WRITE(IOUT, '("Generated Jacobian-left Diagrams and Weight Factors")')
         DO I = 1, NDigL
            WRITE(IOUT, '(X, I3, X, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, 3X, I2)') &
     &           I, &
     &           M1L(1,I), M2L(1,I), M3L(1,I), &
     &           M1L(2,I), M2L(2,I), M3L(2,I), &
     &           M1L(3,I), M2L(3,I), M3L(3,I), &
     &           M1L(4,I), M2L(4,I), M3L(4,I), &
     &           M13L(I), MwpL(I)
         END DO
         !
         ! o Save Lambda diagrams
         !
         CALL SaveJLeftDiagrams(NMax, NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! o Generate programs
         !     
         ! --- Subroutine Name
         !     
         SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
         END SELECT
         !     
         Name = TRIM(Name)//"_JacobL"
         !     
         WRITE(c1, '(I1)') NRankL
         Name = TRIM(Name)//"_Ket"//TRIM(c1)//"_AutoGen"
         !     
         IF (Complx) THEN
            PreFixSrc = "SOCCLR_"
         ELSE
            PreFixSrc = "CCLR_"
         END IF
         Name = TRIM(PreFixSrc)//Name
         !     
         ! --- Print header
         !     
         WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
!         WRITE(ICOD, *) TRIM(Name)
         WRITE(ICOD, *) TRIM(Name)//"(AmpFileT, AmpFileR, AmpFileL)"   ! Name of de-excitation amplitude file
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o This is an automatically generated program", /, &
     &      "!")')
         !     
         ! --- Modules
         !     
         WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
         ELSE
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
         END IF
         WRITE(ICOD, '( &
     &      "!", /, &
     &      6X, "IMPLICIT NONE", /, &
     &      "!")')
         !
         ! --- Input variable
         !
         WRITE(ICOD, '(6X, "CHARACTER(*), INTENT(IN) :: AmpFileT, AmpFileR, AmpFileL")')
         WRITE(ICOD, '("!")')
         !     
         ! --- Large arrays
         !     
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "COMPLEX(8), ALLOCATABLE :: V1(:), V1_(:), T(:), R(:), L(:), G(:), G_(:)", / &
     &         6X, "COMPLEX(8), ALLOCATABLE :: T_(:), R_(:)")')
         ELSE
            WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "REAL(8), ALLOCATABLE :: V1(:), V1_(:), T(:), R(:), L(:), G(:), G_(:)", / &
     &         6X, "REAL(8), ALLOCATABLE :: T_(:), R_(:)")')
         END IF
         !
         ! --- Local variables
         !
         CALL Declaration(ICOD, 'LAMBDA', Complx)
         !     
         ! --- Declare misc. arrays
         !    
         WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigL
         !     
         ! --- Declare CCLib_NComb function
         !     
         WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
         !     
         ! --- Initialize weight and phase factors
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o Weight and phase factors", /, &
     &      "!")')
         DO IDig = 1, NDigL
            WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &         3I2, "/", 3I2, "/", 3I2, "/", 3I2, I3)') &
     &         IDig, MwpL(IDig), &
     &         M1L(1,IDig), M2L(1,IDig), M3L(1,IDig), &
     &         M1L(2,IDig), M2L(2,IDig), M3L(2,IDig), &
     &         M1L(3,IDig), M2L(3,IDig), M3L(3,IDig), &
     &         M1L(4,IDig), M2L(4,IDig), M3L(4,IDig), &
     &         M13L(IDig)
         END DO
         !     
         ! --- Factorization of diagrams
         ! --- Start to generate the code
         !     
         WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, &
     &      ")-Lambda(", I2, ") equation", X, 5("-"), /, "!")') &
     &      NMax, NRankL
         !     
         ! --- Make arrays for string Addresses
         !     
         WRITE(ICOD, '("!", /, &
     &      "!     o Prepare arc weight arrays", /, &
     &      "!", /, &
     &      6X, "CALL CCLib_InitArcWgt", /)')
         !     
         ! --- Factorize the diagrams and generate the main frame of program
         !     
         ANameL = 'L'
!!!         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx, TRIM(ANameL))
         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx)
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, 5("-"), X, "End of Lambda calculation", X, &
     &      5("-"), /, "!")')
         !     
         WRITE(ICOD, '(6X, "END SUBROUTINE")')
         !     
         CLOSE(IOUT)
         CLOSE(ICOD)
         !
         ! --- End generating Jacobian-left programs
         !
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Define_Phase(MaxOp, NRank, M1, M2, M3, M13, MPhase, IOUT)
      !
      USE Vertex_Module, ONLY : NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: MaxOp, NRank, M1(4), M2(4), M3(4), M13, IOUT
      INTEGER, INTENT(OUT) :: MPhase
      !
      CHARACTER(LEN=3) :: Type(MaxOp)
      INTEGER :: M1Abs(4), M2Abs(4), M3Abs(4), N
      INTEGER :: L1, L2, L3
      INTEGER :: NumOp, NumOpL, NumOpT(4), NumOpLT, NumOpV
      INTEGER :: ICnt, IOp, IOpT, IOpV, IOpL, IT
      INTEGER :: IAdd, IOff
      INTEGER :: Link(MaxOp), NLoop(MaxOp)
      INTEGER :: NumLoop, NumHole, NumHoleL, NumHoleT, NumHoleV
      INTEGER :: Next
      INTEGER :: NumT
      !
      NumT = 4   ! # of T (or R) vertices (including zero excitations)
      !
      DO N = 1, NumT
         M1Abs(N) = ABS(M1(N))
         M2Abs(N) = ABS(M2(N))
         M3Abs(N) = ABS(M3(N))
      END DO
      !
      ! o Types of closing vertex
      !
      L1 = NExtPart(M13) + NExtHole(M13)
      L2 = NExtPart(M13) + NExtHole(M13)
      L3 = NExtPart(M13)
!      L1 = NRank
      DO IT = 1, NumT
         L1 = L1 + (M1Abs(IT) * 2 - M2Abs(IT))
      END DO
      IF (MOD(L1, 2) /= 0) THEN
         STOP 'Inconsistency in Define_Phase'
      END IF
      L1 = L1 / 2
      !
!!!
      WRITE(IOUT, '("Closing vertex: ", 3I2)') L1, L2, L3
!!!
      !
      ! --- Assume {(hole)*(particle)(hole)(particle)* ...} ordering of creation/annihilation operators
      !
      NumOpL = L1 * 2
      NumOp = NumOpL
      ICnt = 0
      DO IOp = 1, (NumOpL - 1), 2   ! holes
         ICnt = ICnt + 1
         IF (ICnt <= L2 - L3) THEN
            Type(IOp) = 'ih'   ! Connected to Hamiltonian
         ELSE
            Type(IOp) = 'eh'   ! Connected to T
         END IF
      END DO
      ICnt = 0
      DO IOp = 2, NumOpL, 2   ! particles
         ICnt = ICnt + 1
         IF (ICnt <= L3) THEN
            Type(IOp) = 'ip'   ! Connected to Hamiltonian
         ELSE
            Type(IOp) = 'ep'   ! Connected to T
         END IF
      END DO
      IAdd = NumOpL
      !
      ! o Types of T vertices
      ! --- Assume {(particle)(hole)*(particle)(hole)* ...} ordering
      !
      DO IT = 1, NumT
         NumOpT(IT) = M1Abs(IT) * 2
         IF (M1Abs(IT) == 0) CYCLE
         ICnt = 0
         DO IOp = 1, (NumOpT(IT) - 1), 2   ! particles
            ICnt = ICnt + 1
            IF (ICnt <= M3Abs(IT)) THEN
               Type(IAdd+IOp) = 'ip'
            ELSE
               Type(IAdd+IOp) = 'ep'
            END IF
         END DO
         ICnt = 0
         DO IOp = 2, NumOpT(IT), 2   ! holes
            ICnt = ICnt + 1
            IF (ICnt <= (M2Abs(IT) - M3Abs(IT))) THEN
               Type(IAdd+IOp) = 'ih'
            ELSE
               Type(IAdd+IOp) = 'eh'
            END IF
         END DO
         IAdd = IAdd + NumOpT(IT)
         NumOp = NumOp + NumOpT(IT)
      END DO
      !
      ! o Types of interaction vertex
      ! --- Assume {p*rq*s} (with the corresponding integral <pq||rs>) ordering
      !
      SELECT CASE (M13)
         CASE (1)
            NumOpV = 2
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'ip'
         CASE (2)
            NumOpV = 2
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
         CASE (3)
            NumOpV = 2
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
         CASE (4)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (5)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (6)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (7)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (8)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (9)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (10)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (11)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'ip'
         CASE (12)
            NumOpV = 2
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
         CASE (13)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'eh'
      END SELECT
      NumOp = NumOp + NumOpV
!!!
      WRITE(IOUT, *) (Type(IOp), IOP = 1, NumOp)
!!!
      !
      ! o Connectivity of closing vertex
      ! --- All particles connect to holes within the closing vertex
      ! --- Internal holes connect to external holes of interaction vertex
      ! --- External holes connect to external holes of T vertices
      ! --- Assume hole intices are ordered as (V T4 T3 T2 T1) (T4 is contracted to V at first)
      !
      NumOpLT = NumOpL
      DO IT = 1, NumT
         NumOpLT = NumOpLT + NumOpT(IT)
      END DO
      !
      IAdd = 0
      ICnt = 0
      DO IOp = 1, NumOpL
         IF (MOD(IOp, 2) == 0) THEN   ! particles
            Link(IAdd+IOp) = IAdd + IOp - 1
         ELSE                         ! holes
            ICnt = ICnt + 1
            IF (ICnt <= (L2 - L3)) THEN   ! Holes connected to Hamiltonian
               DO IOpV = 1, NumOpV
                  IF (Type(NumOpLT+IOpV) == 'eh') THEN
                     Link(IOp+IAdd) = NumOpLT + IOpV
                     Type(NumOpLT+IOpV) = '--'
                     EXIT
                  END IF
               END DO
            ELSE   ! Holes connected to T vertices
               IOff = NumOpLT
               LoopT2:DO IT = NumT, 1, -1
                  IF (M1Abs(IT) == 0) CYCLE
                  IOff = IOff - NumOpT(IT)
                  DO IOpT = 1, NumOpT(IT)
                     IF (Type(IOff+IOpT) == 'eh') THEN
                        LinK(IAdd+IOp) = IOff + IOpT
                        Type(IOff+IOpT) = '--'
                        EXIT LoopT2
                     END IF
                  END DO
               END DO LoopT2
            END IF
         END IF
      END DO
      IAdd = IAdd + NumOpL
      !
      ! o Connectivity of T vertices
      ! --- Holes connect to particles
      ! --- Internal particles connet to interaction vertex
      ! --- For internal particles, assume (T1 T2 T3 T4) ordering (T4 is contracted to V at first)
      !
      IAdd = NumOpL
      DO IT = 1, NumT
         IF (M1Abs(IT) == 0) CYCLE
         ICnt = 0
         DO IOpT = 1, NumOpT(IT)
            IF (MOD(IOpT, 2) == 0) THEN   ! holes
               Link(IAdd+IOpT) = IAdd + IOpT - 1
            ELSE   ! particles
               ICnt = ICnt + 1
               IF (ICnt <= M3Abs(IT)) THEN   ! internal particles
                  DO IOpV = 1, NumOpV
                     IF (Type(NumOpLT+IOpV) == 'ip') THEN
                        Link(IOpT+IAdd) = NumOpLT + IOpV
                        Type(NumOpLT+IOpV) = '--'
                        EXIT
                     END IF
                  END DO
               END IF
            END IF
         END DO
         IAdd = IAdd + NumOpT(IT)
      END DO
      !
      ! --- External particles connect to closing vertex
      ! --- Indices are ordered as (V T4 T3 T2 T1)
      !
      IAdd = NumOpLT
      DO IT = NumT, 1, -1
         IF (M1Abs(IT) == 0) CYCLE
         IAdd = IAdd - NumOpT(IT)
         ICnt = 0
         DO IOpT = 1, NumOpT(IT)
            IF (MOD(IOpT, 2) == 0) CYCLE
            ICnt = ICnt + 1
            IF (ICnt > M3Abs(IT)) THEN   ! External particles (Contract to closing vertex)
               DO IOpL = 1, NumOpL
                  IF (Type(IOpL) == 'ep') THEN
                     Link(IOpT+IAdd) = IOpL
                     Type(IOpL) = '--'
                     EXIT
                  END IF
               END DO
            END IF
         END DO
      END DO
      !
      ! o Connectivity of interaction vertex
      ! --- Connection to other vertices
      !
      IAdd = NumOpLT
      DO IOpV = 1, NumOpV
         IF (Type(IAdd+IOpV) == 'ih') THEN   ! Internal holes (Contract to T vertices)
            !
            ! --- Internal holes connect to T vertices in order (T1 T2 T3 T4)
            !
            IOff = NumOpL
            LoopT: DO IT = 1, NumT
               IF (M1Abs(IT) == 0) CYCLE
               DO IOpT = 1, NumOpT(IT)
                  IF (Type(IOff+IOpT) == 'ih') THEN
                     Link(IAdd+IOpV) = IOff + IOpT
                     Type(IOff+IOpT) = '--'
                     EXIT LoopT
                  END IF
               END DO
               IOff = IOff + NumOpT(IT)
            END DO LoopT
         ELSE IF (Type(IAdd+IOpV) == 'ep') THEN   ! External particles (Contracted to closing vertex)
            !
            ! --- External particles connect to closing vertex
            !
            DO IOpL = 1, NumOpL
               IF (Type(IOpL) == 'ip') THEN
                  Link(IAdd+IOpV) = IOpL
                  Type(IOpL) = '--'
                  EXIT
               END IF
            END DO
         END IF
      END DO
      !
      ! --- Connection in interaction vertex
      !
      SELECT CASE (M13)
         CASE (1, 2, 3, 12)
            Link(IAdd+2) = IAdd + 1
         CASE (4, 5, 6, 7, 8, 9, 10, 11, 13)
            Link(IAdd+2) = IAdd + 1
            Link(IAdd+4) = IAdd + 3
      END SELECT
      !
      NumOp = NumOpLT + NumOpV
!!!
      WRITE(IOUT, *) (Link(IOp), IOp = 1, NumOp)
!!!
      !
      ! o Count the # of loops using the connectvity Link
      !
      ! --- Initialize
      !
      DO IOp = 1, NumOp
         NLoop(IOp) = 0
      END DO
      !
      NumLoop = 0
      DO IOp = 1, NumOp
         IF (NLoop(IOp) /= 0) CYCLE
         NumLoop = NumLoop + 1
         NLoop(IOp) = NumLoop
         Next = Link(IOp)
  100    CONTINUE
         IF (NLoop(Next) == NumLoop) GO TO 200
         IF (NLoop(Next) /= 0) STOP 'Error: Inconsistency in Link'
         NLoop(Next) = NumLoop
         Next = Link(Next)
         GO TO 100
  200    CONTINUE
      END DO
      !
      WRITE(IOUT, '("Total # of loops: ", I3)') NumLoop
      !
      ! o # of holes
      !
      NumHoleL = L1
      NumHoleT = 0
      DO IT = 1, NumT
         NumHoleT = NumHoleT + M1Abs(IT)
      END DO
      SELECT CASE (M13)
         CASE (1, 4)
            NumHoleV = 0
         CASE (2, 6, 11, 13)
            NumHoleV = 2
         CASE (3, 7, 9, 12)
            NumHoleV = 1
         CASE (5)
            NumHoleV = 4
         CASE (8, 10)
            NumHoleV = 3
      END SELECT
      !
      NumHole = NumHoleL + NumHoleT + NumHoleV
      IF (MOD(NumHole, 2) /= 0) STOP 'Error: Inconsistent # of holes'
      NumHole = NumHole / 2
      !
      WRITE(IOUT, '("Total # of holes: ", I3)') NumHole
      FLUSH(IOUT)
      !
      IF (MOD(NumLoop - NumHole, 2) == 0) THEN
         MPhase = 1
      ELSE
         MPhase = -1
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Define_Weight(M1, M2, M3, MWeight, IOUT)
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: M1(4), M2(4), M3(4), IOUT
      INTEGER, INTENT(OUT) :: MWeight
      !
      INTEGER :: NumPair, M1I, M2I, M3I, M1J, M2J, M3J
      INTEGER :: IT, JT
      !
      NumPair = 0
      DO IT = 1, 4
         IF (M1(IT) == 0) CYCLE
         M1I = M1(IT)
         M2I = M2(IT)
         M3I = M3(IT)
         DO JT = 1, IT - 1
            IF (M1(JT) == 0) CYCLE
            M1J = M1(JT)
            M2J = M2(JT)
            M3J = M3(JT)
            IF ((M1I == M1J) .AND. (M2I == M2J) .AND. (M3I == M3J)) THEN
               NumPair = NumPair + 1
               EXIT
            END IF
         END DO
      END DO
      !
      MWeight = NumPair + 1
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE CheckRedundancy(NDigL, M1L, M2L, M3L, M13L, MwpL)
        !
        ! o Remove retundant diagrams (For lambda diagrams with no closing vertex)
        !
      USE BasicInformation, ONLY : MaxDig
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT) :: NDigL, M1L(4,*), M2L(4,*), M3L(4,*), M13L(*), MwpL(*)
      !
      INTEGER :: M1L1I, M1L2I, M1L3I, M1L4I, M2L1I, M2L2I, M2L3I, M2L4I, M3L1I, M3L2I, M3L3I, M3L4I, M13LI
      INTEGER :: M1L1J, M1L2J, M1L3J, M1L4J, M2L1J, M2L2J, M2L3J, M2L4J, M3L1J, M3L2J, M3L3J, M3L4J, M13LJ
      INTEGER :: IDig, JDig, KDig, IT
      !
      DO IDig = 1, NDigL
         M1L1I = M1L(1,IDig)
         M1L2I = M1L(2,IDig)
         M1L3I = M1L(3,IDig)
         M1L4I = M1L(4,IDig)
         M2L1I = M2L(1,IDig)
         M2L2I = M2L(2,IDig)
         M2L3I = M2L(3,IDig)
         M2L4I = M2L(4,IDig)
         M3L1I = M3L(1,IDig)
         M3L2I = M3L(2,IDig)
         M3L3I = M3L(3,IDig)
         M3L4I = M3L(4,IDig)
         M13LI = M13L(IDig)
         DO JDig = (IDig + 1), NDigL
            M1L1J = M1L(1,JDig)
            M1L2J = M1L(2,JDig)
            M1L3J = M1L(3,JDig)
            M1L4J = M1L(4,JDig)
            M2L1J = M2L(1,JDig)
            M2L2J = M2L(2,JDig)
            M2L3J = M2L(3,JDig)
            M2L4J = M2L(4,JDig)
            M3L1J = M3L(1,JDig)
            M3L2J = M3L(2,JDig)
            M3L3J = M3L(3,JDig)
            M3L4J = M3L(4,JDig)
            M13LJ = M13L(JDig)
            !
            ! --- Check coincidence
            !
            IF ((M1L1I == M1L1J) .AND. (M1L2I == M1L2J) .AND. (M1L3I == M1L3J) .AND. (M1L4I == M1L4J) .AND. &
     &          (M2L1I == M2L1J) .AND. (M2L2I == M2L2J) .AND. (M2L3I == M2L3J) .AND. (M2L4I == M2L4J) .AND. &
     &          (M3L1I == M3L1J) .AND. (M3L2I == M3L2J) .AND. (M3L3I == M3L3J) .AND. (M3L4I == M3L4J) .AND. &
     &          (M13LI == M13LJ)) THEN
               !
               ! --- Perfect conincidence; Remove JDig-th diagram
               !
               DO KDig = (JDig + 1), NDigL
                  DO IT = 1, 4
                     M1L(IT,KDig-1) = M1L(IT,KDig)
                     M2L(IT,KDig-1) = M2L(IT,KDig)
                     M3L(IT,KDig-1) = M3L(IT,KDig)
                  END DO
                  M13L(KDig-1) = M13L(KDig)
                  MWpL(KDig-1) = MwpL(KDig)
               END DO
               NDigL = NDigL - 1
            END IF
         END DO
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveJRightDiagrams(NMax, NRank, NDig, M1, M2, M3, M13, Mwp)
      !
      IMPLICIT NONE
      !
      INTEGER :: NMax, NRank, NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save Jacobian Right diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="JRightDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="JRightDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE   ! NRank = NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="JRightDiagrams", STATUS="OLD")
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveLambdaDiagrams(NMax, NRank, NDig, M1, M2, M3, M13, Mwp)
      !
      IMPLICIT NONE
      !
      INTEGER :: NMax, NRank, NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save Lambda diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="LambdaDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="LambdaDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
      ELSE   ! NRank = NMax
         OPEN(UNIT=IDIGFILE, FILE="LambdaDiagrams", STATUS="OLD")   ! ParentDiagram file
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveJLeftDiagrams(NMax, NRank, NDig, M1, M2, M3, M13, Mwp)
      !
      IMPLICIT NONE
      !
      INTEGER :: NMax, NRank, NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save Jacobian left diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="JLeftDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="JLeftDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE   ! NRank = NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="JLeftDiagrams", STATUS="OLD")
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveZetaDiagrams(NMax, NRank, NDig, M1, M2, M3, M13, Mwp)
      !
      IMPLICIT NONE
      !
      INTEGER :: NMax, NRank, NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save Zeta diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="ZetaDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="ZetaDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
      ELSE   ! NRank = NMax
         OPEN(UNIT=IDIGFILE, FILE="ZetaDiagrams", STATUS="OLD")   ! ParentDiagram file
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveRespDMCommonDiagrams(IType, NDigDM, M1DM, M2DM, M3DM, MwpDM)
      !
      IMPLICIT NONE
      !
      INTEGER :: IType, NDigDM
      INTEGER :: M1DM(5,*), M2DM(5,*), M3DM(5,*), MwpDM(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NTypeRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save response density matrix diagrams
      !
      IF (IType == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="RespDMCommonDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') 13
         WRITE(IDIGFILE, '("IType=   1")')
         WRITE(IDIGFILE, '(I4)') NDigDM
         DO IDig = 1, NDigDM
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, 3I5, X, I2)') &
     &         IDig, &
     &         M1DM(1,IDig), M2DM(1,IDig), M3DM(1,IDig), &
     &         M1DM(2,IDig), M2DM(2,IDig), M3DM(2,IDig), &
     &         M1DM(3,IDig), M2DM(3,IDig), M3DM(3,IDig), &
     &         M1DM(4,IDig), M2DM(4,IDig), M3DM(4,IDig), &
     &         M1DM(5,IDig), M2DM(5,IDig), M3DM(5,IDig), &
     &         MwpDM(IDig)
         END DO
         !
      ELSE IF (IType < 13) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="RespDMCommonDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NTypeRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NTypeRead < IType - 1) GO TO 200
         WRITE(IDIGFILE, '("IType=", I4)') IType
         WRITE(IDIGFILE, '(I4)') NDigDM
         DO IDig = 1, NDigDM
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, 3I5, X, I2)') &
     &         IDig, &
     &         M1DM(1,IDig), M2DM(1,IDig), M3DM(1,IDig), &
     &         M1DM(2,IDig), M2DM(2,IDig), M3DM(2,IDig), &
     &         M1DM(3,IDig), M2DM(3,IDig), M3DM(3,IDig), &
     &         M1DM(4,IDig), M2DM(4,IDig), M3DM(4,IDig), &
     &         M1DM(5,IDig), M2DM(5,IDig), M3DM(5,IDig), &
     &         MwpDM(IDig)
         END DO
      ELSE
         OPEN(UNIT=IDIGFILE, FILE="RespDMCommonDiagrams", STATUS="OLD")
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NTypeRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NTypeRead < 13 - 1) GO TO 300
         WRITE(IDIGFILE, '("IType=", I4)') IType
         WRITE(IDIGFILE, '(I4)') NDigDM
         DO I = 1, NDigDM
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, 3I5, X, I2)') &
     &         I, &
     &         M1DM(1,I), M2DM(1,I), M3DM(1,I), &
     &         M1DM(2,I), M2DM(2,I), M3DM(2,I), &
     &         M1DM(3,I), M2DM(3,I), M3DM(3,I), &
     &         M1DM(4,I), M2DM(4,I), M3DM(4,I), &
     &         M1DM(5,I), M2DM(5,I), M3DM(5,I), &
     &         MwpDM(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SaveIPRightDiagrams(NMax, NRank, NDig, M1, M2, M3, M13, Mwp)
      !
      IMPLICIT NONE
      !
      INTEGER :: NMax, NRank, NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=120) :: CDum120
      INTEGER, PARAMETER :: IDIGFILE = 90
      INTEGER :: NRankRead, IDig, NDigRead, I, IDum, NDum
      !
      ! o Save IP-EOM right diagrams
      !
      IF (NRank == 1) THEN
         !
         OPEN(UNIT=IDIGFILE, FILE="IPRightDiagrams", STATUS="UNKNOWN")   ! Diagram file
         WRITE(IDIGFILE, '(I3)') NMax
         WRITE(IDIGFILE, '("NRank=   1")')
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE IF (NRank /= NMax) THEN   ! 1 < NRank < NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="IPRightDiagrams", STATUS="OLD")   ! Diagram file
         READ(IDIGFILE, '(I3)') NDum
 200     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         DO IDig = 1, NDigRead
            READ(IDIGFILE, '(I4)') IDum
         END DO
         IF (NRankRead < NRank - 1) GO TO 200
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO IDig = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDig, &
     &         M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &         M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &         M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &         M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &         M13(IDig), Mwp(IDig)
         END DO
         !
      ELSE   ! NRank = NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="IPRightDiagrams", STATUS="OLD")
         READ(IDIGFILE, '(I3)') NDum
 300     CONTINUE
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !
         WRITE(*, '("NRankRead = ", I6)') NRankRead
         WRITE(*, '("NDigRead  = ", I6)') NDigRead
         !
         DO I = 1, NDigRead
            READ(IDIGFILE, '(A120)') CDum120
         END DO
         IF (NRankRead < NRank - 1) GO TO 300
         WRITE(IDIGFILE, '("NRank=", I4)') NRank
         WRITE(IDIGFILE, '(I4)') NDig
         DO I = 1, NDig
            WRITE(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         I, &
     &         M1(1,I), M2(1,I), M3(1,I), &
     &         M1(2,I), M2(2,I), M3(2,I), &
     &         M1(3,I), M2(3,I), M3(3,I), &
     &         M1(4,I), M2(4,I), M3(4,I), &
     &         M13(I), Mwp(I)
         END DO
         !
      END IF
      !
      CLOSE(IDIGFILE)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenFactorizeL1(NRank, NDig, M1, M2, M3, M13, Mwp, Facto, Complx)
!!!     &   ANameL)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Factorize diagrams: Lambda version
        !
        !   ANameL ... Name of de-excitation amplitude array used in the program
        !
        ! * Assume the Lambda vertex is multiplied at last
        ! * R vettices is accompanied by a minus sign
        ! * Lambda vertices is accompanied by an offset LOff
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : LOff
      USE Vertex_Module, ONLY : RightIP, RightEA, LeftIP, LeftEA
      !
      IMPLICIT NONE
      !
!      CHARACTER(*) :: ANameL
      LOGICAL :: Facto, Complx
      INTEGER :: NRank, NDig, M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=2) :: a2
      CHARACTER(LEN=255) :: c1, c2, c3, c4, CWrk, TName, VName
      INTEGER, PARAMETER :: MOff = LOff * ((LOff * 10) ** 2)   ! Offset for Lambda vertices
      INTEGER :: IOUT, ICOD
      INTEGER :: IDig, N, NCnt, NTlevel, NumT1, NumT2, NumT3, NumT4, NumDig, IT, IT1, IT2, IT3, IT4, &
     &   NT_Typ1, NT_Typ2, NT_Typ3, NT_Typ4, N_1, L_1, M_1, N_2, L_2, M_2, N_3, L_3, M_3, N_4, L_4, M_4
      INTEGER :: nB, nJ, nC, nK, nD, nL, nA, nI, nAI
      INTEGER :: NT(4,NDig), NTyp1(NDig), NTyp2(NDig), NTyp3(NDig), NTyp4(NDig)
      INTEGER :: M1Temp
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      DO IT = 1, 4
         RightIP(IT) = .FALSE.
         RightEA(IT) = .FALSE.
         LeftIP(IT) = .FALSE.
         LeftEA(IT) = .FALSE.
      END DO
      !
      ! --- Make 3-digit integers for T of R verteces
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            NT(N,IDig) = M1(N,IDig) * (LOff * 10 * LOff * 10) + M2(N,IDig) * (LOff * 10) + M3(N,IDig)   !<-- M1, M2 and M3 should be less than 10
         END DO   ! N
      END DO   ! IDig
      !
      ! --- Check the # and types of the outermost T or R vertices
      !
      IF (Facto) THEN
         NumT1 = 0   ! # of types of outermost T
         DO IDig = 1, NDig   ! look for non-zero outermost T
            IF ((IDig > 1) .AND. (NT(1,IDig) == NT(1,IDig-1))) CYCLE
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= MOff) THEN   ! Lambda vertex
               WRITE(IOUT, '(X, "A new L1 vertex ", I12, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!",5("-"), X, "A new L1 vertex ", I12, X, "found.")') NTyp1(NumT1)
            ELSE IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I12, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!",5("-"), X, "A new T1 vertex ", I12, X, "found.")') NTyp1(NumT1)
            ELSE   ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I13, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I13, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      ELSE
         NumT1 = 0
         DO IDig = 1, NDig
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= MOff) THEN   ! Lambda vertex
               WRITE(IOUT, '(X, "A new L1 vertex ", I12, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new L1 vertex ", I12, X, "found.")') NTyp1(NumT1)
            ELSE IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I12, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new T1 vertex ", I12, X, "found.")') NTyp1(NumT1)
            ELSE   ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I13, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I13, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      END IF
      WRITE(IOUT, '(/, "Start residue calculation", /)')
      !
      ! --- Size of V1 array (NOc: occ. MO number, NVr: vir. MO number)
      ! --- Integer function CCLib_NComb(N,n) computes N!/(N-n)!
      !
      WRITE(c1, '(I3)') NRank
      CWrk = "CCLib_NComb(NOc,"//TRIM(c1)//")*CCLib_NComb(NVr,"//TRIM(c1)//")"
      WRITE(IOUT, '(/, X, "Initialize V1 of size ", A100)') CWrk
      WRITE(ICOD, '(/, "!", 5("-"), X, "Initialize V1 of size ", A100)') CWrk
      !
      WRITE(ICOD, '(6X, "NSizeV1 = CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ",I2,")")') &
     &   NRank, NRank
      CALL GetVarray("V1", "NSizeV1", "V1", "INIT", ICOD)
      CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
      NumDig = 0
      WRITE(ICOD, '(6X, "NumDig = 0", /)')
      !
      DO IT1 = 1, NumT1
         !
         NT_Typ1 = NTyp1(IT1)
         IF (NT_Typ1 >= MOff) THEN
            WRITE(IOUT, '(/, X, "Processing L1(", I12, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing L1(", I12, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         ELSE IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(/, X, "Processing T1(", I12, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T1(", I12, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         ELSE
            WRITE(IOUT, '(/, X, "Processing R1(", I13, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R1(", I13, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         END IF
         IF (NT_Typ1 == 0) THEN   ! Substitute G into V1, IF there is no 1st T vertex, and GO TO the next T(1)
            NumDig = NumDig + 1
            !
            ! --- Read interaction vertex from disc
            !
            CALL DefGsize(M13(NumDig), ICOD)
            CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
            !
            ! --- Substitute interaction vertex to V1
            !
            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
            CALL AddGtoV("V1", "NSizeV1", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
            CALL FreeArray("G", ICOD)
            !
            ! --- Save and free V1
            !
            CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
            CYCLE
         END IF
         !
         ! --- Initialize V2
         !
!         N_1 = NT_Typ1 / 100
!         L_1 = (NT_Typ1 - 100 * N_1) / 10
!         M_1 = NT_Typ1 - 100 * N_1 - 10 * L_1
         N_1 = NT_Typ1 / (LOff * LOff * 100)
         L_1 = (NT_Typ1 - (LOff * LOff * 100) * N_1) / (LOff * 10)
         M_1 = NT_Typ1 - (LOff * LOff * 100) * N_1 - (LOff * 10) * L_1
         N_1 = N_1 - LOff
         L_1 = L_1 - LOff
         M_1 = M_1 - LOff
         IF (NT_Typ1 < 0) THEN
            N_1 = -N_1
            L_1 = -L_1
            M_1 = -M_1
         END IF
!!!
!         WRITE(*, '(I3, X, 3I1)') NumDig, N_1, L_1, M_1
!!!
         !
         ! Note: Determination of V2 size dependes on the integer triplet for Lambda vertex
         !
         WRITE(c1, '(I3)') NRank - (N_1 - M_1)           ! # of internal particles
         WRITE(c2, '(I3)') NRank - (N_1 - (L_1 - M_1))   ! # of internal holes
         WRITE(c3, '(I3)') M_1                           ! # of external particles
         WRITE(c4, '(I3)') L_1 - M_1                     ! # of external holes
!Lambda         WRITE(c1, '(I3)') M_1
!Lambda         WRITE(c2, '(I3)') L_1 - M_1
!Lambda         WRITE(c3, '(I3)') NRank - N_1 + M_1
!Lambda         WRITE(c4, '(I3)') NRank - N_1 + L_1 - M_1
         CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &      //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &      //")"
         !
         WRITE(IOUT, '(2X, "Initialize V2 of size ", A100)') CWrk
         WRITE(ICOD, '("!", 5("-"), X, "Initialize V2 of size ", A100)') CWrk
         !
         WRITE(ICOD, '(6X, "NSizeV2 = CCLib_NComb(NVr, ", I2, &
     &        ") * CCLib_NComb(NOc, ",I2, &
     &        ") * CCLib_NComb(NVr, ",I2,") * CCLib_NComb(NOc, ",I2, &
     &        ")")') NRank - (N_1 - M_1), NRank - (N_1 - (L_1 - M_1)), M_1, L_1 - M_1
!Lambda     &        ")")') M_1, L_1 - M_1, NRank - N_1 + M_1, NRank - N_1 + L_1 - M_1
         !
         CALL GetVarray("V2", "NSizeV2", "V2", "INIT", ICOD)
         CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
         !
         ! --- Find T(2) or R(2) vertices for current T(1) or R(1)
         !
         IF (Facto) THEN
            NumT2 = 0
            NCnt = 0
            DO IDig = 1, NDig
               IF (NT(1,IDig) /= NT_Typ1) CYCLE
               NCnt = NCnt + 1
               IF ((NCnt > 1) .AND. (NT(2,IDig) == NT(2,IDig-1))) CYCLE
               NumT2 = NumT2 + 1
               NTyp2(NumT2) = NT(2,IDig)
               IF (NTyp2(NumT2) >= 0) THEN
                  WRITE(IOUT, '(2X, "A new T2 vertex ", I12, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I12, X, "found.")') NTyp2(NumT2)
               ELSE
                  WRITE(IOUT, '(2X, "A new R2 vertex ", I13, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I13, X, "found.")') NTyp2(NumT2)
               END IF
            END DO   ! IDig
         ELSE
            NumT2 = 1
            NTyp2(NumT2) = NT(2,IT1)
            IF (NTyp2(NumT2) >= 0) THEN
               WRITE(IOUT, '(2X, "A new T2 vertex ", I12, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I12, X, "found.")') NTyp2(NumT2)
            ELSE
               WRITE(IOUT, '(2X, "A new R2 vertex ", I13, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I13, X, "found.")') NTyp2(NumT2)
            END IF
         END IF
         !
         DO IT2 = 1, NumT2
            !
            NT_Typ2 = NTyp2(IT2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(/, X, "Processing T2(", I12, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T2(", I12, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            ELSE
               WRITE(IOUT, '(/, X, "Processing R2(", I13, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R2(" ,I13, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            END IF
            !
            IF (NT_Typ2 == 0) THEN   ! Substitute G into V2 IF there is no 2nd T vertex
               NumDig = NumDig + 1
               !
               ! --- Read interaction vertex from disc
               !
               CALL DefGsize(M13(NumDig), ICOD)
               CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
               !
               ! --- Substitute G to V2
               !
               CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
               CALL AddGtoV("V2", "NSizeV2", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
               CALL FreeArray("G", ICOD)
               !
               ! --- Save V2 on disk
               !
               CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
               CYCLE
            END IF
            !
            ! --- Initialize V3
            !
            N_2 = NT_Typ2 / (LOff * LOff * 100)
            L_2 = (NT_Typ2 - (LOff * LOff * 100) * N_2) / (LOff * 10)
            M_2 = NT_Typ2 - (LOff * LOff * 100) * N_2 - (LOff * 10) * L_2
            IF (NT_Typ2 < 0) THEN
               N_2 = -N_2
               L_2 = -L_2
               M_2 = -M_2               
            END IF
!!!
!               WRITE(*, '(I3, 4X, 3I1)') NumDig, N_2, L_2, M_2
!!!
            WRITE(c1, '(I3)') (NRank - (N_1 - M_1)) + M_2                   ! # of internal particles
            WRITE(c2, '(I3)') (NRank - (N_1 - (L_1 - M_1))) + (L_2 - M_2)   ! # of internal holes
            WRITE(c3, '(I3)') M_1 - (N_2 - M_2)                             ! # of external particles
            WRITE(c4, '(I3)') (L_1 - M_1) - (N_2 - (L_2 - M_2))             ! # of external holes
!Lambda            WRITE(c1, '(I3)') M_1 + M_2
!Lambda            WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2)
!Lambda            WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2)
!Lambda            WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2)
            CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, " &
     &           //TRIM(c2) &
     &           //") * CCLib_NComb(NVr, "//TRIM(c3) &
     &           //")*CCLib_NComb(NOc, "//TRIM(c4) &
     &           //")"
            !
            WRITE(IOUT, '(3X, "Initialize V3 of size ", A100)') CWrk
            WRITE(ICOD, '("!", 5("-"), X, "Initialize V3 of size ", A100)') CWrk
            !
            WRITE(ICOD, '(6X, "NSizeV3 = CCLib_NComb(NVr, ", I2, &
     &           ") * CCLib_NComb(NOc, ", I2, &
     &           ") * CCLib_NComb(NVr, ", I2, &
     &           ") * CCLib_NComb(NOc, ", I2,")")') &
     &            (NRank - (N_1 - M_1)) + M_2, (NRank - (N_1 - (L_1 - M_1))) + (L_2 - M_2), &
     &            M_1 - (N_2 - M_2), (L_1 - M_1) - (N_2 - (L_2 - M_2))
!Lambda     &           M_1 + M_2, (L_1 - M_1) + (L_2 - M_2), NRank - (N_1 - M_1) - (N_2 - M_2), &
!Lambda     &           NRank - (N_1-L_1+M_1) - (N_2-L_2+M_2)
            CALL GetVarray("V3", "NSizeV3", "V3", "INIT", ICOD)
            CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
            !
            ! --- Find T(3) or R(3) for current T(or R)(1) and T(or R)(2)
            !
            IF (Facto) THEN
               NumT3 = 0
               NCnt = 0
               DO IDig = 1, NDig
                  IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2)) CYCLE
                  NCnt = NCnt + 1
                  IF ((NCnt > 1) .AND. (NT(3,IDig) == NT(3,IDig-1))) CYCLE
                  NumT3 = NumT3 + 1
                  NTyp3(NumT3) = NT(3,IDig)
                  IF (NTyp3(NumT3) >= 0) THEN
                     WRITE(IOUT, '(3X, "A new T3 vertex ", I12, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I12, X, "found.")') NTyp3(NumT3)
                  ELSE
                     WRITE(IOUT, '(3X, "A new R3 vertex ", I13, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I13, X, "found.")') NTyp3(NumT3)
                  END IF
               END DO   ! IDig
            ELSE
               NumT3 = 1
               NTyp3(NumT3) = NT(3,IT1)
               IF (NTyp3(NumT3) >= 0) THEN
                  WRITE(IOUT, '(3X, "A new T3 vertex ", I12, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I12, X, "found.")') NTyp3(NumT3)
               ELSE
                  WRITE(IOUT, '(3X, "A new R3 vertex ", I13, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I13, X, "found.")') NTyp3(NumT3)
               END IF
            END IF
            !
            DO IT3 = 1, NumT3
               !
               NT_Typ3 = NTyp3(IT3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(/, X, "Processing T3(", I12, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T3(", I12, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               ELSE
                  WRITE(IOUT, '(/, X, "Processing R3(", I13, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R3(" ,I13, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               END IF
               !
               IF (NT_Typ3 == 0) THEN   ! Substitute G into V3 IF there is no 3rd T vertex
                  NumDig = NumDig + 1
                  !
                  ! --- Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Substitute G into V3
                  !
                  CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
                  CALL AddGtoV("V3", "NSizeV3", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                  CALL FreeArray("G", ICOD)
                  !
                  ! --- Save V3 on disk
                  !
                  CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
                  CYCLE
               END IF
               !
               ! --- Initialize V4
               !
               N_3 = NT_Typ3 / (LOff * LOff * 100)
               L_3 = (NT_Typ3 - (LOff * LOff * 100) * N_3) / (LOff * 10)
               M_3 = NT_Typ3 - (LOff * LOff* 100) * N_3 - (LOff * 10) * L_3
               IF (NT_Typ3 < 0) THEN
                  N_3 = -N_3
                  L_3 = -L_3
                  M_3 = -M_3
               END IF
!!!
!               WRITE(*, '(I3, X, 3I1, X, 3I1, X, 3I1)') NumDig, N_1, L_1, M_1, N_2, L_2, M_2, N_3, L_3, M_3
!!!
               WRITE(c1, '(I3)') ((NRank - (N_1 - M_1)) + M_2) + M_3                           ! # of internal particles
               WRITE(c2, '(I3)') ((NRank - (N_1 - (L_1 - M_1))) + (L_2 - M_2)) + (L_3 - M_3)   ! # of internal holes
               WRITE(c3, '(I3)') (M_1 - (N_2 - M_2)) - (N_3 - M_3)                             ! # of external particles
               WRITE(c4, '(I3)') ((L_1 - M_1) - (N_2 - (L_2 - M_2))) - (N_3 - (L_3 - M_3))     ! # of external holes
!Lambda               WRITE(c1, '(I3)') M_1 + M_2 + M_3
!Lambda               WRITE(c2, '(I3)') (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3)
!Lambda               WRITE(c3, '(I3)') NRank - (N_1 - M_1) - (N_2 - M_2) - (N_3 - M_3)
!Lambda               WRITE(c4, '(I3)') NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &              //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &              //")"
               !
               WRITE(IOUT, '(4X, "Initialize V4 of size ", A100)') CWrk
               WRITE(ICOD, '("!", 5("-"), X, "Initialize V4 of size ", A100)') CWrk
               !
               WRITE(ICOD, '(6X, "NSizeV4 = CCLib_NComb(NVr, ", I2, &
     &              ") * CCLib_NComb(NOc, ", I2, &
     &              ") * CCLib_NComb(NVr, ", I2, &
     &              ") * CCLib_NComb(NOc, ", I2, &
     &              ")")') &
     &               ((NRank - (N_1 - M_1)) + M_2) + M_3, &
     &               ((NRank - (N_1 - (L_1 - M_1))) + (L_2 - M_2)) + (L_3 - M_3), &
     &               (M_1 - (N_2 - M_2)) - (N_3 - M_3), &
     &               ((L_1 - M_1) - (N_2 - (L_2 - M_2))) - (N_3 - (L_3 - M_3))
!Lambda     &              M_1 + M_2 + M_3, (L_1 - M_1) + (L_2 - M_2) + (L_3 - M_3), &
!Lambda     &              NRank - (N_1 - M_1) - (N_2 - M_2)- ( N_3 - M_3), &
!Lambda     &              NRank - (N_1 - L_1 + M_1) - (N_2 - L_2 + M_2) - (N_3 - L_3 + M_3)
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "INIT", ICOD)
               CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
               !
               ! --- Fint T(or R)(4) for current T(or R)(1), T(or R)(2), and T(or R)(3)
               !
               IF (Facto) THEN
                  NumT4 = 0
                  NCnt = 0
                  DO IDig = 1, NDig
                     IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2) .OR. (NT(3,IDig) /= NT_Typ3)) CYCLE
                     NCnt = NCnt + 1
                     IF ((NCnt > 1) .AND. (NT(4,IDig) == NT(4,IDig-1))) CYCLE
                     NumT4 = NumT4 + 1
                     NTyp4(NumT4) = NT(4,IDig)
                     IF (NTyp4(NumT4) >= 0) THEN
                        WRITE(IOUT, '(4X, "A new T4 vertex ", I12, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I12, X, "found.")') NTyp4(NumT4)
                     ELSE
                        WRITE(IOUT, '(4X, "A new R4 vertex ", I13, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I13, X, "found.")') NTyp4(NumT4)
                     END IF
                  END DO   ! IDig
               ELSE
                  NumT4 = 1
                  NTyp4(NumT4) = NT(4,IT1)
                  IF (NTyp4(NumT4) >= 0) THEN
                     WRITE(IOUT, '(4X, "A new T3 vertex ", I12, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I12, X, "found.")') NTyp4(NumT4)
                  ELSE
                     WRITE(IOUT, '(4X, "A new R4 vertex ", I13, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I13, X, "found.")') NTyp4(NumT4)
                  END IF
               END IF
               !
               DO IT4 = 1, NumT4 
                  !
                  NT_Typ4 = NTyp4(IT4)
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(/, X, "Processing T4(", I12, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T4(", I12, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(/, X, "Processing R4(", I13, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R4(", I13, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  END IF
                  !
                  IF (NT_Typ4 == 0) THEN   ! Substitute G into V4, IF there is no 4th T vertex
                     NumDig = NumDig + 1
                     !
                     ! --- Read interaction vertex from disc
                     !
                     CALL DefGsize(M13(NumDig), ICOD)
                     CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                     !
                     ! --- Substitute G into V4
                     !
                     CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                     CALL AddGtoV("V4", "NSizeV4", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                     CALL FreeArray("G", ICOD)
                     !
                     ! --- Save and free V4
                     !
                     CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                     CYCLE
                  END IF
                  !
                  ! --- Get interaction vertex
                  !
                  NumDig = NumDig + 1
                  !
                  ! --- Allocate G; Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Allocate G_
                  !
                  IF (M1(4,NumDig) < 0) THEN
                     CALL DefG_Size(M13(NumDig), -M1(4,NumDig), -M2(4,NumDig), -M3(4,NumDig), ICOD)
                  ELSE
                     CALL DefG_Size(M13(NumDig), M1(4,NumDig), M2(4,NumDig), M3(4,NumDig), ICOD)
                  END IF
                  CALL GetVarray("G_", "NSizeG_", " ", "NONE", ICOD)
                  !
                  ! --- Re-arrange G
                  !
                  WRITE(IOUT, '(4X, "Re-arrange G array")')
                  WRITE(ICOD, '("!", 5("-"), X, "Re-arrange G array")')
                  CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 4, ICOD)
                  nAI = nA + nI
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
                  CALL MakeSrcArrange("G", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  WRITE(ICOD, *) "     &   "//"G, G_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
                  !
                  ! --- Free G
                  !
                  CALL FreeArray("G", ICOD)
                  !
                  ! o Contract G_ to T(or R)(4); Add to V4
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(5X, "V4 += T4(", I12, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += T4(", I12, ")*I5")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(5X, "V4 += R4(", I13, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += R4(", I13, ")*I5")') NT_Typ4
                  END IF
                  !
                  ! --- Load V4
                  !
                  CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                  !
                  ! --- Load T amplitudes, Contraction: G_*T += V4, Free T
                  !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
                     M1Temp = M1(4,NumDig)
!                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                     CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, 'T', "AmpFileT")
                     CALL Contract("G_", "T", "V4", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("T", ICOD)
                  ELSE
                     M1Temp = IABS(M1(4,NumDig))
!                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
                     CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, 'R', "AmpFileR")
                     CALL Contract("G_", "R", "V4", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("R", ICOD)
                  END IF
                  !
                  ! --- Free G_
                  !
                  CALL FreeArray("G_", ICOD)
                  !
                  ! --- Save and free V4
                  !
                  CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                  !
                  ! --- End of level-4 contraction
                  !
               END DO   ! IT4 (loop over 4th T-vertex)
               !
               WRITE(IOUT, '(4X, "Re-arrange V4 array")')
               WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V4 array")')
               CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &              M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 3, ICOD)
               nAI = 99999
               IF (nA == 0 .AND. nI == 0) nAI = 0
               !
               ! --- Allocate V4_ array
               !
               WRITE(ICOD, '(6X, "NSizeV4_ = mAI * mJ * mB * mK * mC")')
               WRITE(ICOD, '(6X, "ALLOCATE(V4_(NSizeV4_))")')
               !
               ! --- Allocate and load V4 intermediate
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
               !
               ! --- Make SUBROUTINE to rearrange V4 -> V4_
               !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
               CALL MakeSrcArrange("V4", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               WRITE(ICOD, *) "     &   "//"V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
               CALL FreeVarray("V4", "NSizeV4", "V4", "NONE", ICOD)
               WRITE(ICOD, '(/)')
               !
               ! --- Contract V4_ to T(or R)(3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(5X, "V3 += T3(", I12, ")*V4")') NT_Typ3
                  WRITE(ICOD, '("!", 5("-"), X, "V3 += T3(", I12, ")*V4")') NT_Typ3
               ELSE
                  WRITE(IOUT, '(5X, "V3 += R3(", I13, ")*V4")') NT_Typ3
                  WRITE(ICOD, '("!", 5("-"), X, "V3 += R3(", I13, ")*V4")') NT_Typ3
               END IF
               !
               ! --- Load V3
               !
               CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
               !
               ! --- Load T amplitudes, Contraction: V4_*T += V3, Free T array
               !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
                  M1Temp = M1(3,NumDig)
!                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                  CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, 'T', "AmpFileT")
                  CALL Contract("V4_", "T", "V3", NRank, NTlevel, &
     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                  CALL FreeArray("T", ICOD)
               ELSE
                  M1Temp = IABS(M1(3,NumDig))
!                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'R')
                  CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, 'R', "AmpFileR")
                  CALL Contract("V4_", "R", "V3", NRank, NTlevel, &
     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                  CALL FreeArray("R", ICOD)
               END IF
               !
               ! --- Free V4_ array
               !
               CALL FreeArray("V4_", ICOD)
               !
               ! --- Save and free V3
               CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
               !
               ! --- End of level-3 contraction
               !
            END DO   ! IT3
            !
            WRITE(IOUT, '(4X, "Re-arrange V3 array")')
            WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V3 array")')
            CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 2, ICOD)
            nAI = 99999
            IF (nA == 0 .AND. nI == 0) nAI = 0
            !
            ! --- Allocate V3_ array
            !
            WRITE(ICOD, '(6X, "NSizeV3_ = mAI * mJ * mB * mK * mC")')
            WRITE(ICOD, '(6X, "ALLOCATE(V3_(NSizeV3_))")')
            !
            ! --- Allocate and load V3 intermediate
            !
            CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
            !
            ! --- Make SUBROUTINE to rearrange V3 -> V3_
            !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
            CALL MakeSrcArrange("V3", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
            WRITE(ICOD, '(6X, "CALL", $)')
            WRITE(ICOD, *) TRIM(CWrk)//"( &"
            WRITE(ICOD, *) "     &   "//"V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
            CALL FreeVarray("V3", "NSizeV3", "V3", "NONE", ICOD)
            WRITE(ICOD, '(/)')
            !
            ! --- Contract V3_ to T(or R)(2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(3X, "V2 += T2(", I12, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += T2(", I12, ")*V3")') NT_Typ2
            ELSE
               WRITE(IOUT, '(3X, "V2 += R2(" ,I13, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += R2(", I13, ")*V3")') NT_Typ2
            END IF
            !
            ! --- Load V2
            !
            CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
            !
            ! --- Load T amplitudes, Contraction: V3_*T += V2, Free T array
            !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
            NTlevel = 2
            IF (NT_Typ2 >= 0) THEN
               M1Temp = M1(2,NumDig)
!               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, "T", "AmpFileT")
               CALL Contract("V3_", "T", "V2", NRank, NTlevel, &
     &              M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("T", ICOD)
            ELSE
               M1Temp = IABS(M1(2,NumDig))
!               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "R")
               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, "R", "AmpFileR")
               CALL Contract("V3_", "R", "V2", NRank, NTlevel, &
     &              M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("R", ICOD)
            END IF
            !
            ! --- Free V3_ array
            !
            CALL FreeArray("V3_", ICOD)
            !
            ! --- Save and free V2
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
            !
            ! --- End of level-2 contraction
            !
         END DO   ! IT2
         !
         ! --- Contract V2 to Lambda(1)
         !
         IF (NT_Typ1 >= MOff) THEN
            WRITE(IOUT, '(2X, "V1 += L1(", I12, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += L1(", I12, ")*V2")') NT_Typ1
         ELSE IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(2X, "V1 += T1(", I12, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += T1(", I12, ")*V2")') NT_Typ1
         ELSE
            WRITE(IOUT, '(2X, "V1 += R1(", I13, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += R1(", I13, ")*V2")') NT_Typ1
         END IF
         !
         ! --- Allocate and load V2 intermediate
         !
         CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
         !
         IF (N_1 == 0) THEN
            !
            ! --- If Lambda is scalar, add V2 to V1
            !
            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
!!
            WRITE(ICOD, '(6X, "IF (NSizeV1 /= NSizeV2) STOP "" Error!!!""")')
!!!
            IF (Complx) THEN
               WRITE(ICOD, '(6X, "CALL ZAXPY(NSizeV1, OneC, V2, 1, V1, 1)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL DAXPY(NSizeV1, One, V2, 1, V1, 1)")')
            END IF
            !
            ! --- Free V2 array
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "NONE", ICOD)
            !
         ELSE
            !
            ! --- Allocate V1_ array (zero clear)
            !
            WRITE(IOUT, '("Allocate V1_ array")')
            WRITE(ICOD, '("!", 5("-"), X, "Allocate V1_ array")')
            CALL DefIndicesL1(nB, nJ, nA, nI, NRank, &
     &         M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 1, ICOD)
            nAI = 99999
            IF (nA == 0 .AND. nI == 0) nAI = 0
            !
            WRITE(ICOD, '(6X, "NSizeV1_ =  mB * mJ * mA * mI")')
            WRITE(ICOD, '(6X, "ALLOCATE(V1_(NSizeV1_))")')
            IF (Complx) THEN
               WRITE(ICOD, '(6X, "CALL CCLib_ZClear(V1_, NSizeV1_)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL CCLib_DClear(V1_, NSizeV1_)")')
            END IF
            !
            ! --- Load L amplitudes, Contraction: V2*L += V1_, free L array
            !
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
            NTlevel = 1
            IF (NT_Typ1 >= MOff) THEN
               M1Temp = M1(1,NumDig)-LOff
!               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, TRIM(ANameL))
!               CALL ContractL1("V2", TRIM(ANameL), "V1_", NRank, NTlevel, &
!     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!               CALL FreeArray(TRIM(ANameL), ICOD)
               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, "L", "AmpFileL")
               CALL ContractL1("V2", "L", "V1_", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("L", ICOD)
            ELSE IF (NT_Typ1 >= 0) THEN
               M1Temp = M1(1,NumDig)
!               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, "T", "AmpFileT")
               CALL Contract("V2", "T", "V1", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("T", ICOD)
            ELSE
               M1Temp = IABS(M1(1,NumDig))
!               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "R")
               CALL GetAmpFile(M1Temp, M1Temp, M1Temp, ICOD, "R", "AmpFileR")
               CALL Contract("V2", "R", "V1", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("R", ICOD)
            END IF
            !
            ! --- Free V2 array
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "NONE", ICOD)
            !
            ! --- Allocate and load V1
            !
            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
            !
            ! --- Arrange V1_ and add to V1
            !
            IF ((nB == 0 .AND. nJ == 0) .OR. (nA == 0 .AND. nI == 0)) THEN   ! V1_ and V1 are of same size
               IF (Complx) THEN
                  WRITE(ICOD, '(6X, "CALL ZAXPY(NSizeV1, OneC, V1_, 1, V1, 1)")')
               ELSE
                  WRITE(ICOD, '(6X, "CALL DAXPY(NSizeV1, One, V1_, 1, V1, 1)")')
               END IF
            ELSE
               WRITE(ICOD, '(6X, "mAn = CCLib_NComb(NVr, ", I2, ")")') NRank
               WRITE(ICOD, '(6X, "mIn = CCLib_NComb(NOc, ", I2, ")")') NRank
!!!
               FLUSH(ICOD)
               FLUSH(IOUT)
!!!
               CALL MakeSrcArrangeV1(nB, nJ, nA, nI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               WRITE(ICOD, *) "     &   "//"V1, V1_, nB, nJ, nA, nI, mB, mJ, mA, mI, mAn, mIn)"
               WRITE(ICOD, '(/)')
            END IF
            !
            ! --- Free V1_
            !
            CALL FreeArray("V1_", ICOD)
            !
         END IF
         !
         ! --- Save and free V1
         !
         CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
         !
         ! --- End of level-1 contraction
         !
      END DO   ! IT1
      !
      WRITE(IOUT, '(/, X, I5, X, "diagrams have been processed")') NumDig
      WRITE(ICOD, '(/, "!", 5("-"), X, I5, X, "diagrams have been processed")') NumDig
      !
      IF (NumDig /= NDig) THEN
         WRITE(IOUT, '(3X, "!!! NumDig is not equal to NDig !!!")')
         WRITE(*, '(3X, "!!! NumDig is not equal to NDig !!!",2I6)') NumDig, NDig
         STOP
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenFactorizeIPR(NRank, NDig, M1, M2, M3, M13, Mwp, Facto, Complx)
      !
      ! o Factorize diagrams: IP-EOM right version
      !
      USE Vertex_Module, ONLY : RightIP, RightEA, LeftIP, LeftEA
      !
      IMPLICIT NONE
      !
      LOGICAL :: Facto, Complx
      INTEGER :: NRank, NDig, M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      CHARACTER(LEN=2) :: a2
      CHARACTER(LEN=255) :: c1, c2, c3, c4, CWrk, TName, VName
      INTEGER :: IOUT, ICOD
      INTEGER :: IDig, N, NCnt, NTlevel, NumT1, NumT2, NumT3, NumT4, NumDig, IT, IT1, IT2, IT3, IT4, &
     &   NT_Typ1, NT_Typ2, NT_Typ3, NT_Typ4, N_1, L_1, M_1, N_2, L_2, M_2, N_3, L_3, M_3, N_4, L_4, M_4
      INTEGER :: nB, nJ, nC, nK, nD, nL, nA, nI, nAI
      INTEGER :: NT(4,NDig), NTyp1(NDig), NTyp2(NDig), NTyp3(NDig), NTyp4(NDig)
      INTEGER :: NumIntPart1, NumIntHole1, NumExtPart1, NumExtHole1, &
     &           NumIntPart2, NumIntHole2, NumExtPart2, NumExtHole2, &
     &           NumIntPart3, NumIntHole3, NumExtPart3, NumExtHole3, &
     &           NumIntPart4, NumIntHole4, NumExtPart4, NumExtHole4
      INTEGER :: M1Temp
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      DO IT = 1, 4
         RightIP(IT) = .FALSE.
         RightEA(IT) = .FALSE.
         LeftIP(IT) = .FALSE.
         LeftEA(IT) = .FALSE.
      END DO
      !
      ! --- Make 3-digit integers for T of R verteces
      !
      DO IDig = 1, NDig
         DO N = 1, 4
            NT(N,IDig) = M1(N,IDig) * 100 + M2(N,IDig) * 10 + M3(N,IDig)   !<-- M1, M2 and M3 should be less than 10
         END DO   ! N
      END DO   ! IDig
      !
      ! --- Check the # and types of the outermost T or R vertices
      !
      IF (Facto) THEN
         NumT1 = 0   ! # of types of outermost T
         DO IDig = 1, NDig   ! look for non-zero outermost T
            IF ((IDig > 1) .AND. (NT(1,IDig) == NT(1,IDig-1))) CYCLE
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!",5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE                ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      ELSE
         NumT1 = 0
         DO IDig = 1, NDig
            NumT1 = NumT1 + 1
            NTyp1(NumT1) = NT(1,IDig)
            IF (NTyp1(NumT1) >= 0) THEN   ! T-vertex or nothing
               WRITE(IOUT, '(X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new T1 vertex ", I3, X, "found.")') NTyp1(NumT1)
            ELSE                ! R-vertex
               WRITE(IOUT, '(X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
               WRITE(ICOD, '("!", 5("-"), X, "A new R1 vertex ", I4, X, "found.")') NTyp1(NumT1)
            END IF
         END DO
      END IF
      WRITE(IOUT, '(/, "Start residue calculation", /)')
      !
      ! --- Size of V1 array (NOc: occ. MO number, NVr: vir. MO number)
      ! --- Integer function CCLib_NComb(N,n) computes N!/(N-n)!
      !
      NumIntPart1 = 0
      NumIntHole1 = 0
      NumExtPart1 = NRank - 1
      NumExtHole1 = NRank
      WRITE(c1, '(I3)') NumExtPart1
      WRITE(c2, '(I3)') NumExtHole1
      CWrk = "CCLib_NComb(NOc,"//TRIM(c1)//")*CCLib_NComb(NVr,"//TRIM(c2)//")"
      WRITE(IOUT, '(/, X, "Initialize V1 of size ", A100)') CWrk
      WRITE(ICOD, '(/, "!", 5("-"), X, "Initialize V1 of size ", A100)') CWrk
      !
      WRITE(ICOD, '(6X, "NSizeV1 = CCLib_NComb(NVr, ", I2, ") * CCLib_NComb(NOc, ",I2, ")")') &
     &   NumExtPart1, NumExtHole1
      CALL GetVarray("V1", "NSizeV1", "V1", "INIT", ICOD)
      CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
      NumDig = 0
      WRITE(ICOD, '(6X, "NumDig = 0", /)')
      !
      DO IT1 = 1, NumT1
         !
         NT_Typ1 = NTyp1(IT1)
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(/, X, "Processing T1(", I3, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T1(", I3, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         ELSE
            WRITE(IOUT, '(/, X, "Processing R1(", I4, ")")') NT_Typ1
            WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R1(", I4, ")", X, 3("-"), /, "!")') &
     &         NT_Typ1
         END IF
         RightIP(1) = (NT_Typ1 < 0)
!IPR         IF (NT_Typ1 == 0) THEN   ! Substitute G into V1, IF there is no 1st T vertex, and GO TO the next T(1)
!IPR            NumDig = NumDig + 1
!IPR            !
!IPR            ! --- Read interaction vertex from disc
!IPR            !
!IPR            CALL DefGsize(M13(NumDig), ICOD)
!IPR            CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
!IPR            !
!IPR            ! --- Substitute interaction vertex to V1
!IPR            !
!IPR            CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
!IPR            CALL AddGtoV("V1", "NSizeV1", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
!IPR            CALL FreeArray("G", ICOD)
!IPR            !
!IPR            ! --- Save and free V1
!IPR            !
!IPR            CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
!IPR            CYCLE
!IPR         END IF
         !
         ! --- Initialize V2
         !
         N_1 = NT_Typ1 / 100
         L_1 = (NT_Typ1 - (100 * N_1)) / 10
         M_1 = (NT_Typ1 - (100 * N_1)) - (10 * L_1)
         IF (NT_Typ1 < 0) THEN
            N_1 = -N_1
            L_1 = -L_1
            M_1 = -M_1
         END IF
         NumIntPart2 = NumIntPart1 + M_1
         NumIntHole2 = NumIntHole1 + (L_1 - M_1)
         IF (RightIP(1)) THEN
            NumExtPart2 = NumExtPart1 - ((N_1 - 1) - M_1)
         ELSE
            NumExtPart2 = NumExtPart1 - (N_1 - M_1)
         END IF
         NumExtHole2 = NumExtHole1 - (N_1 - (L_1 - M_1))
         !
         WRITE(c1, '(I3)') NumIntPart2
         WRITE(c2, '(I3)') NumIntHole2
         WRITE(c3, '(I3)') NumExtPart2
         WRITE(c4, '(I3)') NumExtHole2
         CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &      //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &      //")"
         !
         WRITE(IOUT, '(2X, "Initialize V2 of size ", A100)') CWrk
         WRITE(ICOD, '("!", 5("-"), X, "Initialize V2 of size ", A100)') CWrk
         !
         WRITE(ICOD, '(6X, "NSizeV2 = CCLib_NComb(NVr, ", I2, &
     &      ") * CCLib_NComb(NOc, ",I2, &
     &      ") * CCLib_NComb(NVr, ",I2,") * CCLib_NComb(NOc, ",I2, &
     &      ")")') &
     &      NumIntPart2, NumIntHole2, NumExtPart2, NumExtHole2
         !
         CALL GetVarray("V2", "NSizeV2", "V2", "INIT", ICOD)
         CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
         !
         ! --- Find T(2) or R(2) vertices for current T(1) or R(1)
         !
         IF (Facto) THEN
            NumT2 = 0
            NCnt = 0
            DO IDig = 1, NDig
               IF (NT(1,IDig) /= NT_Typ1) CYCLE
               NCnt = NCnt + 1
               IF ((NCnt > 1) .AND. (NT(2,IDig) == NT(2,IDig-1))) CYCLE
               NumT2 = NumT2 + 1
               NTyp2(NumT2) = NT(2,IDig)
               IF (NTyp2(NumT2) >= 0) THEN
                  WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               ELSE
                  WRITE(IOUT, '(2X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               END IF
            END DO   ! IDig
         ELSE
            NumT2 = 1
            NTyp2(NumT2) = NT(2,IT1)
            IF (NTyp2(NumT2) >= 0) THEN
               WRITE(IOUT, '(2X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new T2 vertex ", I3, X, "found.")') NTyp2(NumT2)
            ELSE
               WRITE(IOUT, '(2X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
               WRITE(ICOD, '("!", 5("-"), X, "A new R2 vertex ", I4, X, "found.")') NTyp2(NumT2)
            END IF
         END IF
         !
         DO IT2 = 1, NumT2
            !
            NT_Typ2 = NTyp2(IT2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(/, X, "Processing T2(", I3, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T2(", I3, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            ELSE
               WRITE(IOUT, '(/, X, "Processing R2(", I4, ")")') NT_Typ2
               WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R2(" ,I4, ")", X, 3("-"), /, "!")') &
     &            NT_Typ2
            END IF
            RightIP(2) = (NT_Typ2 < 0)
            !
            IF (NT_Typ2 == 0) THEN   ! Substitute G into V2 IF there is no 2nd T vertex
               NumDig = NumDig + 1
               !
               ! --- Read interaction vertex from disc
               !
               CALL DefGsize(M13(NumDig), ICOD)
               CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
               !
               ! --- Substitute G to V2
               !
               CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
               CALL AddGtoV("V2", "NSizeV2", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
               CALL FreeArray("G", ICOD)
               !
               ! --- Save V2 on disk
               !
               CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
               CYCLE
            END IF
            !
            ! --- Initialize V3
            !
            N_2 = NT_Typ2 / 100
            L_2 = (NT_Typ2 - 100 * N_2) / 10
            M_2 = NT_Typ2 - 100 * N_2 - 10 * L_2
            IF (NT_Typ2 < 0) THEN
               N_2 = -N_2
               L_2 = -L_2
               M_2 = -M_2
            END IF
            NumIntPart3 = NumIntPart2 + M_2
            NumIntHole3 = NumIntHole2 + (L_2 - M_2)
            IF (RightIP(2)) THEN
               NumExtPart3 = NumExtPart2 - ((N_2 - 1) - M_2)
            ELSE
               NumExtPart3 = NumExtPart2 - (N_2 - M_2)
            END IF
            NumExtHole3 = NumExtHole2 - (N_2 - (L_2 - M_2))
            !
            WRITE(c1, '(I3)') NumIntPart3
            WRITE(c2, '(I3)') NumIntHole3
            WRITE(c3, '(I3)') NumExtPart3
            WRITE(c4, '(I3)') NumExtHole3
            CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, " &
     &           //TRIM(c2) &
     &           //") * CCLib_NComb(NVr, "//TRIM(c3) &
     &           //")*CCLib_NComb(NOc, "//TRIM(c4) &
     &           //")"
            !
            WRITE(IOUT, '(3X, "Initialize V3 of size ", A100)') CWrk
            WRITE(ICOD, '("!", 5("-"), X, "Initialize V3 of size ", A100)') CWrk
            !
            WRITE(ICOD, '(6X, "NSizeV3 = CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2, &
     &         ") * CCLib_NComb(NVr, ", I2, &
     &         ") * CCLib_NComb(NOc, ", I2,")")') &
     &         NumIntPart3, NumIntHole3, NumExtPart3, NumExtHole3
            !
            CALL GetVarray("V3", "NSizeV3", "V3", "INIT", ICOD)
            CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
            !
            ! --- Find T(3) or R(3) for current T(or R)(1) and T(or R)(2)
            !
            IF (Facto) THEN
               NumT3 = 0
               NCnt = 0
               DO IDig=1,NDig
                  IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2)) CYCLE
                  NCnt = NCnt + 1
                  IF ((NCnt > 1) .AND. (NT(3,IDig) == NT(3,IDig-1))) CYCLE
                  NumT3 = NumT3 + 1
                  NTyp3(NumT3) = NT(3,IDig)
                  IF (NTyp3(NumT3) >= 0) THEN
                     WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  ELSE
                     WRITE(IOUT, '(3X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  END IF
               END DO   ! IDig
            ELSE
               NumT3 = 1
               NTyp3(NumT3) = NT(3,IT1)
               IF (NTyp3(NumT3) >= 0) THEN
                  WRITE(IOUT, '(3X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new T3 vertex ", I3, X, "found.")') NTyp3(NumT3)
               ELSE
                  WRITE(IOUT, '(3X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
                  WRITE(ICOD, '("!", 5("-"), X, "A new R3 vertex ", I4, X, "found.")') NTyp3(NumT3)
               END IF
            END IF
            !
            DO IT3 = 1, NumT3
               !
               NT_Typ3 = NTyp3(IT3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(/, X, "Processing T3(", I3, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T3(", I3, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               ELSE
                  WRITE(IOUT, '(/, X, "Processing R3(", I4, ")")') NT_Typ3
                  WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R3(" ,I4, ")", X, 3("-"), /, "!")') &
     &               NT_Typ3
               END IF
               RightIP(3) = (NT_Typ3 < 0)
               !
               IF (NT_Typ3 == 0) THEN   ! Substitute G into V3 IF there is no 3rd T vertex
                  NumDig = NumDig + 1
                  !
                  ! --- Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Substitute G into V3
                  !
                  CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
                  CALL AddGtoV("V3", "NSizeV3", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                  CALL FreeArray("G", ICOD)
                  !
                  ! --- Save V3 on disk
                  !
                  CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
                  CYCLE
               END IF
               !
               ! --- Initialize V4
               !
               N_3 = NT_Typ3 / 100
               L_3 = (NT_Typ3 - 100 * N_3) / 10
               M_3 = NT_Typ3 - 100 * N_3 - 10 * L_3
               IF (NT_Typ3 < 0) THEN
                  N_3 = -N_3
                  L_3 = -L_3
                  M_3 = -M_3
               END IF
               !
               NumIntPart4 = NumIntPart3 + M_3
               NumIntHole4 = NumIntHole3 + (L_3 - M_3)
               IF (RightIP(3)) THEN
                  NumExtPart4 = NumExtPart3 - ((N_3 - 1) - M_3)
               ELSE
                  NumExtPart4 = NumExtPart3 - (N_3 - M_3)
               END IF
               NumExtHole4 = NumExtHole3 - (N_3 - (L_3 - M_3))
               !
               WRITE(c1, '(I3)') NumIntPart4
               WRITE(c2, '(I3)') NumIntHole4
               WRITE(c3, '(I3)') NumExtPart4
               WRITE(c4, '(I3)') NumExtHole4
               CWrk = "CCLib_NComb(NVr, "//TRIM(c1)//")*CCLib_NComb(NOc, "//TRIM(c2) &
     &              //") * CCLib_NComb(NVr, "//TRIM(c3)//")*CCLib_NComb(NOc, "//TRIM(c4) &
     &              //")"
               !
               WRITE(IOUT, '(4X, "Initialize V4 of size ", A100)') CWrk
               WRITE(ICOD, '("!", 5("-"), X, "Initialize V4 of size ", A100)') CWrk
               !
               WRITE(ICOD, '(6X, "NSizeV4 = CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ") * CCLib_NComb(NVr, ", I2, &
     &            ") * CCLib_NComb(NOc, ", I2, &
     &            ")")') &
     &            NumIntPart4, NumIntHole4, NumExtPart4, NumExtHole4
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "INIT", ICOD)
               CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
               !
               ! --- Find T(or R)(4) for current T(or R)(1), T(or R)(2), and T(or R)(3)
               !
               IF (Facto) THEN
                  NumT4 = 0
                  NCnt = 0
                  DO IDig = 1, NDig
                     IF ((NT(1,IDig) /= NT_Typ1) .OR. (NT(2,IDig) /= NT_Typ2) .OR. (NT(3,IDig) /= NT_Typ3)) CYCLE
                     NCnt = NCnt + 1
                     IF ((NCnt > 1) .AND. (NT(4,IDig) == NT(4,IDig-1))) CYCLE
                     NumT4 = NumT4 + 1
                     NTyp4(NumT4) = NT(4,IDig)
                     IF (NTyp4(NumT4) >= 0) THEN
                        WRITE(IOUT, '(4X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     ELSE
                        WRITE(IOUT, '(4X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                        WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     END IF
                  END DO   ! IDig
               ELSE
                  NumT4 = 1
                  NTyp4(NumT4) = NT(4,IT1)
                  IF (NTyp4(NumT4) >= 0) THEN
                     WRITE(IOUT, '(4X, "A new T3 vertex ", I3, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new T4 vertex ", I3, X, "found.")') NTyp4(NumT4)
                  ELSE
                     WRITE(IOUT, '(4X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                     WRITE(ICOD, '("!", 5("-"), X, "A new R4 vertex ", I4, X, "found.")') NTyp4(NumT4)
                  END IF
               END IF
               !
               DO IT4 = 1, NumT4 
                  !
                  NT_Typ4 = NTyp4(IT4)
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(/, X, "Processing T4(", I3, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing T4(", I3, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(/, X, "Processing R4(", I4, ")")') NT_Typ4
                     WRITE(ICOD, '("!", /, "!", 5X, 3("-"), X, "Processing R4(", I4, ")", &
     &                  X, 3("-"), /, "!")') NT_Typ4
                  END IF
                  RightIP(4) = (NT_Typ4 < 0)
                  !
                  IF (NT_Typ4 == 0) THEN   ! Substitute G into V4, IF there is no 4th T vertex
                     NumDig = NumDig + 1
                     !
                     ! --- Read interaction vertex from disc
                     !
                     CALL DefGsize(M13(NumDig), ICOD)
                     CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                     !
                     ! --- Substitute G into V4
                     !
                     CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                     CALL AddGtoV("V4", "NSizeV4", M13(NumDig), Mwp(NumDig), IOUT, ICOD)
                     CALL FreeArray("G", ICOD)
                     !
                     ! --- Save and free V4
                     !
                     CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                     CYCLE
                  END IF
                  !
                  ! --- Get interaction vertex
                  !
                  NumDig = NumDig + 1
                  !
                  ! --- Allocate G; Read interaction vertex from disc
                  !
                  CALL DefGsize(M13(NumDig), ICOD)
                  CALL GetGarray(M13(NumDig), ICOD, IOUT, NumDig)
                  !
                  ! --- Allocate G_
                  !
                  IF (M1(4,NumDig) < 0) THEN
                     CALL DefG_Size(M13(NumDig), -M1(4,NumDig), -M2(4,NumDig), -M3(4,NumDig), ICOD)
                  ELSE
                     CALL DefG_Size(M13(NumDig), M1(4,NumDig), M2(4,NumDig), M3(4,NumDig), ICOD)
                  END IF
                  CALL GetVarray("G_", "NSizeG_", " ", "NONE", ICOD)
                  !
                  ! --- Re-arrange G
                  !
                  WRITE(IOUT, '(4X, "Re-arrange G array")')
                  WRITE(ICOD, '("!", 5("-"), X, "Re-arrange G array")')
                  CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 4, ICOD)
!091112Bgn
                  IF ((nC /= 0) .OR. (nK /= 0)) THEN
!091112End
                     nAI = nA + nI
                     CALL MakeSrcArrange("G", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                     WRITE(ICOD, '(6X, "CALL", $)')
                     WRITE(ICOD, *) TRIM(CWrk)//"( &"
                     WRITE(ICOD, *) "     &   "//"G, G_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
!091112Bgn
                  ELSE
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeG, G, 1, G_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeG, G, 1, G_, 1)")')
                     END IF
                  END IF
!091112End
                  !
                  ! --- Free G
                  !
                  CALL FreeArray("G", ICOD)
!091112Bgn
                  !
                  ! Load T amplitudes
                  !
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
                     M1Temp = M1(4,NumDig)
                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
                  ELSE
                     M1Temp = IABS(M1(4,NumDig))
                     CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')   ! IPR
                  END IF
                  !
                  ! --- Re-arrange T (or R) if neccessary
                  !
                  N_4 = NT_Typ4 / 100
                  L_4 = (NT_Typ4 - 100 * N_4) / 10
                  M_4 = NT_Typ4 - 100 * N_4 - 10 * L_4
                  IF (NT_Typ4 < 0) THEN
                     N_4 = -N_4
                     L_4 = -L_4
                     M_4 = -M_4
                  END IF
                  !
                  nB = M_4
                  nJ = L_4 - M_4
                  IF (NT_Typ4 >= 0) THEN
                     nC = N_4 - nB
                  ELSE
                     nC = (N_4 - 1) - nB   ! IPR
                  END IF
                  nK = N_4 - nJ
                  nD = nB + nC
                  nL = nJ + nK
                  nA = 0
                  nI = 0
                  !
                  WRITE(ICOD, '(6X, "nB =", I3)') nB
                  WRITE(ICOD, '(6X, "nJ =", I3)') nJ
                  WRITE(ICOD, '(6X, "nC =", I3)') nC
                  WRITE(ICOD, '(6X, "nK =", I3)') nK
                  WRITE(ICOD, '(6X, "nD =", I3)') nD
                  WRITE(ICOD, '(6X, "nL =", I3)') nL
                  WRITE(ICOD, '(6X, "nA =", I3)') nA
                  WRITE(ICOD, '(6X, "nI =", I3)') nI
                  !
                  WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
                  WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
                  WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
                  WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
                  WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
                  WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
                  WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &               "CCLib_NComb(NOc, nI)")')
                  !
                  WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &                                        "CCLib_NComb(NVr, nB) * ", &
     &                                        "CCLib_NComb(NOc, nK) * ", &
     &                                        "CCLib_NComb(NVr, nC)")')
                  !
                  IF (NT_Typ4 >= 0) THEN
                     CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
                  ELSE
                     CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
                  END IF
                  !
                  IF ((nC == 0) .AND. (nK == 0)) GO TO 5000
                  IF ((nC == 0) .AND. (nJ == 0)) GO TO 5000
                  !
                  WRITE(IOUT, '(4X, "Re-arrange T(4) amplitude")')
                  !
                  nAI = 0
                  CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                     CALL FreeArray("T", ICOD)
                  ELSE
                     WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                     CALL FreeArray("R", ICOD)
                  END IF
                  GO TO 5100
 5000             CONTINUE
                  IF (NT_Typ4 >= 0) THEN
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
                     END IF
                     CALL FreeArray("T", ICOD)
                  ELSE
                     IF (Complx) THEN
                        WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
                     ELSE
                        WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
                     END IF
                     CALL FreeArray("R", ICOD)
                  END IF
 5100             CONTINUE
!091112End
                  !
                  ! --- Contract G_ to T(or R)(4); Add to V4
                  !
                  IF (NT_Typ4 >= 0) THEN
                     WRITE(IOUT, '(5X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += T4(", I3, ")*I5")') NT_Typ4
                  ELSE
                     WRITE(IOUT, '(5X, "V4 += R4(", I4, ")*I5")') NT_Typ4
                     WRITE(ICOD, '("!", 5("-"), X, "V4 += R4(", I4, ")*I5")') NT_Typ4
                  END IF
                  !
                  ! --- Load V4
                  !
                  CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
                  !
                  ! --- Load T amplitudes, Contraction: G_*T += V4, Free T
                  !
                  NTlevel = 4
                  IF (NT_Typ4 >= 0) THEN
!GEMV                     M1Temp = M1(4,NumDig)
!GEMV                     CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
!GEMV                     CALL Contract("G_", "T", "V4", NRank, NTlevel, &
!GEMV     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV                     CALL FreeArray("T", ICOD)
                     CALL ContractBLAS3("G_", "T_", "V4", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("T_", ICOD)
                  ELSE
!GEMV                     M1Temp = IABS(M1(4,NumDig))
!GEMV                     CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')
!GEMV                     CALL Contract("G_", "R", "V4", NRank, NTlevel, &
!GEMV     &                   M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV                     CALL FreeArray("R", ICOD)
                     CALL ContractBLAS3("G_", "R_", "V4", NRank, NTlevel, &
     &                   M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("R_", ICOD)
                  END IF
                  !
                  ! --- Free G_
                  !
                  CALL FreeArray("G_", ICOD)
                  !
                  ! --- Save and free V4
                  !
                  CALL FreeVarray("V4", "NSizeV4", "V4", "SAVE", ICOD)
                  !
                  ! --- End of level-4 contraction
                  !
               END DO   ! IT4 (loop over 4th T-vertex)
               !
               WRITE(IOUT, '(4X, "Re-arrange V4 array")')
               WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V4 array")')
               CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 3, ICOD)
               nAI = 99999
               IF ((nA == 0) .AND. (nI == 0)) nAI = 0
               !
               ! --- Allocate V4_ array
               !
               WRITE(ICOD, '(6X, "NSizeV4_ = mAI * mJ * mB * mK * mC")')
               WRITE(ICOD, '(6X, "ALLOCATE(V4_(NSizeV4_))")')
               !
               ! --- Allocate and load V4 intermediate
               !
               CALL GetVarray("V4", "NSizeV4", "V4", "LOAD", ICOD)
!091112Bgn
               IF ((nC /= 0) .OR. (nK /= 0)) THEN
!091112End
                  !
                  ! --- Make SUBROUTINE to rearrange V4 -> V4_
                  !
                  CALL MakeSrcArrange("V4", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
                  WRITE(ICOD, '(6X, "CALL", $)')
                  WRITE(ICOD, *) TRIM(CWrk)//"( &"
                  WRITE(ICOD, *) "     &   "//"V4, V4_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
!091112Bgn
               ELSE
                  IF (Complx) THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeV4, V4, 1, V4_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeV4, V4, 1, V4_, 1)")')
                  END IF
               END IF
!091112End
               !
               CALL FreeVarray("V4", "NSizeV4", "V4", "NONE", ICOD)
               WRITE(ICOD, '("!")')
!091112Bgn
               !
               ! Load T amplitudes
               !
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
                  M1Temp = M1(3,NumDig)
                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
               ELSE
                  M1Temp = IABS(M1(3,NumDig))
                  CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')   !IPR
               END IF
               !
               WRITE(IOUT, '(4X, "Re-arrange T(3) amplitude")')
               !
               ! --- Re-arrange T (or R) if neccessary
               !
               nB = M_3
               nJ = L_3 - M_3
               IF (NT_Typ3 >= 0) THEN
                  nC = N_3 - nB
               ELSE
                  nC = (N_3 - 1) - nB   !IPR
               END IF
               nK = N_3 - nJ
               nD = nB + nC
               nL = nJ + nK
               nA = 0
               nI = 0
               !
               WRITE(ICOD, '(6X, "nB =", I3)') nB
               WRITE(ICOD, '(6X, "nJ =", I3)') nJ
               WRITE(ICOD, '(6X, "nC =", I3)') nC
               WRITE(ICOD, '(6X, "nK =", I3)') nK
               WRITE(ICOD, '(6X, "nD =", I3)') nD
               WRITE(ICOD, '(6X, "nL =", I3)') nL
               WRITE(ICOD, '(6X, "nA =", I3)') nA
               WRITE(ICOD, '(6X, "nI =", I3)') nI
               !
               WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
               WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
               WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
               WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
               WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
               WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
               WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &            "CCLib_NComb(NOc, nI)")')
               !
               WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &                                     "CCLib_NComb(NVr, nB) * ", &
     &                                     "CCLib_NComb(NOc, nK) * ", &
     &                                     "CCLib_NComb(NVr, nC)")')
               !
               IF (NT_Typ3 >= 0) THEN
                  CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
               ELSE
                  CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
               END IF
               !
               IF ((nC == 0) .AND. (nK == 0)) GO TO 6000
               IF ((nC == 0) .AND. (nJ == 0)) GO TO 6000
               !
               nAI = 0
               CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               IF (NT_Typ3 >= 0) THEN
                  WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("T", ICOD)
               ELSE
                  WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
                  CALL FreeArray("R", ICOD)
               END IF
               GO TO 6100
 6000          CONTINUE
               IF (NT_Typ3 >= 0) THEN
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
                  END IF
                  CALL FreeArray("T", ICOD)
               ELSE
                  IF (Complx)THEN
                     WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
                  ELSE
                     WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
                  END IF
                  CALL FreeArray("R", ICOD)
               END IF
 6100          CONTINUE
!091112End
               !
               ! --- Contract V4_ to T(or R)(3)
               !
               IF (NT_Typ3 >= 0) THEN
                  WRITE(IOUT, '(5X, "V3 += T3(", I3, ")*V4")') NT_Typ3
                  WRITE(ICOD, '("!", 5("-"), X, "V3 += T3(", I3, ")*V4")') NT_Typ3
               ELSE
                  WRITE(IOUT, '(5X, "V3 += R3(", I4, ")*V4")') NT_Typ3
                  WRITE(ICOD, '("!", 5("-"), X, "V3 += R3(", I4, ")*V4")') NT_Typ3
               END IF
               !
               ! --- Load V3
               !
               CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
               !
               ! --- Load T amplitudes, Contraction: V4_*T += V3, Free T array
               !
               NTlevel = 3
               IF (NT_Typ3 >= 0) THEN
!GEMV                  M1Temp = M1(3,NumDig)
!GEMV                  CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
!GEMV                  CALL Contract("V4_", "T", "V3", NRank, NTlevel, &
!GEMV     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV                  CALL FreeArray("T", ICOD)
                     CALL ContractBLAS3("V4_", "T_", "V3", NRank, NTlevel, &
     &                  M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("T_", ICOD)
               ELSE
!GEMV                  M1Temp = IABS(M1(3,NumDig))
!GEMV                  CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')
!GEMV                  CALL Contract("V4_", "R", "V3", NRank, NTlevel, &
!GEMV     &                 M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV                  CALL FreeArray("R", ICOD)
                     CALL ContractBLAS3("V4_", "R_", "V3", NRank, NTlevel, &
     &                   M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
                     CALL FreeArray("R_", ICOD)
               END IF
               !
               ! --- Free V4_ array
               !
               CALL FreeArray("V4_", ICOD)
               !
               ! --- Save and free V3
               !
               CALL FreeVarray("V3", "NSizeV3", "V3", "SAVE", ICOD)
               !
               ! --- End of level-3 contraction
               !
            END DO   ! IT3
            !
            WRITE(IOUT, '(4X, "Re-arrange V3 array")')
            WRITE(ICOD, '("!", 5("-"), X, "Re-arrange V3 array")')
            CALL DefIndices(nB, nJ, nC, nK, nD, nL, nA, nI, NRank, &
     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), 2, ICOD)
            nAI = 99999
            IF ((nA == 0) .AND. (nI == 0)) nAI = 0
            !
            ! --- Allocate V3_ array
            !
            WRITE(ICOD, '(6X, "NSizeV3_ = mAI * mJ * mB * mK * mC")')
            WRITE(ICOD, '(6X, "ALLOCATE(V3_(NSizeV3_))")')
            !
            ! --- Allocate and load V3 intermediate
            !
            CALL GetVarray("V3", "NSizeV3", "V3", "LOAD", ICOD)
!091112Bgn
            IF ((nC /= 0) .OR. (nK /= 0)) THEN
!091112End
               !
               ! --- Make SUBROUTINE to rearrange V3 -> V3_
               !
               CALL MakeSrcArrange("V3", nB, nJ, nC, nK, nD, nL, nAI, CWrk)
               WRITE(ICOD, '(6X, "CALL", $)')
               WRITE(ICOD, *) TRIM(CWrk)//"( &"
               WRITE(ICOD, *) "     &   "//"V3, V3_, nB, nJ, nD, nL, mAI, mB, mJ, mD, mL, mC, mK)"
!091112Bgn
            ELSE
               IF (Complx) THEN
                  WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeV4, V4, 1, V4_, 1)")')
               ELSE
                  WRITE(ICOD, '(6X, "CALL DCOPY(NSizeV4, V4, 1, V4_, 1)")')
               END IF
            END IF
!091112End
            !
            CALL FreeVarray("V3", "NSizeV3", "V3", "NONE", ICOD)
            WRITE(ICOD, '("!")')
!091112Bgn
            !
            ! Load T amplitudes
            !
            NTlevel = 2
            IF (NT_Typ2 >= 0) THEN
               M1Temp = M1(2,NumDig)
               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
            ELSE
               M1Temp = IABS(M1(2,NumDig))
               CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')   !IPR
            END IF
            !
            WRITE(IOUT, '(4X, "Re-arrange T(2) amplitude")')
            !
            ! --- Re-arrange T (or R) if neccessary
            !
            nB = M_2
            nJ = L_2 - M_2
            IF (NT_Typ2 >= 0) THEN
               nC = N_2 - nB
            ELSE
               nC = (N_2 - 1) - nB   !IPR
            END IF
            nK = N_2 - nJ
            nD = nB + nC
            nL = nJ + nK
            nA = 0
            nI = 0
            !
            WRITE(ICOD, '(6X, "nB =", I3)') nB
            WRITE(ICOD, '(6X, "nJ =", I3)') nJ
            WRITE(ICOD, '(6X, "nC =", I3)') nC
            WRITE(ICOD, '(6X, "nK =", I3)') nK
            WRITE(ICOD, '(6X, "nD =", I3)') nD
            WRITE(ICOD, '(6X, "nL =", I3)') nL
            WRITE(ICOD, '(6X, "nA =", I3)') nA
            WRITE(ICOD, '(6X, "nI =", I3)') nI
            !
            WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
            WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
            WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
            WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
            WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
            WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
            WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &         "CCLib_NComb(NOc, nI)")')
            !
            WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &                                  "CCLib_NComb(NVr, nB) * ", &
     &                                  "CCLib_NComb(NOc, nK) * ", &
     &                                  "CCLib_NComb(NVr, nC)")')
            !
            IF (NT_Typ2 >= 0) THEN
               CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
            ELSE
               CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
            END IF
            !
            IF ((nC == 0) .AND. (nK == 0)) GO TO 7000
            IF ((nC == 0) .AND. (nJ == 0)) GO TO 7000
            !
            nAI = 0
            CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
            WRITE(ICOD, '(6X, "CALL", $)')
            WRITE(ICOD, *) TRIM(CWrk)//"( &"
            IF (NT_Typ2 >= 0) THEN
               WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
               CALL FreeArray("T", ICOD)
            ELSE
               WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
               CALL FreeArray("R", ICOD)
            END IF
            GO TO 7100
 7000       CONTINUE
            IF (NT_Typ2 >= 0) THEN
               IF (Complx)THEN
                  WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
               ELSE
                  WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
               END IF
               CALL FreeArray("T", ICOD)
            ELSE
               IF (Complx)THEN
                  WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
               ELSE
                  WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
               END IF
               CALL FreeArray("R", ICOD)
            END IF
 7100       CONTINUE
!091112End
            !
            ! --- Contract V3_ to T(or R)(2)
            !
            IF (NT_Typ2 >= 0) THEN
               WRITE(IOUT, '(3X, "V2 += T2(", I3, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += T2(", I3, ")*V3")') NT_Typ2
            ELSE
               WRITE(IOUT, '(3X, "V2 += R2(" ,I4, ")*V3")') NT_Typ2
               WRITE(ICOD, '("!", 5("-"), X, "V2 += R2(", I4, ")*V3")') NT_Typ2
            END IF
            !
            ! --- Load V2
            !
            CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
            !
            ! --- Load T amplitudes, Contraction: V3_*T += V2, Free T array
            !
            NTlevel = 2
            IF (NT_Typ2 >= 0) THEN
!GEMV               M1Temp = M1(2,NumDig)
!GEMV               CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
!GEMV               CALL Contract("V3_", "T", "V2", NRank, NTlevel, &
!GEMV     &              M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV               CALL FreeArray("T", ICOD)
               CALL ContractBLAS3("V3_", "T_", "V2", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("T_", ICOD)
            ELSE
!GEMV               M1Temp = IABS(M1(2,NumDig))
!GEMV               CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, "R")
!GEMV               CALL Contract("V3_", "R", "V2", NRank, NTlevel, &
!GEMV     &              M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV               CALL FreeArray("R", ICOD)
               CALL ContractBLAS3("V3_", "R_", "V2", NRank, NTlevel, &
     &            M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
               CALL FreeArray("R_", ICOD)
            END IF
            !
            ! --- Free V3_ array
            !
            CALL FreeArray("V3_", ICOD)
            !
            ! --- Save and free V2
            !
            CALL FreeVarray("V2", "NSizeV2", "V2", "SAVE", ICOD)
            !
            ! --- End of level-2 contraction
            !
         END DO   ! IT2
         !
         ! --- Contract V2 to T(1)
         !
         IF (NT_Typ1 >= 0) THEN
            WRITE(IOUT, '(2X, "V1 += T1(", I3, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += T1(", I3, ")*V2")') NT_Typ1
         ELSE
            WRITE(IOUT, '(2X, "V1 += R1(", I4, ")*V2")') NT_Typ1
            WRITE(ICOD, '("!", 5("-"), X, "V1 += R1(", I4, ")*V2")') NT_Typ1
         END IF
!091112Bgn
         !
         ! Load T amplitudes
         !
         NTlevel = 1
         IF (NT_Typ1 >= 0) THEN
            M1Temp = M1(1,NumDig)
            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, 'T')
         ELSE
            M1Temp = IABS(M1(1,NumDig))
            CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, 'R')   ! IPR
         END IF
         !
         WRITE(IOUT, '(4X, "Re-arrange T(1) amplitude")')
         !
         ! --- Re-arrange T if neccessary
         !
         nB = M_1
         nJ = L_1 - M_1
         IF (NT_Typ1 >= 0) THEN
            nC = N_1 - nB
         ELSE
            nC = (N_1 - 1) - nB   ! IPR
         END IF
         nK = N_1 - nJ
         nD = nB + nC
         nL = nJ + nK
         nA = 0
         nI = 0
         !
         WRITE(ICOD, '(6X, "nB =", I3)') nB
         WRITE(ICOD, '(6X, "nJ =", I3)') nJ
         WRITE(ICOD, '(6X, "nC =", I3)') nC
         WRITE(ICOD, '(6X, "nK =", I3)') nK
         WRITE(ICOD, '(6X, "nD =", I3)') nD
         WRITE(ICOD, '(6X, "nL =", I3)') nL
         WRITE(ICOD, '(6X, "nA =", I3)') nA
         WRITE(ICOD, '(6X, "nI =", I3)') nI
         !
         WRITE(ICOD, '(6X, "mB = CCLib_NComb(NVr, nB)")') 
         WRITE(ICOD, '(6X, "mJ = CCLib_NComb(NOc, nJ)")') 
         WRITE(ICOD, '(6X, "mC = CCLib_NComb(NVr, nC)")') 
         WRITE(ICOD, '(6X, "mK = CCLib_NComb(NOc, nK)")') 
         WRITE(ICOD, '(6X, "mD = CCLib_NComb(NVr, nD)")') 
         WRITE(ICOD, '(6X, "mL = CCLib_NComb(NOc, nL)")') 
         WRITE(ICOD, '(6X, "mAI = CCLib_NComb(NVr, nA) * ", &
     &      "CCLib_NComb(NOc, nI)")')
         !
         WRITE(ICOD, '(6X, "NSizeT_ = CCLib_NComb(NOc, nJ) * ", &
     &                               "CCLib_NComb(NVr, nB) * ", &
     &                               "CCLib_NComb(NOc, nK) * ", &
     &                               "CCLib_NComb(NVr, nC)")')
         !
         IF (NT_Typ1 >= 0) THEN
            CALL GetVarray("T_", "NSizeT_", " ", "NONE", ICOD)
         ELSE
            CALL GetVarray("R_", "NSizeT_", " ", "NONE", ICOD)
         END IF
         !
         IF ((nC == 0) .AND. (nJ == 0)) GO TO 8000
         IF ((nC == 0) .AND. (nK == 0)) GO TO 8000
         !
         nAI = 0
         CALL MakeSrcArrange("T", nC, nK, nB, nJ, nD, nL, nAI, CWrk)
         WRITE(ICOD, '(6X, "CALL", $)')
         WRITE(ICOD, *) TRIM(CWrk)//"( &"
         IF (NT_Typ1 >= 0) THEN
            WRITE(ICOD, *) "     &   "//"T, T_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
            CALL FreeArray("T", ICOD)
         ELSE
            WRITE(ICOD, *) "     &   "//"R, R_, nC, nK, nD, nL, mAI, mC, mK, mD, mL, mB, mJ)"
            CALL FreeArray("R", ICOD)
         END IF
         GO TO 8100
 8000    CONTINUE
         IF (NT_Typ1 >= 0) THEN
            IF (Complx)THEN
               WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, T, 1, T_, 1)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, T, 1, T_, 1)")')
            END IF
            CALL FreeArray("T", ICOD)
         ELSE
            IF (Complx) THEN
               WRITE(ICOD, '(6X, "CALL ZCOPY(NSizeT, R, 1, R_, 1)")')
            ELSE
               WRITE(ICOD, '(6X, "CALL DCOPY(NSizeT, R, 1, R_, 1)")')
            END IF
            CALL FreeArray("R", ICOD)
         END IF
 8100    CONTINUE
!072112End
         !
         ! --- Load V1 and V2
         !
         CALL GetVarray("V1", "NSizeV1", "V1", "LOAD", ICOD)
         CALL GetVarray("V2", "NSizeV2", "V2", "LOAD", ICOD)
         !
         ! --- Load T amplitudes, Contraction: V2*T += V1, free T array
         !
         NTlevel = 1
         IF (NT_Typ1 >= 0) THEN
!GEMV            M1Temp = M1(1,NumDig)
!GEMV            CALL GetAmp(M1Temp, M1Temp, M1Temp, ICOD, "T")
!GEMV            CALL Contract("V2", "T", "V1", NRank, NTlevel, &
!GEMV     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV            CALL FreeArray("T", ICOD)
            CALL ContractBLAS3("V2", "T_", "V1", NRank, NTlevel, &
     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
            CALL FreeArray("T_", ICOD)
         ELSE
!GEMV            M1Temp = IABS(M1(1,NumDig))
!GEMV            CALL GetAmp(M1Temp, M1Temp, (M1Temp-1), ICOD, "R")
!GEMV            CALL Contract("V2", "R", "V1", NRank, NTlevel, &
!GEMV     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
!GEMV            CALL FreeArray("R", ICOD)
            CALL ContractBLAS3("V2", "R_", "V1", NRank, NTlevel, &
     &           M1(1,NumDig), M2(1,NumDig), M3(1,NumDig), M13(NumDig), ICOD)
            CALL FreeArray("R_", ICOD)
         END IF
         !
         ! --- Free V2 array
         !
         CALL FreeVarray("V2", "NSizeV2", "V2", "NONE", ICOD)
         !
         ! --- Save and free V1
         !
         CALL FreeVarray("V1", "NSizeV1", "V1", "SAVE", ICOD)
         !
         ! --- End of level-1 contraction
         !
      END DO   ! IT1
      !
      WRITE(IOUT, '(/, X, I5, X, "diagrams have been processed")') NumDig
      WRITE(ICOD, '(/, "!", 5("-"), X, I5, X, "diagrams have been processed")') NumDig
      !
      IF (NumDig /= NDig) THEN
         WRITE(IOUT, '(3X, "!!! NumDig is not equal to NDig !!!")')
         WRITE(*, '(3X, "!!! NumDig is not equal to NDig !!!",2I6)') NumDig, NDig
         STOP
      END IF
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE ContractL1(VName, TName, WName, NRank, NT, M1, M2, M3, M13, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Contract "VName" to Lambda --> Generate "WName"
        !
        !
        ! This version contracts lambda (or any de-excitation operator) to 
        ! an intermediate "VName" assuming the lambda is the last vertex to be 
        ! contracted. (i.e. L1T2T3T4V)
        !
        ! nC and nK are always zero.
        !
        ! NT ... Level of L vertex (1 to 4; always 1 in this case)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : LOff
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      CHARACTER(*), INTENT(IN):: VName, WName, TName
      INTEGER, INTENT(IN) :: NT, NRank, M1(4), M2(4), M3(4), M13, ICOD
      !
      INTEGER :: IT
      INTEGER :: M1_(4), M2_(4), M3_(4)
      INTEGER :: nAt, nIt, nB, nJ, nC, nK, nD, nL, nAo, nIo, nAn, nIn
      CHARACTER(LEN=255) :: CWrk
      !
!!!
!      WRITE(ICOD, '("Check")')
!      WRITE(ICOD, '("T vertex type: ", 3I2)') M1(NT), M2(NT), M3(NT)
!!!
      DO IT = 1, 4
         M1_(IT) = IABS(M1(IT))
         M2_(IT) = IABS(M2(IT))
         M3_(IT) = IABS(M3(IT))
         !
         IF (M1_(IT) >= LOff) THEN
            M1_(IT) = M1_(IT) - LOff
            M2_(IT) = M2_(IT) - LOff
            M3_(IT) = M3_(IT) - LOff
         END IF
      END DO
      !
      ! o Define indices
      !
      nAn = M1_(NT) - M3_(NT)
      nIn = M1_(NT) - (M2_(NT) - M3_(NT))
      nB = NIntPart(M13)
      nJ = NIntHole(M13)
      DO IT = (NT + 1), 4
         nB = nB - M3_(IT)
         nJ = nJ - (M2_(IT) - M3_(IT))
      END DO
      nAo = NExtPart(M13)
      nIo = NExtHole(M13)
      DO IT = (NT + 1), 4
         nAo = nAo + (M1_(IT) - M3_(IT))
         nIo = nIo + (M1_(IT) - (M2_(IT) - M3_(IT)))
      END DO
      nD = nAo + nAn
      nL = nIo + nIn
      !
      WRITE(ICOD, '( &
     &   6X, "nAo = ", I2, /, &
     &   6X, "nIo = ", I2, /, &
     &   6X, "nB  = ", I2, /, &
     &   6X, "nJ  = ", I2, /, &
     &   6X, "nD  = ", I2, /, &
     &   6X, "nL  = ", I2, /, &
     &   6X, "nAn = ", I2, /, &
     &   6X, "nIn = ", I2, /, &
     &   6X, "mB  = CCLib_NComb(NVr, nB)", /, &
     &   6X, "mJ  = CCLib_NComb(NOc, nJ)", /, &
     &   6X, "mD  = CCLib_NComb(NVr, nD)", /, &
     &   6X, "mL  = CCLib_NComb(NOc, nL)", /, &
     &   6X, "mAo = CCLib_NComb(NVr, nAo)", /, &
     &   6X, "mIo = CCLib_NComb(NOc, nIo)", /, &
     &   6X, "mAn = CCLib_NComb(NVr, nAn)", /, &
     &   6X, "mIn = CCLib_NComb(NOc, nIn)")') &
     &   nAo, nIo, nB, nJ, nD, nL, nAn, nIn
      !
      ! --- Make SUBROUTINE for L1 contraction
      !
      CALL MakeSrcContrctL1(nAo, nIo, nAn, nIn, nB, nJ, CWrk)
      !
      ! --- CALL L1 contraction SUBROUTINE
      !
      WRITE(ICOD, '(6X, "CALL", $)')
!      WRITE(ICOD, *) TRIM(CWrk)//"("//TRIM(VName)//",T,"
!     *   //TRIM(WName)//",nAo,nIo,nAt,",
!     *   "nIt,nB,nJ,mCK,mB,mJ,mAo,mIo,mD,mL,mAn,mIn)"
      WRITE(ICOD, *) TRIM(CWrk)//"( &"
      WRITE(ICOD, *) "     &   " &
     &   //TRIM(VName)//", "//TRIM(TName)//", "//TRIM(WName)//", nAo, nIo, nAn, ", &
     &   "nIn, nB, nJ, mB, mJ, mAo, mIo, mD, mL, mAn, mIn)"
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MakeSrcContrctL1(nAo, nIo, nAn, nIn, nB, nJ, RtnName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Generate a SUBROUTINE which contracts an intermedeate V(BJAI)
        !     with a Lambda vertex
        !
        !     In:  nAo and nIo ... # of external lines of the given intermediate
        !          nC and nK ... # of free internal lines
        !
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx, PreFixSrc
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: nAo, nIo, nAn, nIn, nB, nJ
      CHARACTER(LEN=255), INTENT(OUT) :: RtnName
      !
      CHARACTER(LEN=2) :: a21, a22, a23, a24, A25, A26
      CHARACTER(LEN=7) :: LpName1(100), LpName2(100)
      CHARACTER(LEN=255) :: BasName, RtnName_lw
      LOGICAL :: Finish, Vector
      INTEGER :: IOUT, ICOD1, ICOD2
      INTEGER :: iIn, iAn, iB, iJ, iAo, iIo
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      ! --- Name of the SUBROUTINE
      !
      BasName = TRIM(PreFixSrc)//"ContractL1"
      !
      CALL Int2Char(nAo, a21, "0")
      CALL Int2Char(nIo, a22, "0")
      CALL Int2Char(nAn, a23, "0")
      CALL Int2Char(nIn, a24, "0")
      CALL Int2Char(nJ, a25, "0")
      CALL Int2Char(nB, a26, "0")
      RtnName = TRIM(BasName)//"_Ao"//a21//"Io"//a22//"An"//a23//"In"//a24//"J"//a25//"B"//a26
      !
      Vector = ((nB /= 0) .OR. (nJ /= 0))
      IF (.NOT. Vector) THEN
         RtnName = TRIM(RtnName)//"S"   ! C and K are zero (scalar)
      ELSE
         RtnName = TRIM(RtnName)//"V"   ! C and/or K is non-zero (vector)
      END IF
      !
      RtnName = TRIM(RtnName)//"_AutoGen"
      !
      ! --- Convert to lower case for filename
      !
      RtnName_lw = RtnName
      CALL Up2Lw(RtnName_lw)
      !
      OPEN(UNIT=ICOD2, FILE=TRIM(RtnName_lw)//".f90", STATUS='UNKNOWN')
      REWIND(ICOD2)
      !
      ! --- Header
      !
      WRITE(ICOD2, '(6X, "SUBROUTINE ", $)')
      WRITE(ICOD2, *) TRIM(RtnName)//" &"
      WRITE(ICOD2, *) "     &   "// &
     &   "(V, T, W, nAo, nIo, nAn, nIn, nB, nJ, mB, mJ, mAo, ", &
     &   "mIo, mD, mL, mAn, mIn)"
      WRITE(ICOD2, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!", /, &
     &   "!", 5X, "Contract intermediate V(B,J,Ao,Io) to T(D,L)", /, &
     &   "!", 5X, "to generate a new intermediate W(C,K,An,In)", /, &
     &   "!" &
     &   )')
      !
      WRITE(ICOD2, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : OneC")')
      ELSE
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : One")')
      END IF
      !
      ! --- Declare local variables
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(/, 6X, "IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "COMPLEX(8):: V(mB*mJ,mAo,mIo), T(mD,mL)", &
     &      ", W(mB*mJ,mAn,mIn), F(mAo,mIo), Alpha", /, &
     &      6X, "INTEGER:: NStringB(nB), NStringJ(nJ)", &
     &      ", NStringD(nAo+nAn), NStringL(nIo+nIn)", /, &
     &      6X, "INTEGER:: NStringAn(nAn), NStringIn(nIn), ", &
     &      "NStringAo(nAo), NStringIo(nIo)", /)')
      ELSE
         WRITE(ICOD2, '(/, 6X, "IMPLICIT DOUBLE PRECISION(A-H, O-Z)", /, &
     &      "!", /, &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "REAL(8):: V(mB*mJ,mAo,mIo), T(mD,mL)", &
     &      ", W(mB*mJ,mAn,mIn), F(mAo,mIo), Alpha", /, &
     &      6X, "INTEGER:: NStringB(nB), NStringJ(nJ)", &
     &      ", NStringD(nAo+nAn), NStringL(nIo+nIn)", /, &
     &      6X, "INTEGER:: NStringAn(nAn), NStringIn(nIn), ", &
     &      "NStringAo(nAo), NStringIo(nIo)", /)')
      END IF
      !
      ! --- Loop over In (Uncontracted ext. holes in T)
      !
      IF (nIn /= 0) THEN
         !   
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nIn, a21, "0")
         WRITE(ICOD2, '(6X, "DO iIn00 = ", I2, ", NOc", $)') nIn
         WRITE(ICOD2, '("   ! loop over uncontracted ext. hole(s) in T-vertex")')
         WRITE(ICOD2, '(9X, "NStringIn(", I2, ") = iIn00")') nIn
         DO iIn = nIn - 1, 1, -1
            CALL Int2Char(nIn - iIn, a21, "0")
            CALL Int2Char(nIn - iIn - 1, a22, "0")
            CALL Int2Char(iIn, a23, " ")
            WRITE(ICOD2, '(6X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIn"//a21//" = "//a23//", iIn"//a22//" - 1")
            WRITE(ICOD2, '(9X, "NStringIn(", I2, ") = ", $)') iIn
            WRITE(ICOD2, *) TRIM("iIn"//a21)
         END DO
         !
         WRITE(ICOD2, '(9X, &
     &      "CALL CCLib_StringAddress(NStringIn, nIn, ", &
     &      """Occ"", IAddIn)")')
         !
      ELSE
         !
         WRITE(ICOD2, '(9X, &
     &      "IAddIn = 1")')
         !         
      END IF
      !
      ! --- Loop over An (Uncontracted ext. particles in T)
      !
      IF (nAn /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nAn, a21, "0")
         WRITE(ICOD2, '(9X, "DO iAn00 = ", I2, ", NVr", $)') nAn
         WRITE(ICOD2, '("   ! loop over uncontracted ext. particle(s) in T-vertex")')
         WRITE(ICOD2, '(12X, "NStringAn(", I2, ") = iAn00")') nAn
         DO iAn = (nAn - 1), 1, -1
            CALL Int2Char((nAn-iAn), a21, "0")
            CALL Int2Char((nAn-iAn-1), a22, "0")
            CALL Int2Char(iAn, a23, " ")
            WRITE(ICOD2, '(9X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAn"//a21//" = "//a23//", iAn"//a22//" - 1")
            WRITE(ICOD2, '(12X, "NStringAn(", I2, ") = ", $)') iAn
            WRITE(ICOD2, *) TRIM("iAn"//a21)
         END DO
         !
         WRITE(ICOD2, '(12X, &
     &      "CALL CCLib_StringAddress(NStringAn, nAn, ", &
     &      """Vir"", IAddAn)")')
         !
      ELSE
         !
         WRITE(ICOD2, '(12X, &
     &      "IAddAn = 1")')
         !
      END IF
      !
      ! --- Zero-clear work array
      !      
      IF (Complx) THEN
         WRITE(ICOD2, '(/, 12X, "CALL CCLib_ZClear(F, (mAo*mIo))")')
      ELSE
         WRITE(ICOD2, '(/, 12X, "CALL CCLib_DClear(F, (mAo*mIo))")')
      END IF
      !
      ! --- Loop over Io (External holes which are contracted)
      !
      IF (nIo /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nIo, a21, "0")
         WRITE(ICOD2, '(12X, "DO iIo00 = ", I2, ", NOc ", $)') nIo
         WRITE(ICOD2, '("   ! loop over ext. hole(s) which are contracted")')
         WRITE(ICOD2, '(15X, "NStringIo(",I2,") = iIo00")') nIo
         DO iIo = (nIo - 1), 1, -1
            CALL Int2Char((nIo - iIo), a21, "0")
            CALL Int2Char((nIo - iIo - 1), a22, "0")
            CALL Int2Char(iIo, a23, " ")
            WRITE(ICOD2, '(12X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIo"//a21//" = "//a23//",iIo"//a22//" - 1")
            WRITE(ICOD2, '(15X, "NStringIo(", I2, ") = ", $)') iIo
            WRITE(ICOD2, *) TRIM("iIo"//a21)
         END DO
!
         IF (nIn /= 0) THEN
            WRITE(ICOD2, '(15X, "COIN = .FALSE.", /, &
     &         15X, "CALL CCLib_MergeStrings(NStringIo, NStringIn, ", &
     &         "NStringL, nIo, nIn, ISgnL, COIN)", /, &
     &         15X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         15X, "CALL CCLib_StringAddress(NStringL, nIo+nIn, ", &
     &         """Occ"", IAddL)", /, &
     &         15X, "CALL CCLib_StringAddress(NStringIo, nIo, ", &
     &         """Occ"", IAddIo)")')
         ELSE
            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIo, nIo, ", """Occ"", IAddIo)")')
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X,"IAddL = IAddIo")')
         END IF
         !
      ELSE   ! nIo == 0
         !
         WRITE(ICOD2, '(15X, "IAddIo = 1", /)')
         IF (nIn /= 0) THEN
!!!            WRITE(ICOD2, '(15X, &
!!!     &         "CALL CCLib_StringAddress(NStringIn, nIn, ", &
!!!     &         """Occ"", IAddIn)")')
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X, "IAddL = IAddIn")')
         ELSE
            WRITE(ICOD2, '(15X, "ISgnL = 1", /, 15X, "IAddL = 1")')
         END IF
         !
      END IF   ! nIo = 0 or not
      !
      ! --- Loop over Ao (Ext. particles which are contracted)
      !
      IF (nAo /= 0) THEN
         !
         WRITE(ICOD2, '(/)')
         CALL Int2Char(nAo, a21, "0")
         WRITE(ICOD2, '(15X, "DO iAo00 = ", I2, ", NVr", $)') nAo
         WRITE(ICOD2, '("   ! loop over ext. particle(s) which are contracted")')
         WRITE(ICOD2, '(18X, "NStringAo(", I2, ") = iAo00")') nAo
         DO iAo = (nAo - 1), 1, -1
            CALL Int2Char((nAo - iAo), a21, "0")
            CALL Int2Char((nAo - iAo - 1), a22, "0")
            CALL Int2Char(iAo, a23, " ")
            WRITE(ICOD2, '(15X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAo"//a21//" = "//a23//",iAo"//a22//" - 1")
            WRITE(ICOD2, '(18X, "NStringAo(", I2, ") = ", $)') iAo
            WRITE(ICOD2, *) TRIM("iAo"//a21)
         END DO
         !
         IF (nAn /= 0) THEN
            WRITE(ICOD2, '(18X, "COIN = .FALSE.", /, &
     &         18X, "CALL CCLib_MergeStrings", &
     &         "(NStringAo, NStringAn, NStringD, nAo, nAn, ISgnD, COIN)", /, &
     &         18X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         18X, "CALL CCLib_StringAddress(NStringD, nAo+nAn, ", """Vir"", IAddD)", /, &
     &         18X, "CALL CCLib_StringAddress(NStringAo, nAo, ", """Vir"", IAddAo)")')
         ELSE
            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAo, nAo, ", """Vir"", IAddAo)")')
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, 18X, "IAddD = IAddAo")')
         END IF
         !
      ELSE   ! nAo == 0
         !
         WRITE(ICOD2, '(18X, "IAddAo = 1", /)')
         IF (nAn /= 0) THEN
!!!            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAn, nAn, ", """Vir"", IAddAn)")')
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, 18X, "IAddD = IAddAn")')
         ELSE
            WRITE(ICOD2, '(18X, "ISgnD = 1", /, &
     &         18X, "IAddD = 1")')
         END IF
         !
      END IF
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(18X, "F(IAddAo,IAddIo) = DCMPLX(ISgnL * ISgnD) * T(IAddD,IAddL)")')
      ELSE
         WRITE(ICOD2, '(18X, "F(IAddAo,IAddIo) = DBLE(ISgnL * ISgnD) * T(IAddD,IAddL)")')
      END IF
      !
      ! --- Close loops over contracted ext. indices
      !
      IF (nAo /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(15X, "END DO   ! Contracted ext. particles")')
         DO iAo = 2, nAo
            WRITE(ICOD2, '(15X, "END DO")')
         END DO
      END IF
      !
      IF (nIo /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(12X, "END DO   ! Contracted ext. holes")')
         DO iIo = 2, nIo
            WRITE(ICOD2, '(12X, "END DO")')
         END DO
      END IF
!L1      !
!L1      ! --- Loop over internal holes J of intermediate
!L1      !
!L1      IF (nJ /= 0) THEN
!L1         !
!L1         WRITE(ICOD2, '(/)')
!L1         CALL Int2Char(nJ, a21, "0")
!L1         WRITE(ICOD2, '(12X, "DO iJ00 = ", I2, ", NOc ", $)') nJ
!L1         WRITE(ICOD2, '("   ! loop over internal hole(s) in V")')
!L1         WRITE(ICOD2, '(15X, "NStringJ(", I2, ") = iJ00")') nJ
!L1         DO iJ = nJ - 1, 1, -1
!L1            CALL Int2Char(nJ - iJ, a21, "0")
!L1            CALL Int2Char(nJ - iJ - 1, a22, "0")
!L1            CALL Int2Char(iJ, a23, " ")
!L1            WRITE(ICOD2, '(12X, "DO ", $)')
!L1            WRITE(ICOD2, *) TRIM("iJ"//a21//" = "//a23//", iJ"//a22//" - 1")
!L1            WRITE(ICOD2, '(15X, "NStringJ(", I2, ") = ", $)') iJ
!L1            WRITE(ICOD2, *) TRIM("iJ"//a21)
!L1         END DO
!L1         !
!L1         IF (nIt /= 0) THEN
!L1            WRITE(ICOD2, '( &
!L1     &         15X, "COIN = .FALSE.", /, &
!L1     &         15X, "CALL CCLib_MergeStrings(NStringJ, NStringIt, ", &
!L1     &         "NStringIn, nJ, nIt, ISgnIn, COIN)", /, &
!L1     &         15X, "IF (COIN) CYCLE")')
!L1            WRITE(ICOD2, '(15X, &
!L1     &         "CALL CCLib_StringAddress(NStringIn, nJ+nIt, ""Occ"", IAddIn)", /, &
!L1     &         15X, "CALL CCLib_StringAddress(NStringJ, nJ, ""Occ"", IAddJ)")')
!L1         ELSE   ! nIt = 0
!L1            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringJ, nJ, ""Occ"", IAddJ)")')
!L1            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
!L1     &         15X, "IAddIn = IAddJ")')
!L1         END IF
!L1         !
!L1      ELSE   ! nJ == 0
!L1         !
!L1         WRITE(ICOD2, '(15X, "IAddJ = 1")')
!L1         IF (nIt /= 0) THEN
!L1            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIt, nIt, ""Occ"", IAddIt)")')
!L1            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
!L1     &         15X, "IAddIn = IAddIt")')
!L1         ELSE
!L1            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
!L1     &         15X, "IAddIn = 1")')
!L1         END IF
!L1         !
!L1      END IF
!L1      !
!L1      ! --- Loop over internal (external in lambda diagram) particles B
!L1      !
!L1      IF (nB /= 0) THEN
!L1         !
!L1         WRITE(ICOD2, '(/)')
!L1         CALL Int2Char(nB, a21, "0")
!L1         WRITE(ICOD2, '(15X,"DO iB00 = ",I2,", NVr",$)') nB
!L1         WRITE(ICOD2, '("   ! loop over external particle(s) in V")')
!L1         WRITE(ICOD2, '(18X,"NStringB(",I2,") = iB00")') nB
!L1         DO iB = nB-1, 1, -1
!L1            CALL Int2Char(nB-iB, a21, "0")
!L1            CALL Int2Char(nB-iB-1, a22, "0")
!L1            CALL Int2Char(iB, a23, " ")
!L1            WRITE(ICOD2, '(15X,"DO ",$)')
!L1            WRITE(ICOD2, *) TRIM("iB"//a21//" = "//a23//", iB"//a22//" - 1")
!L1            WRITE(ICOD2, '(18X,"NStringB(",I2,") = ",$)') iB
!L1            WRITE(ICOD2, *) TRIM("iB"//a21)
!L1         END DO
!L1!
!L1         IF (nAt /= 0) THEN
!L1            WRITE(ICOD2, '( &
!L1     &         18X,"COIN = .FALSE.",/, &
!L1     &         18X,"CALL CCLib_MergeStrings(NStringB, NStringAt, ", &
!L1     &         "NStringAn, nB, nAt, ISgnAn, COIN)",/, &
!L1     &         18X,"IF (COIN) CYCLE")')
!L1            WRITE(ICOD2, '(18X, &
!L1     &         "CALL CCLib_StringAddress(NStringAn, nB+nAt, ", &
!L1     &         """Vir"", IAddAn)",/, &
!L1     &         18X,"CALL CCLib_StringAddress(NStringB, nB, ", &
!L1     &         """Vir"", IAddB)")')
!L1         ELSE
!L1            WRITE(ICOD2, '(18X, &
!L1     &         "CALL CCLib_StringAddress(NStringB, nB, ", &
!L1     &         """Vir"", IAddB)")')
!L1            WRITE(ICOD2, '(18X,"ISgnAn = 1",/, &
!L1     &         18X,"IAddAn = IAddB")')
!L1         END IF
!L1
!L1      ELSE   ! nB = 0
!L1
!L1         WRITE(ICOD2, '(18X, "IAddB = 1")')
!L1         IF (nAt /= 0) THEN
!L1            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAt, nAt, ""Vir"", IAddAt)")')
!L1            WRITE(ICOD2, '(18X,"ISgnAn = 1",/, &
!L1     &         18X,"IAddAn = IAddAt")')
!L1         ELSE
!L1            WRITE(ICOD2, '(18X,"ISgnAn = 1",/, &
!L1     &         18X,"IAddAn = 1")')
!L1         END IF
!L1         !
!L1      END IF
      !
      ! --- Contract V to T
      !
      IF (Complx) THEN
         WRITE(ICOD2, '(18X,"Alpha = OneC")')
         WRITE(ICOD2, '(18X,"CALL ZGEMV(""N"", mB*mJ, mAo*mIo, ", &
     &        "Alpha, V, mB*mJ, F, ", &
     &        "1, OneC, W(1,IAddAn,IAddIn), 1)")')
      ELSE
         WRITE(ICOD2, '(18X,"Alpha = One")')
         WRITE(ICOD2, '(18X,"CALL DGEMV(""N"", mB*mJ, mAo*mIo, ", &
     &        "Alpha, V, mB*mJ, F, ", &
     &        "1, One, W(1,IAddAn,IAddIn), 1)")')
      END IF
      !
      ! --- Close loops
      !
      IF (nAn /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(6X, "END DO   ! Uncontracted ext. particles")')
         DO iAn = 2, nAn
            WRITE(ICOD2, '(6X, "END DO")')
         END DO
      END IF
      !
      IF (nIn /= 0) THEN
         WRITE(ICOD2, '(/)')
         WRITE(ICOD2, '(9X, "END DO   ! Uncontracted ext. holes")')
         DO iIn = 2, nIn
            WRITE(ICOD2, '(9X, "END DO")')
         END DO
      END IF
      !
      ! --- End of the SUBROUTINE
      !
      WRITE(ICOD2, '("!", /, 6X, "END SUBROUTINE")')
      !
      CLOSE(ICOD2)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE DefIndicesL1(nB, nJ, nA, nI, NRank, M1, M2, M3, M13, NLLev, ICOD)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ! Define indices for contraction V2*L = V1_
        !  nB, nJ ... # of uncontracted internal lines in interaction vertex
        !  nA, nI ... # of uncontracted external lines in L vertex
        !
        ! * Note: Only NLLev = 1 is available
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : LOff
      USE Vertex_Module, ONLY : NIntPart, NIntHole, NExtPart, NExtHole
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: M1(4), M2(4), M3(4), M13, NRank, ICOD, NLLev
      INTEGER, INTENT(OUT) :: nB, nJ, nA, nI
      !
      INTEGER:: M1_(4), M2_(4), M3_(4), IT, I
      !
      IF (NLLev /= 1) STOP 'NLLev must be 1 in DefIndicesL1'
      !
      DO IT = 1, 4
         M1_(IT) = IABS(M1(IT))
         M2_(IT) = IABS(M2(IT))
         M3_(IT) = IABS(M3(IT))
      END DO
      !
      nB = NIntPart(M13)
      nJ = NIntHole(M13)
      DO I = 4, (NLLev + 1), -1
         nB = nB - M3_(I)
         nJ = nJ - (M2_(I) - M3_(I))
      END DO
      nA = (M1_(NLLev) - LOff) - NExtPart(M13)
      nI = (M1_(NLLev) - LOff) - NExtHole(M13)
      DO I = 4, (NLLev + 1), -1
         nA = nA - (M1_(I) - M3_(I))
         nI = nI - (M1_(I) - (M2_(I) - M3_(I)))
      END DO
      !
      WRITE(ICOD, '("!",/, &
     &   "!", 5X, "nA, nI ... # of uncontracted ext. lines of L vertex" &
     &   , /, "!",5X,"nB, nJ ... Uncontracted int. lines of interaction vertex" &
     &   , /, "!")')
      !
      WRITE(ICOD, '(6X, &
     &   "nA = ",I3,/, 6X, &
     &   "nI = ",I3,/, 6X, &
     &   "nB = ",I3,/, 6X, &
     &   "nJ = ",I3,/, 6X, &
     &   "mA = CCLib_NComb(NVr, nA)", /, 6X, &
     &   "mI = CCLib_NComb(NOc, nI)", /, 6X, &
     &   "mB = CCLib_NComb(NVr, nB)", /, 6X, &
     &   "mJ = CCLib_NComb(NOc, nJ)", /, 6X  &
     &   )') &
     &   nA, nI, nB, nJ
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiagZeta(NDig, NMax, NRank, M1,  M2,  M3,  M13,  Mwp, &
     &   NDigZ, M1Z, M2Z, M3Z, M13Z, MwpZ)
        !**************************************************************************
        !     Generate diagrams for zeta diagrams
        !**************************************************************************
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: NDig, NMax, NRank
      INTEGER, INTENT(IN) :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      INTEGER, INTENT(OUT) :: NDigZ, M1Z(4,*), M2Z(4,*), M3Z(4,*), M13Z(*), MwpZ(*)
      !
      INTEGER :: IDig, IT, JT, M1T, M2T, M3T, M1T_Old, M2T_Old, M3T_Old, IOUT
      !
      COMMON/IOFILE/IOUT
      !
      NDigZ = 0
      !
      DO IDig = 1, NDig
         M1T_Old = 0
         M2T_Old = 0
         M3T_Old = 0
         DO IT = 2, 4
            IF (M1(IT,IDig) == 0) EXIT
            M1T = M1(IT,IDig)
            M2T = M2(IT,IDig)
            M3T = M3(IT,IDig)
            IF (M1T == M1T_Old .AND. M2T == M2T_Old .AND. M3T == M3T_Old) CYCLE
            M1T_Old = M1T
            M2T_Old = M2T
            M3T_Old = M3T
            !
            NDigZ = NDigZ + 1
            !
            ! --- Replace T by R
            !
            M1Z(IT,NDigZ) = -M1T
            M2Z(IT,NDigZ) = -M2T
            M3Z(IT,NDigZ) = -M3T
            DO JT = 1, (IT - 1)
               M1Z(JT,NDigZ) = M1(JT,IDig)
               M2Z(JT,NDigZ) = M2(JT,IDig)
               M3Z(JT,NDigZ) = M3(JT,IDig)
            END DO
            DO JT = (IT + 1), 4
               M1Z(JT,NDigZ) = M1(JT,IDig)
               M2Z(JT,NDigZ) = M2(JT,IDig)
               M3Z(JT,NDigZ) = M3(JT,IDig)
            END DO
            M13Z(NDigZ) = M13(IDig)
            MWpZ(NDigZ) = Mwp(IDig)
            !
         END DO
      END DO
      !
      WRITE(IOUT, '("Zeta diagrams")')
      DO IDig = 1, NDigZ
         WRITE(IOUT, '(X, I3, X, 3I5, "/", 3I2, "/", 3I2, "/", 3I2, "/", I3, I3)') &
     &      IDig, &
     &      M1Z(1,IDig), M2Z(1,IDig), M3Z(1,IDig), &
     &      M1Z(2,IDig), M2Z(2,IDig), M3Z(2,IDig), &
     &      M1Z(3,IDig), M2Z(3,IDig), M3Z(3,IDig), &
     &      M1Z(4,IDig), M2Z(4,IDig), M3Z(4,IDig), &
     &      M13Z(IDig), MwpZ(IDig)
      END DO
      FLUSH(IOUT)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenDiagDM_Common(NDigTotal, NMax, NRankParent, IType, &
     &   M1P, M2P, M3P, M13P, MwpP, NDigDM, M1DM, M2DM, M3DM, MwpDM)
        !**************************************************************************
        !     Generate response density-matrix diagrams 
        !**************************************************************************
        !
      USE BasicInformation, ONLY : LOff
      USE Vertex_Module, ONLY : NExtPart, NExtHole, NIntHole, NIntPart
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(IN) :: NDigTotal, NMax, NRankParent(*), IType, &
     &   M1P(4,*), M2P(4,*), M3P(4,*), M13P(*), MwpP(*)
      INTEGER :: NDigDM, M1DM(5,*), M2DM(5,*), M3DM(5,*), MwpDM(*)
      !
      INTEGER :: NCnt
      INTEGER :: IDig, IT, JT
      INTEGER :: Lambda1, Lambda2, Lambda3
      INTEGER :: IOUT
      !
      COMMON/IOFILE/IOUT
      !
      NDigDM = 0
      DO IDig = 1, NDigTotal   ! Loop over the parent diagrams
         !
         ! --- Skip if the interaction vertex is not IType
         !
         IF (M13P(IDig) /= IType) CYCLE
         !
         WRITE(IOUT, '("Parent ", &
     &      I3, ": ", 3I2, X, 3I2, X, 3I2, X, 3I2, I3)') &
     &      IDig, &
     &      M1P(1,IDig), M2P(1,IDig), M3P(1,IDig), &
     &      M1P(2,IDig), M2P(2,IDig), M3P(2,IDig), &
     &      M1P(3,IDig), M2P(3,IDig), M3P(3,IDig), &
     &      M1P(4,IDig), M2P(4,IDig), M3P(4,IDig), &
     &      M13P(IDig)
         !
         ! o Non-constant terms
         !
         ! --- Rank of Lambda vertex (de-excitation operator)
         !
!YA070212         Lambda1 = LOff + NRankParent(IDig)
!YA070212         Lambda2 = LOff + NExtPart(M13P(IDig)) + NExtHole(M13P(IDig))
!YA070212         Lambda3 = LOff + NExtPart(M13P(IDig))
         NDigDM = NDigDM + 1
         !
         ! --- Initialize
         !
         DO IT = 1, 5
            M1DM(IT,NDigDM) = 0
            M2DM(IT,NDigDM) = 0
            M3DM(IT,NDigDM) = 0
         END DO
         !
         ! --- Define lambda vertex
         !
         Lambda1 = NRankParent(IDig)
         Lambda2 = (Lambda1 * 2) - (NExtPart(IType) + NExtHole(IType))
         Lambda3 = Lambda1 - NExtPart(IType)
         !
         Lambda1 = Lambda1 + LOff
         Lambda2 = Lambda2 + LOff
         Lambda3 = Lambda3 + LOff
         !
         M1DM(5,NDigDM) = Lambda1
         M2DM(5,NDigDM) = Lambda2
         M3DM(5,NDigDM) = Lambda3
         !
         ! --- Arrange T vertices in reverse order
         !
         NCnt = 0
         DO IT = 4, 1, -1
            IF (M1P(IT,IDig) == 0) CYCLE
            NCnt = NCnt + 1
            M1DM(NCnt,NDigDM) = M1P(IT,IDig)
            M2DM(NCnt,NDigDM) = M2P(IT,IDig)
            M3DM(NCnt,NDigDM) = M3P(IT,IDig)
         END DO
         !
         ! --- Weight and phase factor
         !
         MwpDM(NDigDM) = MwpP(IDig)
         !
      END DO   ! IDig
      !
      ! o Constant terms
      !
      IF (IType == 3) THEN
         NDigDM = NDigDM + 1
         DO IT = 1, 5
            M1DM(IT,NDigDM) = 0
            M2DM(IT,NDigDM) = 0
            M3DM(IT,NDigDM) = 0
         END DO
         M1DM(1,NDigDM) = 1
         M2DM(1,NDigDM) = 2
         M3DM(1,NDigDM) = 1
         MwpDM(NDigDM) = 1
      ELSE IF (IType == 11) THEN
         NDigDM = NDigDM + 1
         DO IT = 1, 5
            M1DM(IT,NDigDM) = 0
            M2DM(IT,NDigDM) = 0
            M3DM(IT,NDigDM) = 0
         END DO
         M1DM(1,NDigDM) = 2
         M2DM(1,NDigDM) = 4
         M3DM(1,NDigDM) = 2
         MwpDM(NDigDM) = 1
         !
         NDigDM = NDigDM + 1
         DO IT = 1, 5
            M1DM(IT,NDigDM) = 0
            M2DM(IT,NDigDM) = 0
            M3DM(IT,NDigDM) = 0
         END DO
         DO IT = 1, 2
            M1DM(IT,NDigDM) = 1
            M2DM(IT,NDigDM) = 2
            M3DM(IT,NDigDM) = 1
         END DO
         MwpDM(NDigDM) = 2
      END IF
      !
      WRITE(IOUT, '("Response density matrix of type", I3)') IType
      DO IDig = 1, NDigDM
         WRITE(IOUT, '(I3, X, 3I2, "/", 3I2, "/", 3I2, "/", 3I2, "/", 3I5, I3)') &
     &      IDig, &
     &      M1DM(1,IDig), M2DM(1,IDig), M3DM(1,IDig), &
     &      M1DM(2,IDig), M2DM(2,IDig), M3DM(2,IDig), &
     &      M1DM(3,IDig), M2DM(3,IDig), M3DM(3,IDig), &
     &      M1DM(4,IDig), M2DM(4,IDig), M3DM(4,IDig), &
     &      M1DM(5,IDig), M2DM(5,IDig), M3DM(5,IDig), &
     &      MwpDM(IDig)
      END DO
      FLUSH(IOUT)
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_Zeta(M1P, M2P, M3P, M13P, NDigL, M1L, M2L, M3L, M13L, MwpL)
        !
      USE BasicInformation, ONLY : NMax, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      INTEGER :: NDigL
      INTEGER :: M1P(4,*), M2P(4,*), M3P(4,*), M13P(*), M1L(4,*), M2L(4,*), M3L(4,*), M13L(*), MwpL(*)
      !
      CHARACTER(LEN=255) :: Name, ANameL
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=2) :: c2
      LOGICAL :: Constant
      INTEGER, PARAMETER :: IDIGFILE = 99
      INTEGER :: IAdd, NDigJL, NRankRead, NDigRead, IDigRead, IDum, NRank, NRankL, I, IDig, IDigJL, IT
      INTEGER :: IOUT, ICOD
      INTEGER :: MwpP(MaxDig)
      INTEGER :: LArray(3,MaxDig)
      INTEGER :: NDum, NumT, L1
      INTEGER :: MPhase, MWeight
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Start generating zeta equations")')
      !
      ! * Zeta diagrams are generated by simply replacing one of T vertices 
      !  in each Jacobian left diagram by a R vertex
      !
      ! * Diagrams with no T vertices are omitted
      !
      ! * The order of vertices will not be changed (Phase factor does not change)
      !
      ! * The weight factor will be re-evaluated
      !
      ! --- Read Jacobian left diagrams
      !
      DO NRankL = 1, NMax
         !
         OPEN(UNIT=IDIGFILE, FILE="JLeftDiagrams", STATUS="OLD")
         READ(IDIGFILE, '(I3)') NDum
         !
 1000    CONTINUE
         !
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         IF (TRIM(CDum) /= 'NRank=') GO TO 1000
         IF (NRankRead /= NRankL) GO TO 1000
         !
         READ(IDIGFILE, '(I4)') NDigJL
         !     
         DO IDigJL = 1, NDigJL
            READ(IDIGFILE, '(I4, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDum, &
     &         M1P(1,IDigJL), &
     &         M2P(1,IDigJL), &
     &         M3P(1,IDigJL), &
     &         M1P(2,IDigJL), &
     &         M2P(2,IDigJL), &
     &         M3P(2,IDigJL), &
     &         M1P(3,IDigJL), &
     &         M2P(3,IDigJL), &
     &         M3P(3,IDigJL), &
     &         M1P(4,IDigJL), &
     &         M2P(4,IDigJL), &
     &         M3P(4,IDigJL), &
     &         M13P(IDigJL), &
     &         MwpP(IDigJL)
         END DO
         !
         CLOSE(IDIGFILE)
         !     
!         IF ((NMax == 2) .AND. NoT1 .AND. (NRankL == 1)) CYCLE   ! CCD
         !     
         WRITE(*, '("Generating programs for zeta equations for NRankL =", &
     &      I3, " ...")') NRankL
         !     
         ! --- Name of SUBROUTINE
         !
         SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
         END SELECT
         Name = TRIM(Name)//"_zetaconst"
         SELECT CASE (NRankL)
         CASE (1)
            Name = TRIM(Name)//"_ket1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_ket2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_ket3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_ket4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_ket5_autogen.f90"
         END SELECT
         !     
         IF (Complx) THEN
            PreFixFile = "socclr_"
         ELSE
            PreFixFile = "cclr_"
         END IF
         Name = TRIM(PreFixFile)//Name
         !     
         ! --- Open log file and program file
         !     
         CALL Int2Char(NRankL, c2, "0")
         OPEN(UNIT=IOUT, FILE='CC_ZetaConst'//c2//'_log', STATUS='UNKNOWN')   ! log file
         OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
         !     
         ! --- Pareparation for zeta diagrams generation
         ! * The resulting diagrams include the removed T vertices at the leftmost position
         ! * Lambda vertices are saved in LArray(1:3,1:NDigL)
         ! --- For the case where the closing vertex = 1:
         ! * LArray contains (0,0,0)
         ! * The removd diagram will be replaced by a T vertex with no external lines
         !     
!         Constant = .FALSE.   ! Jacobian left operator involves no constant terms
!         CALL GenDiagLambdaPre(NDigTotal, NMax, NRankParent, NRankL, Constant, &
!     &        M1P, M2P, M3P, M13P, NDigL, LArray, M1L, M2L, M3L, M13L)
         CALL GenDiagZeta(NDigJL, NMax, NRankL, M1P, M2P, M3P, M13P, MwpP, &
     &      NDigL, M1L, M2L, M3L, M13L, MwpL)
         !
         ! --- Check size of intermediates
         !
         DO IDig = 1, NDigL
!_Temp            CALL CheckOrderTL(M1L(1,IDig),M2L(1,IDig),M3L(1,IDig),
!_TEmp     *           M13L(IDig),NRankL)
         END DO
         !     
         ! --- Weight and phase factors
         !     
         WRITE(IOUT, '(/, X, "Weight factors:")')
         DO IDig = 1, NDigL
            !
            CALL Define_Weight(M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), MWeight, IOUT)
            IF (MwpL(IDig) > 0) THEN
               MwpL(IDig) = MWeight
            ELSE
               MwpL(IDig) = -MWeight
            END IF
         END DO
         !     
         ! --- Sort diagrams for factorization
         !     
!!!         CALL SortDiagJacobian(NDigL, M1L, M2L, M3L, M13L, MwpL)
         CALL SortDiagGen(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !
         ! --- Eliminate redundancy
         !
         CALL CheckRedundancy(NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! --- Punch out the zeta diagrams
         !     
         WRITE(IOUT, '("Generated Zeta Diagrams and Weight Factors")')
         DO I = 1, NDigL
            WRITE(IOUT, '(X, I3, X, 3I5, X, 3I2, X, 3I2, X, 3I2, X, I2, 3X, I2)') &
     &         I, &
     &         M1L(1,I), M2L(1,I), M3L(1,I), &
     &         M1L(2,I), M2L(2,I), M3L(2,I), &
     &         M1L(3,I), M2L(3,I), M3L(3,I), &
     &         M1L(4,I), M2L(4,I), M3L(4,I), &
     &         M13L(I), MwpL(I)
         END DO
         !
         ! o Save zeta diagrams
         !
         CALL SaveZetaDiagrams(NMax, NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL)
         !     
         ! o Generate programs
         !     
         ! --- Subroutine Name
         !     
         SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
         END SELECT
         !     
         Name = TRIM(Name)//"_ZetaConst"
         !     
         WRITE(c1, '(I1)') NRankL
         Name = TRIM(Name)//"_Ket"//TRIM(c1)//"_AutoGen"
         !     
         IF (Complx) THEN
            PreFixSrc = "SOCCLR_"
         ELSE
            PreFixSrc = "CCLR_"
         END IF
         Name = TRIM(PreFixSrc)//Name
         !     
         ! --- Print header
         !     
         WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
         WRITE(ICOD, *) TRIM(Name)//"(AmpFileT, AmpFileR, AmpFileL)"
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o This is an automatically generated program", /, &
     &      "!")')
         !     
         ! --- Modules
         !     
         WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
         ELSE
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
         END IF
         WRITE(ICOD, '( &
     &      "!", /, &
     &      6X, "IMPLICIT NONE", /, &
     &      "!")')
         !
         ! --- Input variable
         !
         WRITE(ICOD, '(6X, "CHARACTER(*), INTENT(IN) :: AmpFileT, AmpFileR, AmpFileL")')
         WRITE(ICOD, '("!")')
         !     
         ! --- Large arrays
         !     
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "COMPLEX(8), ALLOCATABLE :: V1(:), V1_(:), T(:), R(:), L(:), G(:), G_(:)")')
         ELSE
            WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "REAL(8), ALLOCATABLE :: V1(:), V1_(:), T(:), R(:), L(:), G(:), G_(:)")')
         END IF
         !
         ! --- Local variables
         !
         CALL Declaration(ICOD, 'LAMBDA', Complx)
         !     
         ! --- Declare misc. arrays
         !    
         WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigL
         !     
         ! --- Declare CCLib_NComb function
         !     
         WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
         !     
         ! --- Initialize weight and phase factors
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o Weight and phase factors", /, &
     &      "!")')
         DO IDig = 1, NDigL
            WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &         3I2, "/", 3I2, "/", 3I2, "/", 3I2, I3)')   &
     &         IDig, MwpL(IDig), &
     &         M1L(1,IDig), M2L(1,IDig), M3L(1,IDig), &
     &         M1L(2,IDig), M2L(2,IDig), M3L(2,IDig), &
     &         M1L(3,IDig), M2L(3,IDig), M3L(3,IDig), &
     &         M1L(4,IDig), M2L(4,IDig), M3L(4,IDig), &
     &         M13L(IDig)
         END DO
         !     
         ! --- Factorization of diagrams
         ! --- Start to generate the code
         !     
         WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, &
     &      ")-Zeta(", I2, ") equation", X, 5("-"), /, "!")') &
     &      NMax, NRankL
         !     
         ! --- Make arrays for string Addresses
         !     
         WRITE(ICOD, '("!", /, &
     &      "!     o Prepare arc weight arrays", /, &
     &      "!", /, &
     &      6X, "CALL CCLib_InitArcWgt", /)')
         !     
         ! --- Factorize the diagrams and generate the main frame of program
         !     
         ANameL = 'L'
!         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx, TRIM(ANameL))
         CALL GenFactorizeL1(NRankL, NDigL, M1L, M2L, M3L, M13L, MwpL, Facto, Complx)
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, 5("-"), X, "End of Lambda calculation", X, &
     &      5("-"), /, "!")')
         !     
         WRITE(ICOD, '(6X, "END SUBROUTINE")')
         !     
         CLOSE(IOUT)
         CLOSE(ICOD)
         !
         ! --- End generating Jacobian-left programs
         !
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE GenCC_IPRight(M1, M2, M3, M13, Mwp, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      USE BasicInformation, ONLY : NMax, NRank, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      ! o Generate programs for CC Jacobian right eigenequations
      !
      INTEGER :: NDigJ
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*),  M1J(4,*), M2J(4,*), M3J(4,*), M13J(*), MwpJ(*)
      !
      CHARACTER(LEN=255) :: Name
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=2) :: c2
      INTEGER :: IDig, I
      INTEGER :: MPhase, MWeight, NumT
      INTEGER :: ICOD, IOUT
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Generating programs for CC ionization energies for NRank", I3, " ...")') NRank
      !
      ! --- Name of SUBROUTINE
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
      END SELECT
      Name = TRIM(Name)//"_ipr"
      SELECT CASE (NRank)
         CASE (1)
            Name = TRIM(Name)//"_bra1_autogen.f90"
         CASE (2)
            Name = TRIM(Name)//"_bra2_autogen.f90"
         CASE (3)
            Name = TRIM(Name)//"_bra3_autogen.f90"
         CASE (4)
            Name = TRIM(Name)//"_bra4_autogen.f90"
         CASE (5)
            Name = TRIM(Name)//"_bra5_autogen.f90"
      END SELECT
      !
      IF (Complx) THEN
         PreFixFile = "socclr_"
      ELSE
         PreFixFile = "cclr_"
      END IF
      Name = TRIM(PreFixFile)//Name
      !
      ! --- Open log file and program file
      !
      CALL Int2Char(NRank, c2, "0")
      OPEN(UNIT=IOUT, FILE='CC_IPR'//c2//'_log', STATUS='UNKNOWN')   ! log file
      OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
      !
      ! --- Generate diagrams (phase factor is determined simultaneously)
      !
      CALL GenDiagIPRight(NRank, M1, M2, M3, M13, Mwp, &
     &   NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Check size of intermediates
      !
      DO IDig = 1, NDigJ
         CALL CheckOrderIPR(M1J(1,IDig), M2J(1,IDig), M3J(1,IDig), M13J(IDig), IOUT)
      END DO
      !
      ! --- Sort diagrams according to R operators
      !
!!!      CALL SortDiagJacobian(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      CALL SortDiagGen(NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Weight and phase factors
      !
      DO IDig = 1, NDigJ
!!!         CALL Define_Phase(MaxOp, NRank, M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), M13J(IDig), &
!!!     &      MPhase, IOUT)
         CALL Define_Phase2(MaxOp, NRank, M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), M13J(IDig), &
     &      'IONZ', MPhase, IOUT)
         CALL Define_Weight(M1J(1:4,IDig), M2J(1:4,IDig), M3J(1:4,IDig), MWeight)
         MwPJ(IDig) = MPhase * MWeight
      END DO
      !
      ! --- Save IP right diagrams
      !
      CALL SaveIPRightDiagrams(NMax, NRank, NDigJ, M1J, M2J, M3J, M13J, MwpJ)
      !
      ! --- Punch out the diagrams
      !
      WRITE(IOUT, '("Generated Diagrams and Weight Factors")')
      DO I = 1, NDigJ
         WRITE(IOUT, '(X, I3, X, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, 3X, I2)') &
     &      I, &
     &      M1J(1,I), M2J(1,I), M3J(1,I), &
     &      M1J(2,I), M2J(2,I), M3J(2,I), &
     &      M1J(3,I), M2J(3,I), M3J(3,I), &
     &      M1J(4,I), M2J(4,I), M3J(4,I), &
     &      M13J(I), MwpJ(I)
      END DO
      !
      ! o Generate programs
      !
      ! --- Subroutine header
      !
      SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c1, '(I1)') NMax
            Name = "CCfull"//TRIM(c1)
      END SELECT
      !
      Name = TRIM(Name)//"_IPR"
      !
      WRITE(c1, '(I1)')NRank
      Name = TRIM(Name)//"_Bra"//TRIM(c1)//"_AutoGen"
      !
      IF (Complx) THEN
         PreFixSrc = "SOCCLR_"
      ELSE
         PreFixSrc = "CCLR_"
      END IF
      Name = TRIM(PreFixSrc)//Name
      !
      ! --- Print header
      !
      WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
      WRITE(ICOD, *) TRIM(Name)
      WRITE(ICOD, '("!", /, "!", 5X, "o This is an automatically generated program", /, "!")')
      !
      ! --- Modules
      !
      WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
      ELSE
         WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
      END IF
      WRITE(ICOD, '("!", /, 6X, "IMPLICIT NONE",/, "!")')
      !
      ! --- Large arrays
      !
      IF (Complx) THEN
         WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "COMPLEX(8), ALLOCATABLE :: V1(:), T(:), R(:), T_(:), R_(:), G(:), G_(:)")')
      ELSE
         WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &      "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, &
     &      6X, "REAL(8), ALLOCATABLE :: V1(:), T(:), R(:), T_(:), R_(:), G(:), G_(:)")')
      END IF
      !
      ! --- Local variables
      !
      CALL Declaration(ICOD, 'MAIN', Complx)
      !
      ! --- Declare misc. arrays
      !
      WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigJ
      !
      ! --- Declare CCLib_NComb function
      !
      WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
      !
      ! --- Initialize weight and phase factors
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, "o Weight and phase factors", /, &
     &   "!")')
      DO IDig = 1, NDigJ
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &      3I2, "/", 3I2, "/", 3I2, "/", 3I2, I3)') &
     &      IDig, MwpJ(IDig), &
     &      M1J(1,IDig), M2J(1,IDig), M3J(1,IDig), &
     &      M1J(2,IDig), M2J(2,IDig), M3J(2,IDig), &
     &      M1J(3,IDig), M2J(3,IDig), M3J(3,IDig), &
     &      M1J(4,IDig), M2J(4,IDig), M3J(4,IDig), &
     &      M13J(IDig)
      END DO
      !
      ! --- Factorization of diagrams
      ! --- Start to generate the code
      !
      WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, ")-T(", I2, &
     &   ") equation", X, 5("-"), /, "!")') NMax, NRank
      !
      ! --- Make arrays for string Addresses
      !
      WRITE(ICOD, '("!", /, &
     &   "! o Prepare arc weight arrays", /, &
     &   "!", /, &
     &   6X, "CALL CCLib_InitArcWgt", /)')
      !
      ! --- Factorize the diagrams and generate the main frame of program
      !
      CALL GenFactorizeIPR(NRank, NDigJ, M1J, M2J, M3J, M13J, MwpJ, Facto, Complx)
      !
      WRITE(ICOD, '("!", /, &
     &   "!", 5X, 5("-"), X, "End of IP Jacobian right calculation", X, &
     &   5("-"), /, "!")')
      !
      WRITE(ICOD, '(6X, "END SUBROUTINE")')
      !
      CLOSE(IOUT)
      CLOSE(ICOD)
      !
      WRITE(*, '("Generation of IP right programss done.")')
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!!!      SUBROUTINE GenCC_DM_Common(M1P, M2P, M3P, M13P, MwpP, NDigDM, M1DM, M2DM, M3DM, MwpDM)
      SUBROUTINE GenCC_DM_Common(M1P, M2P, M3P, M13P, MwpP, NDigDM)
        !
      USE BasicInformation, ONLY : NMax, Complx, PreFixFile, PreFixSrc, MaxDig, &
     &   NoT1, MaxOrder, OrderCut, Facto, MaxOp
      !
      IMPLICIT NONE
      !
      INTEGER, INTENT(INOUT) :: M1P(4,*), M2P(4,*), M3P(4,*), M13P(*), MwpP(*)
      INTEGER, INTENT(OUT):: NDigDM
!      INTEGER, INTENT(OUT) :: M1DM(5,*), M2DM(5,*), M3DM(5,*), MwpDM(*)
      !
      CHARACTER(LEN=255) :: Name, ANameL
      CHARACTER(LEN=30) :: c1
      CHARACTER(LEN=6) :: CDum
      CHARACTER(LEN=2) :: c2
      LOGICAL :: Constant
      INTEGER, PARAMETER :: IDIGFILE = 99
      INTEGER :: IType
      INTEGER :: IAdd, NDigTotal, NRankRead, NDigRead, IDigRead, IDum, NRank, NRankL, I, IDig, IT
      INTEGER :: IOUT, ICOD
      INTEGER :: NRankParent(MaxDig)
      INTEGER :: NDum, NumT, L1
      INTEGER :: MPhase, MWeight
      INTEGER :: M1DM(5,MaxDig), M2DM(5,MaxDig), M3DM(5,MaxDig), MwpDM(MaxDig)
      !
      COMMON/IOFILE/IOUT, ICOD
      !
      WRITE(*, '("Start generating density matrices (Common terms)")')
      !
      ! --- Read parent diagrams
      !
      OPEN(UNIT=IDIGFILE, FILE="ParentDiagrams", STATUS="OLD")
      READ(IDIGFILE, '(I3)') NDum
      !
      IAdd = 0
      NDigTotal = 0
      DO NRank = 1, NMax
         READ(IDIGFILE, '(A6, I4)') CDum, NRankRead
         READ(IDIGFILE, '(I4)') NDigRead
         !     
         DO IDigRead = 1, NDigRead
            READ(IDIGFILE, '(I4, 3I2, X, 3I2, X, 3I2, X, 3I2, X, I2, X, I2)') &
     &         IDum, &
     &         M1P(1, IDigRead+IAdd), &
     &         M2P(1, IDigRead+IAdd), &
     &         M3P(1, IDigRead+IAdd), &
     &         M1P(2, IDigRead+IAdd), &
     &         M2P(2, IDigRead+IAdd), &
     &         M3P(2, IDigRead+IAdd), &
     &         M1P(3, IDigRead+IAdd), &
     &         M2P(3, IDigRead+IAdd), &
     &         M3P(3, IDigRead+IAdd), &
     &         M1P(4, IDigRead+IAdd), &
     &         M2P(4, IDigRead+IAdd), &
     &         M3P(4, IDigRead+IAdd), &
     &         M13P(IDigRead+IAdd), MwpP(IDigRead+IAdd)
            NRankParent(IDigRead+IAdd) = NRankRead
            NDigTotal = NDigTotal + 1
         END DO
         IAdd = IAdd + NDigRead
      END DO
      !
      CLOSE(IDIGFILE)
      !     
      WRITE(*, '("Total # of parent diagrams = ", I6)') NDigTotal
      !     
      ! --- Loop over types of interaction vertex
      !     
      DO IType = 1, 13
         !
         CALL Int2Char(IType, C2, '0')
         !     
!         IF ((NMax == 2) .AND. NoT1 .AND. (NRankL == 1)) CYCLE   ! CCD
         !     
         WRITE(*, '("Generating programs for CC response density matrix of type", &
     &      I3, " ...")') IType
         !     
         ! --- Name of SUBROUTINE
         !
         SELECT CASE (NMax)
         CASE (1)
            Name = "ccs"
         CASE (2)
            Name = "ccsd"
            IF (NoT1) Name = "ccd"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "cc2"
         CASE (3)
            Name = "ccsdt"
         CASE (4)
            Name = "ccsdtq"
         CASE (5)
            Name = "ccsdtq5"
         END SELECT
         Name = TRIM(Name)//"_respdm"//C2//"common_autogen.f90"
         !     
         IF (Complx) THEN
            PreFixFile = "socclr_"
         ELSE
            PreFixFile = "cclr_"
         END IF
         Name = TRIM(PreFixFile)//Name
         !     
         ! --- Open log file and program file
         !     
         OPEN(UNIT=IOUT, FILE='CC_RespDM'//C2//'Common_log', STATUS='UNKNOWN')   ! log file
         OPEN(UNIT=ICOD, FILE=TRIM(Name), STATUS='UNKNOWN')   ! main source file
         !     
         ! --- Generation of response DM digrams
         !
         ! From a parent diagram T1-T2-T3-T4-V:
         ! Resp. DM diagram L-T1-T2-T3-T4 is represented in the form T4-T3-T2-T1-L
         ! Phase factor is not changed
         ! Weight factor is not changed
         !
         CALL GenDiagDM_Common(NDigTotal, NMax, NRankParent, IType, &
     &      M1P, M2P, M3P, M13P, MwpP, NDigDM, M1DM, M2DM, M3DM, MwpDM)
         !
         ! --- Check size of intermediates
         !
         DO IDig = 1, NDigDM
!_Temp            CALL CheckOrderTL(M1L(1,IDig),M2L(1,IDig),M3L(1,IDig),
!_TEmp     *         M13L(IDig),NRankL)
         END DO
         !     
         ! --- Weight and phase factors
         !     
!         WRITE(IOUT, '(/, X, "Weight and phase factors:")')
!         DO IDig = 1, NDigL
!            CALL Define_Phase(MaxOp, L1, M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), M13L(IDig), &
!     &         MPhase, IOUT)
!            CALL Define_Weight(M1L(1:4,IDig), M2L(1:4,IDig), M3L(1:4,IDig), MWeight, IOUT)
!            MwpL(IDig) = MPhase * MWeight
!         END DO
         !     
         ! --- Sort diagrams for factorization
         !     
         CALL SortDiagGenDM(NDigDM, M1DM, M2DM, M3DM, MwpDM)
         !
         ! --- Eliminate redundancy
         !
!         CALL CheckRedundancy(NDigDM, M1DM, M2DM, M3DM, MwpL)
         !     
         ! --- Punch out the response density matrix Diagrams
         !     
         WRITE(IOUT, '("Generated Density Matrix Diagrams of Type", I3)') IType
         DO I = 1, NDigDM
            WRITE(IOUT, '(X, I3, X, 3I2, X, 3I2, X, 3I2, X, 3I2, X, 3I5, X, I2)') &
     &         I, &
     &         M1DM(1,I), M2DM(1,I), M3DM(1,I), &
     &         M1DM(2,I), M2DM(2,I), M3DM(2,I), &
     &         M1DM(3,I), M2DM(3,I), M3DM(3,I), &
     &         M1DM(4,I), M2DM(4,I), M3DM(4,I), &
     &         M1DM(5,I), M2DM(5,I), M3DM(5,I), &
     &         MwpDM(I)
         END DO
         !
         ! o Save DM diagrams
         !
         CALL SaveRespDMCommonDiagrams(IType, NDigDM, M1DM, M2DM, M3DM, MwpDM)
         !     
         ! o Generate programs
         !     
         ! --- Subroutine Name
         !     
         SELECT CASE (NMax)
         CASE (1)
            Name = "CCS"
         CASE (2)
            Name = "CCSD"
            IF (NoT1) Name = "CCD"
            IF (OrderCut .AND. (MaxOrder == 1)) Name = "CC2"
         CASE (3)
            Name = "CCSDT"
         CASE (4)
            Name = "CCSDTQ"
         CASE (5)
            Name = "CCSDTQ5"
         CASE DEFAULT
            WRITE(c2, '(I1)') NMax
            Name = "CCfull"//TRIM(c2)
         END SELECT
         !     
         Name = TRIM(Name)//"_RespDM"//TRIM(C2)//"Common_AutoGen"
         !     
         IF (Complx) THEN
            PreFixSrc = "SOCCLR_"
         ELSE
            PreFixSrc = "CCLR_"
         END IF
         Name = TRIM(PreFixSrc)//Name
         !     
         ! --- Print header
         !     
         WRITE(ICOD, '(6X, "SUBROUTINE", X, $)')
         WRITE(ICOD, *) TRIM(Name)//"(AmpFileT, AmpFileR, AmpFileL)"   ! Name of amplitude files
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o This is an automatically generated program", /, &
     &      "!")')
         !     
         ! --- Modules
         !     
         WRITE(ICOD, '(6X, "USE CC_Module, ONLY : NOc, NVr")')
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, OneC")')
         ELSE
            WRITE(ICOD, '(6X, "USE CC_Constant_Module, ONLY : Half, One")')
         END IF
         WRITE(ICOD, '( &
     &      "!", /, &
     &      6X, "IMPLICIT NONE", /, &
     &      "!")')
         !
         ! --- Input variable
         !
         WRITE(ICOD, '(6X, "CHARACTER(*), INTENT(IN) :: AmpFileT, AmpFileR, AmpFileL")')
         WRITE(ICOD, '("!")')
         !     
         ! --- Large arrays
         !     
         IF (Complx) THEN
            WRITE(ICOD, '(6X, "COMPLEX(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "COMPLEX(8), ALLOCATABLE :: V1(:), V1_(:), T(:), L(:), G(:), G_(:)")')
         ELSE
            WRITE(ICOD, '(6X, "REAL(8), ALLOCATABLE ::", X, &
     &         "V2(:), V3(:), V4(:), V3_(:), V4_(:)", /, 6X, &
     &         "REAL(8), ALLOCATABLE :: V1(:), V1_(:), T(:), L(:), G(:), G_(:)")')
         END IF
         !
         ! --- Local variables
         !
         CALL Declaration(ICOD, 'LAMBDA', Complx)
         !     
         ! --- Declare misc. arrays
         !    
         WRITE(ICOD, '(6X, "INTEGER :: Mwp(", I4, ")")') NDigDM
         !     
         ! --- Declare CCLib_NComb function
         !     
         WRITE(ICOD, '(6X, "INTEGER :: CCLib_NComb")')
         !     
         ! --- Initialize weight and phase factors
         !     
         WRITE(ICOD, '("!", /, &
     &      "!", 5X, "o Weight and phase factors", /, &
     &      "!")')
      DO IDig = 1, NDigDM
         WRITE(ICOD, '(6X, "Mwp(", I4, ")=", I5, 3X, "!", X, &
     &      3I2, "/", 3I2, "/", 3I2, "/", 3I2, "/", 3I5)') &
     &      IDig, MwpDM(IDig), &
     &      M1DM(1,IDig), M2DM(1,IDig), M3DM(1,IDig), &
     &      M1DM(2,IDig), M2DM(2,IDig), M3DM(2,IDig), &
     &      M1DM(3,IDig), M2DM(3,IDig), M3DM(3,IDig), &
     &      M1DM(4,IDig), M2DM(4,IDig), M3DM(4,IDig), &
     &      M1DM(5,IDig), M2DM(5,IDig), M3DM(5,IDig)
      END DO
      !     
      ! --- Factorization of diagrams
      ! --- Start to generate the code
      !     
         WRITE(ICOD, '("!", /, "!", 5X, 5("-"), X, "CC(", I2, &
     &      ")-Response DM of Type", I3, X, 5("-"), /, "!")') &
     &      NMax, IType
         !     
         ! --- Make arrays for string Addresses
         !     
         WRITE(ICOD, '("!", /, &
     &      "!     o Prepare arc weight arrays", /, &
     &      "!", /, &
     &      6X, "CALL CCLib_InitArcWgt", /)')
         !     
         ! --- Factorize the diagrams and generate the main frame of program
         !
!         CALL GenFactorizeDMCommon(IType, NDigDM, M1DM, M2DM, M3DM, MwpDM, Facto, Complx)
         !     
         WRITE(ICOD, '("        	!", /, &
     &      "	!", 5X, 5("-"), X, "End of Lambda calculation", X, &
     &      5("-"), /, "	!")')
         !     
         WRITE(ICOD, '(6X, "END SUBROUTINE")')
         !     
         CLOSE(IOUT)
         CLOSE(ICOD)
         !
         ! --- End generating Lambda programs
         !
      END DO   ! IType
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SortDiagGen(NDig, M1, M2, M3, M13, Mwp)
        !**************************************************************************
        ! Sort diagrams (general)
        !**************************************************************************
      USE BasicInformation, ONLY : LOff
      !
      IMPLICIT NONE
      !
      INTEGER :: NDig
      INTEGER :: M1(4,*), M2(4,*), M3(4,*), M13(*), Mwp(*)
      !
      LOGICAL :: Next1, Next2, Next3, Done(NDig), DoneAll
      INTEGER :: IDig, IT, NCnt, IOUT
      INTEGER :: NT1Current, NT2Current, NT3Current, NumT1, NumT1Tot
      INTEGER :: NumAdd1, NumAdd2, NumAdd3
      INTEGER :: NTOrig(4,NDig), M1Orig(4,NDig), M2Orig(4,NDig), M3Orig(4,NDig), M13Orig(NDig), MwpOrig(NDig)
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Copy original diagrams to temporal arrays
      !
      DO IDig = 1, NDig
         DO IT = 1, 4
            M1Orig(IT,IDig) = M1(IT,IDig)
            M2Orig(IT,IDig) = M2(IT,IDig)
            M3Orig(IT,IDig) = M3(IT,IDig)
         END DO
         M13Orig(IDig) = M13(IDig)
         MwpOrig(IDig) = Mwp(IDig)
      END DO
      !
      ! --- Make single integer representing each vertices
      !
      DO IDig = 1, NDig
         DO IT = 1, 4
            NTOrig(IT,IDig) = M1Orig(IT,IDig) * (LOff * 10 * LOff * 10) + &
     &                        M2Orig(IT,IDig) * (LOff * 10) + &
     &                        M3Orig(IT,IDig)
         END DO
      END DO
      !
      ! --- Factorize diagrams
      !
      DO IDig = 1, NDig
         Done(IDig) = .FALSE.
      END DO
      !
      Next1 = .TRUE.
      Next2 = .TRUE.
      Next3 = .TRUE.
      NumAdd1 = 0
      NumAdd2 = 0
      NumAdd3 = 0
      NCnt = 0
  100 CONTINUE
      DO IDig = 1, NDig
         IF (Done(IDig)) CYCLE
         IF (Next1) THEN
            NT1Current = NTOrig(1,IDig)
            Next1 = .FALSE.
         END IF
         IF (Next2) THEN
            IF (NTOrig(1,IDig) == NT1Current) THEN
               NT2Current = NTOrig(2,IDig)
               Next2 = .FALSE.
            END IF
         END IF
         IF (Next3) THEN
            IF ((NTOrig(1,IDig) == NT1Current) .AND. (NTOrig(2,IDig) == NT2Current)) THEN
               NT3Current = NTOrig(3,IDig)
               Next3 = .FALSE.
            END IF
         END IF
         IF ((NTOrig(1,IDig) == NT1Current) .AND. &
     &       (NTOrig(2,IDig) == NT2Current) .AND. &
     &       (NTOrig(3,IDig) == NT3Current)) THEN
            NCnt = NCnt + 1
            NumAdd1 = NumAdd1 + 1
            NumAdd2 = NumAdd2 + 1
            NumAdd3 = NumAdd3 + 1
            DO IT = 1, 4
               M1(IT,NCnt) = M1Orig(IT,IDig)
               M2(IT,NCnt) = M2Orig(IT,IDig)
               M3(IT,NCnt) = M3Orig(IT,IDig)
            END DO
            M13(NCnt) = M13Orig(IDig)
            Mwp(NCnt) = MwpOrig(IDig)
            Done(IDig) = .TRUE.
         END IF
      END DO
      !
      DoneAll = .TRUE.
      DO IDig = 1, NDig
         DoneAll = DoneAll .AND. Done(IDig)
      END DO
      IF (.NOT. DoneAll) THEN
         Next3 = .TRUE.
         Next2 = (NumAdd3 == 0)
         Next1 = (NumAdd2 == 0)
         NumAdd3 = 0
         IF (Next2) NumAdd2 = 0
         IF (Next1) NumAdd1 = 0
         GO TO 100
      END IF
      !
      WRITE(IOUT, '("Sorted Diagrams:")')
      DO IDig = 1, NDig
         WRITE(IOUT, '(I3, X, 3I5, "/", 3I2, "/", 3I2, "/", 3I2, I3, I3)') &
     &      IDig, &
     &      M1(1,IDig), M2(1,IDig), M3(1,IDig), &
     &      M1(2,IDig), M2(2,IDig), M3(2,IDig), &
     &      M1(3,IDig), M2(3,IDig), M3(3,IDig), &
     &      M1(4,IDig), M2(4,IDig), M3(4,IDig), &
     &      M13(IDig), Mwp(IDig)
      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE SortDiagGenDM(NDig, M1, M2, M3, Mwp)
        !**************************************************************************
        ! Sort diagrams (general)
        !**************************************************************************
      USE BasicInformation, ONLY : LOff
      !
      IMPLICIT NONE
      !
      INTEGER :: NDig
      INTEGER :: M1(5,*), M2(5,*), M3(5,*), Mwp(*)
      !
      LOGICAL :: Next1, Next2, Next3, Done(NDig), DoneAll
      INTEGER :: IDig, IT, NCnt, IOUT
      INTEGER :: NT1Current, NT2Current, NT3Current, NumT1, NumT1Tot
      INTEGER :: NumAdd1, NumAdd2, NumAdd3
      INTEGER :: NTOrig(5,NDig), M1Orig(5,NDig), M2Orig(5,NDig), M3Orig(5,NDig), MwpOrig(NDig)
      !
      COMMON/IOFILE/IOUT
      !
      ! --- Copy original diagrams to temporal arrays
      !
      DO IDig = 1, NDig
         DO IT = 1, 5
            M1Orig(IT,IDig) = M1(IT,IDig)
            M2Orig(IT,IDig) = M2(IT,IDig)
            M3Orig(IT,IDig) = M3(IT,IDig)
         END DO
         MwpOrig(IDig) = Mwp(IDig)
      END DO
      !
      ! --- Make single integer representing each vertices
      !
      DO IDig = 1, NDig
         DO IT = 1, 4
            NTOrig(IT,IDig) = M1Orig(IT,IDig) * (LOff * 10 * LOff * 10) + &
     &                        M2Orig(IT,IDig) * (LOff * 10) + &
     &                        M3Orig(IT,IDig)
         END DO
      END DO
      !
      ! --- Factorize diagrams
      !
      DO IDig = 1, NDig
         Done(IDig) = .FALSE.
      END DO
      !
      Next1 = .TRUE.
      Next2 = .TRUE.
      Next3 = .TRUE.
      NumAdd1 = 0
      NumAdd2 = 0
      NumAdd3 = 0
      NCnt = 0
  100 CONTINUE
      DO IDig = 1, NDig
         IF (Done(IDig)) CYCLE
         IF (Next1) THEN
            NT1Current = NTOrig(1,IDig)
            Next1 = .FALSE.
         END IF
         IF (Next2) THEN
            IF (NTOrig(1,IDig) == NT1Current) THEN
               NT2Current = NTOrig(2,IDig)
               Next2 = .FALSE.
            END IF
         END IF
         IF (Next3) THEN
            IF ((NTOrig(1,IDig) == NT1Current) .AND. (NTOrig(2,IDig) == NT2Current)) THEN
               NT3Current = NTOrig(3,IDig)
               Next3 = .FALSE.
            END IF
         END IF
         IF ((NTOrig(1,IDig) == NT1Current) .AND. &
     &       (NTOrig(2,IDig) == NT2Current) .AND. &
     &       (NTOrig(3,IDig) == NT3Current)) THEN
            NCnt = NCnt + 1
            NumAdd1 = NumAdd1 + 1
            NumAdd2 = NumAdd2 + 1
            NumAdd3 = NumAdd3 + 1
            DO IT = 1, 5
               M1(IT,NCnt) = M1Orig(IT,IDig)
               M2(IT,NCnt) = M2Orig(IT,IDig)
               M3(IT,NCnt) = M3Orig(IT,IDig)
            END DO
            Mwp(NCnt) = MwpOrig(IDig)
            Done(IDig) = .TRUE.
         END IF
      END DO
      !
      DoneAll = .TRUE.
      DO IDig = 1, NDig
         DoneAll = DoneAll .AND. Done(IDig)
      END DO
      IF (.NOT. DoneAll) THEN
         Next3 = .TRUE.
         Next2 = (NumAdd3 == 0)
         Next1 = (NumAdd2 == 0)
         NumAdd3 = 0
         IF (Next2) NumAdd2 = 0
         IF (Next1) NumAdd1 = 0
         GO TO 100
      END IF
      !
!      WRITE(IOUT, '("Sorted Diagrams:")')
!      DO IDig = 1, NDig
!         WRITE(IOUT, '(I3, X, 3I2, "/", 3I2, "/", 3I2, "/", 3I2, "/", 3I5, I3)') &
!     &      IDig, &
!     &      M1(1,IDig), M2(1,IDig), M3(1,IDig), &
!     &      M1(2,IDig), M2(2,IDig), M3(2,IDig), &
!     &      M1(3,IDig), M2(3,IDig), M3(3,IDig), &
!     &      M1(4,IDig), M2(4,IDig), M3(4,IDig), &
!     &      M1(5,IDig), M2(5,IDig), M3(5,IDig), &
!     &      Mwp(IDig)
!      END DO
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE Define_Phase2(MaxOp, NRank, M1, M2, M3, M13, RType, MPhase, IOUT)
        !
        ! o Evaluate phase factor
        !
        !   RType : 'EXCI' or 'IONZ'
        !
      IMPLICIT NONE
      !
      CHARACTER(LEN=4), INTENT(IN) :: RType
      INTEGER, INTENT(IN) :: MaxOp, NRank, M1(4), M2(4), M3(4), M13, IOUT
      INTEGER, INTENT(OUT) :: MPhase   ! +1 or -1
      !
      CHARACTER(LEN=2) :: Type(MaxOp), TypeT, TypeV, TypeI, TypeJ
      INTEGER :: M1Abs(4), M2Abs(4), M3Abs(4), N
      INTEGER :: L1, L2, L3
      INTEGER :: NumOp, NumOpL, NumOpT(4), NumOpV, LocOpT(4), LocOpV
      INTEGER :: ICnt, IOp, JOp, KOp, IOpT, IOpV, IT
      INTEGER :: IAdd, IOff, NAddOpT, NAddOpV, NAddIOp, NAddJOp
      INTEGER :: NumLoop, NumHole, NumHoleL, NumHoleT, NumHoleV
      INTEGER :: NumT
      INTEGER :: NParity
      INTEGER :: NOcc(MaxOp)
!!!
      WRITE(IOUT, '("Defining phase factor of the diagram ", &
     &   3I2, "/", 3I2, "/", 3I2, "/", 3I2, X, I2)') &
     &   M1(1), M2(1), M3(1), &
     &   M1(2), M2(2), M3(2), &
     &   M1(3), M2(3), M3(3), &
     &   M1(4), M2(4), M3(4), &
     &   M13
!!!
      !
      NumT = 4   ! # of T (or R) vertices (including zero excitations)
      !
      DO N = 1, NumT
         M1Abs(N) = ABS(M1(N))
         M2Abs(N) = ABS(M2(N))
         M3Abs(N) = ABS(M3(N))
      END DO
      !
      ! o Make full string of second-quantization operators
      !
      NumOp = 0
      IAdd = 0
      !
      ! --- String of closing vertex
      !
      IF (TRIM(RType) == 'EXCT') THEN
         NumOpL = NRank + NRank
         Type(1) = 'eh'
         Type(2) = 'ep'
         IOff = 2
      ELSE IF (TRIM(RType) == 'IONZ') THEN
         NumOpL = NRank + (NRank - 1)
         Type(1) = 'eh'   ! Unpaired hole
         IOff = 1
      END IF
      !
      DO IOp = (IOff + 1), NumOpL, 2   ! holes
         Type(IOp) = 'eh'
      END DO
      DO IOp = (IOff + 2), NumOpL, 2   ! particles
         Type(IOp) = 'ep'
      END DO
      !
      NumOp = NumOp + NumOpL
      IAdd = IAdd + NumOp
      !
      ! --- String of interaction vertex
      ! --- Assume {p*rq*s} (with the corresponding integral <pq||rs>) ordering
      !
      SELECT CASE (M13)
         CASE (1)
            NumOpV = 2
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'ip'
         CASE (2)
            NumOpV = 2
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
         CASE (3)
            NumOpV = 2
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
         CASE (4)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (5)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (6)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (7)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (8)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (9)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'ip'
         CASE (10)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'eh'
         CASE (11)
            NumOpV = 4
            Type(IAdd+1) = 'ih'
            Type(IAdd+2) = 'ip'
            Type(IAdd+3) = 'ih'
            Type(IAdd+4) = 'ip'
         CASE (12)
            NumOpV = 2
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
         CASE (13)
            NumOpV = 4
            Type(IAdd+1) = 'ep'
            Type(IAdd+2) = 'eh'
            Type(IAdd+3) = 'ep'
            Type(IAdd+4) = 'eh'
      END SELECT
      !
      LocOpV = IAdd
      NumOp = NumOp + NumOpV
      IAdd = IAdd + NumOpV
      !
      ! --- T or R vertices
      !
      DO IT = NumT, 1, -1   ! Order of contraction
         LocOpT(IT) = IAdd   ! String of IT-th vertex starts from (LocOpT(IT) + 1)
         !
         IF (M1(IT) == 0) THEN
            NumOpT(IT) = 0
            CYCLE
         ELSE IF ((M1(IT) > 0) .OR. (TRIM(RType) == 'EXCT')) THEN   ! Excitation
            !
            NumOpT(IT) = M1Abs(IT) + M1Abs(IT)
            ICnt = 0
            DO IOp = 1, (NumOpT(IT) - 1), 2   ! particles
               ICnt = ICnt + 1
               IF (ICnt <= M3Abs(IT)) THEN
                  Type(IAdd+IOp) = 'ip'
               ELSE
                  Type(IAdd+IOp) = 'ep'
               END IF
            END DO
            ICnt = 0
            DO IOp = 2, NumOpT(IT), 2   ! holes
               ICnt = ICnt + 1
               IF (ICnt <= (M2Abs(IT) - M3Abs(IT))) THEN
                  Type(IAdd+IOp) = 'ih'
               ELSE
                  Type(IAdd+IOp) = 'eh'
               END IF
            END DO
            !
         ELSE IF ((M1(IT) < 0) .AND. (TRIM(RType) == 'IONZ')) THEN   ! Ionization
            !
            NumOpT(IT) = M1Abs(IT) + (M1Abs(IT) - 1)
            ICnt = 0
            DO IOp = 1, (NumOpT(IT) - 2), 2   ! particles
               ICnt = ICnt + 1
               IF (ICnt <= M3Abs(IT)) THEN
                  Type(IAdd+IOp) = 'ip'
               ELSE
                  Type(IAdd+IOp) = 'ep'
               END IF
            END DO
            !
            ! ----- For paired holes
            !
            ICnt = 0
            DO IOp = 2, (NumOpT(IT) - 1), 2   ! holes
               ICnt = ICnt + 1
               IF (ICnt <= (M2Abs(IT) - M3Abs(IT))) THEN
                  Type(IAdd+IOp) = 'ih'
               ELSE
                  Type(IAdd+IOp) = 'eh'
               END IF
            END DO
            !
            ! ----- For unpaired hole
            !
            ICnt = ICnt + 1
            IF (ICnt <= (M2Abs(IT) - M3Abs(IT))) THEN
               Type(IAdd+NumOpT(IT)) = 'ih'
            ELSE
               Type(IAdd+NumOpT(IT)) = 'eh'
            END IF
            !
         END IF
         !
         IAdd = IAdd + NumOpT(IT)
         NumOp = NumOp + NumOpT(IT)
         !      
      END DO
!!!
      WRITE(IOUT, '("Operator string")')
      DO IOp = 1, NumOp
         WRITE(IOUT, '(X, I3, X, A2)') IOp, Type(IOp)
      END DO
!!!
      !
      ! o Internal contraction
      !
      ! --- Initialize the occupation array
      !
      DO IOp = 1, NumOp
         NOcc(IOp) = 1
      END DO
      !
      ! --- Take internal contractions
      !
      NParity = 0
      DO IT = 1, NumT
         !
         DO IOpT = 1, NumOpT(IT)
            NAddOpT = IOpT + LocOpT(IT)
            IF (NOcc(NAddOpT) == 0) CYCLE
            TypeT = Type(NAddOpT)
            IF (TypeT(1:1) /= 'i') CYCLE   ! Internal only
            DO IOpV = 1, NumOpV
               NAddOpV = IOpV + LocOpV
               IF (NOcc(NAddOpV) == 0) CYCLE
               TypeV = Type(NAddOpV)
               IF (TypeV(1:1) /= 'i') CYCLE   ! Internal only
               !
               IF (TRIM(TypeV) == TRIM(TypeT)) THEN   ! Internal contraction
                  !
                  IF ((NAddOpT - NAddOpV) == 1) THEN
                     CONTINUE
                  ELSE IF ((NAddOpT - NAddOpV) >= 2) THEN   ! Count the # of permutations
                     DO JOp = (NAddOpV + 1), (NAddOpT - 1)
                        NParity = NParity + NOcc(JOp)
                     END DO
                  END IF
                  !
!!!
                  WRITE(IOUT, '("Int. Contraction (", 2I3, ")")') NAddOpV, NAddOpT
!!!
                  NOcc(NAddOpV) = 0
                  NOcc(NAddOpT) = 0
                  EXIT
                  !
               END IF
               !
            END DO   !IOpV
         END DO   ! IOpT
         !
      END DO   ! IT
!!!
      WRITE(IOUT, '("Parity for int. contractions:", I3 )') NParity
!!!
      !
      ! o External contraction
      !
      DO IOp = 1, (NumOp - NumOpL)   ! External indices of V, T, R
         NAddIOp = LocOpV + IOp
         IF (NOcc(NAddIOp) == 0) CYCLE
         TypeI = Type(NAddIOp)
         IF (TypeI(1:1) /= 'e') CYCLE   ! External only
         !
         DO JOp = NumOpL, 1, -1   ! External indices of closing vertex
            NAddJOp = JOp
            IF (NOcc(NAddJOp) == 0) CYCLE
            TypeJ = Type(NAddJOp)
            IF (TypeJ(1:1) /= 'e') CYCLE   ! External only
            !
            IF (TRIM(TypeI) == TRIM(TypeJ)) THEN   ! External contraction
               IF ((NAddIOp - NAddJOp) == 1) THEN
                  CONTINUE
               ELSE IF ((NAddIOp - NAddJOp) >= 2) THEN
                  DO KOp = (NAddJOp + 1), (NAddIOp - 1)
                     NParity = NParity + NOcc(KOp)
                  END DO
               END IF
               !
!!!
               WRITE(IOUT, '("Ext. Contraction (", 2I3, ")")') NAddJOp, NAddIOp
!!!
               NOcc(NAddIOp) = 0
               NOcc(NAddJOp) = 0
               EXIT
            END IF
            !
         END DO
      END DO
      !
      ! o Evaluate phase factor
      !
!!!
      WRITE(IOUT, '("Parity for full contraction:", I3 )') NParity
!!!
      MPhase = (-1) ** NParity
      !
      END SUBROUTINE
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      SUBROUTINE MakeSrcContrctBLAS3(nAo, nIo, nAt, nIt, nB, nJ, nC, nK, RtnName)
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        !     Generate a SUBROUTINE which contracts an intermedeate V(CKBJAI)
        !     with a T vertex T(BJA'I') 
        !     to generate a new intermediate W(CKA"I")
        !
        !     This version uses BLAS3 (DGEMM or ZGEMM)
        !
        !     In:  nAo and nIo ... # of external lines of the given intermediate
        !          nT, lT, mT ... Specify the T-vertex
        !          nC and nK ... # of free internal lines
        !
        !CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      USE BasicInformation, ONLY : Complx, PreFixSrc
      !
      IMPLICIT NONE
      !
      CHARACTER(LEN=255) RtnName
      INTEGER :: nAo, nIo, nAt, nIt, nB, nJ, nC, nK
      !
      CHARACTER(LEN=2) a21, a22, a23, a24, A25, A26
      CHARACTER(LEN=2) AName
      CHARACTER(LEN=3) AName_
      CHARACTER(LEN=7) LpName1(100), LpName2(100)
      CHARACTER(LEN=255) BasName, RtnName_lw
      LOGICAL :: Finish, Vector
      INTEGER :: IOUT, ICOD1, ICOD2
      INTEGER :: iIt, iAt, iB, iJ, iAo, iIo
      !
      COMMON/IOFILE/IOUT, ICOD1, ICOD2
      !
      ! --- Name of the SUBROUTINE
      !
      BasName = TRIM(PreFixSrc)//"ContractGEMM"
      !
      CALL Int2Char(nAo, a21, "0")
      CALL Int2Char(nIo, a22, "0")
      CALL Int2Char(nAt, a23, "0")
      CALL Int2Char(nIt, a24, "0")
      RtnName = TRIM(BasName)//"_Ao"//a21//"Io"//a22//"At"//a23//"It"//a24
      !
      Vector = ((nC /= 0) .OR. (nK /= 0))
      IF (.NOT. Vector) THEN
         RtnName = TRIM(RtnName)//"S"   ! C and K are zero (scalar)
      ELSE
         RtnName = TRIM(RtnName)//"V"   ! C and/or K is non-zero (vector)
      END IF
      !
      RtnName = TRIM(RtnName)//"_AutoGen"
      !
      ! --- Convert to lower CASE for filename
      !
      RtnName_lw = RtnName
      CALL Up2Lw(RtnName_lw)
      !
      OPEN(UNIT=ICOD2, FILE=TRIM(RtnName_lw)//".f90", STATUS='UNKNOWN')
      REWIND(ICOD2)
      !
      ! --- Header
      !
      WRITE(ICOD2, '(6X, "SUBROUTINE ", $)')
      WRITE(ICOD2, *) TRIM(RtnName)//" &"
      WRITE(ICOD2, *) "     &   "// &
     &   "(V, T, W, nAo, nIo, nAt, nIt, mCK, mBJ, mAo, ", &
     &   "mIo, mAt, mIt, mAn, mIn)"
      WRITE(ICOD2, '( &
     &   "!", /, &
     &   "!", 5X, "o This is an automatically generated program", /, &
     &   "!", /, &
     &   "!", 5X, "Contract intermediate V(CK,BJ,Ao,Io) to T(BJ,At,It)", /, &
     &   "!", 5X, "to generate a new intermediate W(C,K,An,In)", /, &
     &   "!" &
     &   )')
      !
      WRITE(ICOD2, '(6X, "USE CC_Module, ONLY : NOc, NVr, IArcWgtOcc, IArcWgtVir")')
      IF (Complx) THEN
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : OneC, ZeroC")')
      ELSE
         WRITE(ICOD2, '(6X, "USE CC_Constant_Module, ONLY : One, Zero")')
      END IF
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "IMPLICIT NONE")')
      !
      ! --- Declare in/out variables
      !
      WRITE(ICOD2, '("!")')
      WRITE(ICOD2, '(6X, "INTEGER :: nAo, nIo, nAt, nIt, mCK, mBJ, mAo, mIo, ", &
     &   "mAt, mIt, mAn, mIn")')
      IF (Vector) THEN
         IF (Complx) THEN
            WRITE(ICOD2, '( &
     &         6X, "COMPLEX(8):: V(mCK,mBJ,mAo,mIo), ", &
     &         "T(mBJ,mAt,mIt), W(mCK,mAn,mIn)")')
         ELSE
            WRITE(ICOD2, '( &
     &         6X, "REAL(8):: V(mCK,mBJ,mAo,mIo), ", &
     &         "T(mBJ,mAt,mIt), W(mCK,mAn,mIn)")')
         END IF
      ELSE
         IF (Complx) THEN
            WRITE(ICOD2, '( &
     &         6X, "COMPLEX(8):: V(mBJ,mAo,mIo), ", &
     &         "T(mBJ,mAt,mIt), W(mAn,mIn)")')
         ELSE
            WRITE(ICOD2, '( &
     &         6X, "REAL(8):: V(mBJ,mAo,mIo), ", &
     &         "T(mBJ,mAt,mIt), W(mAn,mIn)")')
         END IF
      END IF
      !
      ! --- Declare local variables
      !
      WRITE(ICOD2, '("!")')
      IF (Complx) THEN
         WRITE(ICOD2, '( &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "COMPLEX(8) :: Alpha", /, &
     &      6X, "INTEGER:: NStringAt(nAt), NStringIt(nIt), ", &
     &      "NStringAo(nAo), NStringIo(nIo), ", &
     &      "NStringAn(nAo+nAt), NStringIn(nIo+nIt)", /)')
         WRITE(ICOD2, '(6X, "COMPLEX(8), ALLOCATABLE :: U(:,:,:)")')
      ELSE
         WRITE(ICOD2, '( &
     &      6X, "LOGICAL:: COIN", /, &
     &      6X, "REAL(8) :: Alpha", /, &
     &      6X, "INTEGER:: NStringAt(nAt), NStringIt(nIt), ", &
     &      "NStringAo(nAo), NStringIo(nIo), ", &
     &      "NStringAn(nAo+nAt), NStringIn(nIo+nIt)", /)')
         WRITE(ICOD2, '(6X, "REAL(8), ALLOCATABLE :: U(:,:,:)")')
      END IF
!072112Bgn
      !
      ! --- Integer scalars
      !
      WRITE(ICOD2, '(6X, "INTEGER :: IAddIo")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddAo")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddIt")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddAt")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddIn")')
      WRITE(ICOD2, '(6X, "INTEGER :: IAddAn")')
      WRITE(ICOD2, '(6X, "INTEGER :: ISgnIn")')
      WRITE(ICOD2, '(6X, "INTEGER :: ISgnAn")')
      IF (.NOT. Vector) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: IAddAtIt")')
      END IF
      !
      IF (nIo /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iIo00, IAddIo00")')
         DO iIo = 1, (nIo - 1)
            CALL Int2Char(iIo, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iIo", A2, ", IAddIo", A2)') a21, a21
         END DO
      END IF
      !
      IF (nAo /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iAo00, IAddAo00")')
         DO iAo = 1, (nAo - 1)
            CALL Int2Char(iAo, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iAo", A2, ", IAddAo", A2)') a21, a21
         END DO
      END IF
      !
      IF (nIt /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iIt00, IAddIt00")')
         DO iIt = 1, (nIt - 1)
            CALL Int2Char(iIt, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iIt", A2, ", IAddIt", A2)') a21, a21
         END DO
      END IF
      !
      IF (nAt /= 0) THEN
         WRITE(ICOD2, '(6X, "INTEGER :: iAt00, IAddAt00")')
         DO iAt = 1, (nAt - 1)
            CALL Int2Char(iAt, a21, "0")
            WRITE(ICOD2, '(6X, "INTEGER :: iAt", A2, ", IAddAt", A2)') a21, a21
         END DO
      END IF
!072012End
      !
      ! --- Allocate work array
      !
      WRITE(ICOD2, '("!")')
      IF (Vector) THEN
         WRITE(ICOD2, '(6X, "ALLOCATE(U(mCK,mAt,mIt))")')
      ELSE
         WRITE(ICOD2, '(6X, "ALLOCATE(U(mAt*mIt,mAo,mIo))")')
      END IF
      !
      ! --- Contraction (Scalar case)
      !
      IF (.NOT. Vector) THEN
         WRITE(ICOD2, '("!")')
         IF (Complx) THEN
            WRITE(ICOD2, '(6X, &
     &         "CALL ZGEMM(""T"", ""N"", (mAt*mIt), (mAo*mIo), mBJ, OneC, ", &
     &         "T, mBJ, V, mBJ, ZeroC, U, (mAt*mIt))")')
         ELSE
            WRITE(ICOD2, '(6X, &
     &         "CALL DGEMM(""T"", ""N"", (mAt*mIt), (mAo*mIo), mBJ, One, ", &
     &         "T, mBJ, V, mBJ, Zero, U, (mAt*mIt))")')
         END IF
      END IF
      !
      ! --- Loop over Io (External holes in V)
      !
      IF (nIo /= 0) THEN
         !   
         WRITE(ICOD2, '("!")')
         CALL Int2Char(nIo, a21, "0")
         WRITE(ICOD2, '(6X, "DO iIo00 = ", I2, ", NOc", $)') nIo
         WRITE(ICOD2, '("   ! loop over external hole(s) in V")')
         WRITE(ICOD2, '(9X, "NStringIo(", I2, ") = iIo00")') nIo
         WRITE(ICOD2, '(9X, "IAddIo00 = 1 + IArcWgtOcc(iIo00, ", I2, ", nIo)")') nIo
         IF (nIo == 1) THEN
            WRITE(ICOD2, '(9X, "IAddIo = IAddIo00")')
         END IF
         DO iIo = (nIo - 1), 1, -1
            CALL Int2Char((nIo-iIo), a21, "0")
            CALL Int2Char((nIo-iIo-1), a22, "0")
            CALL Int2Char(iIo, a23, " ")
            WRITE(ICOD2, '(6X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIo"//a21//" = "//a23//", iIo"//a22//" - 1")
            WRITE(ICOD2, '(9X, "NStringIo(", I2, ") = ", $)') iIo
            WRITE(ICOD2, *) TRIM("iIo"//a21)
!072012Bgn
            WRITE(ICOD2, '(9X, "IAddIo", A2, " = IAddIo", A2, " + IArcWgtOcc(iIo", &
     &         A2, ", ", I2, ", nIo)")') a21, a22, a21, iIo
            IF (iIo == 1) THEN
               WRITE(ICOD2, '(9X, "IAddIo = IAddIo", A2)') a21
            END IF
!072012End
         END DO
         !      
      ELSE
         !
         WRITE(ICOD2, '(9X, "IAddIo = 1")')
         !
      END IF
      !
      ! --- Loop over Ao (External particles in V)
      !
      WRITE(ICOD2, '("!")')
      !
      IF (nAo /= 0) THEN
         !
         CALL Int2Char(nAo, a21, "0")
         WRITE(ICOD2, '(9X, "DO iAo00 = ", I2, ", NVr", $)') nAo
         WRITE(ICOD2, '("   ! loop over external particle(s) in V")')
         WRITE(ICOD2, '(12X, "NStringAo(", I2, ") = iAo00")') nAo
         WRITE(ICOD2, '(12X, "IAddAo00 = 1 + IArcWgtVir(iAo00, ", I2, ", nAo)")') nAo
         IF (nAo == 1) THEN
            WRITE(ICOD2, '(12X, "IAddAo = IAddAo00")')
         END IF
         DO iAo = (nAo - 1), 1, -1
            CALL Int2Char((nAo - iAo), a21, "0")
            CALL Int2Char((nAo - iAo - 1), a22, "0")
            CALL Int2Char(iAo, a23, " ")
            WRITE(ICOD2, '(9X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAo"//a21//" = "//a23//", iAo"//a22//" - 1")
            WRITE(ICOD2, '(12X, "NStringAo(", I2, ") = ", $)') iAo
            WRITE(ICOD2, *) TRIM("iAo"//a21)
!072012Bgn
            WRITE(ICOD2, '(12X, "IAddAo", A2, " = IAddAo", A2, " + IArcWgtVir(iAo", &
     &         A2, ", ", I2, ", nAo)")') a21, a22, a21, iAo
            IF (iAo == 1) THEN
               WRITE(ICOD2, '(12X, "IAddAo = IAddAo", A2)') a21
            END IF
!072012End
         END DO
         !
      ELSE
         !
         WRITE(ICOD2, '(12X, "IAddAo = 1")')
         !
      END IF
      !
      ! --- Contraction (Vector case)
      !
      IF (Vector) THEN
         WRITE(ICOD2, '("!")')
         IF (Complx) THEN
            WRITE(ICOD2, '(12X, &
     &         "CALL ZGEMM(""N"", ""N"", mCK, (mAt*mIt), mBJ, OneC, ", &
     &         "V(1,1,IAddAo,IAddIo), mCK, T, mBJ, ZeroC, U, mCK)")')
         ELSE
            WRITE(ICOD2, '(12X, &
     &         "CALL DGEMM(""N"", ""N"", mCK, (mAt*mIt), mBJ, One, ", &
     &         "V(1,1,IAddAo,IAddIo), mCK, T, mBJ, Zero, U, mCK)")')
         END IF
      END IF
      !
      ! --- Loop over It (Internal holes in T)
      !
      WRITE(ICOD2, '("!")')
      !
      IF (nIt /= 0) THEN
         !
         CALL Int2Char(nIt, a21, "0")
         WRITE(ICOD2, '(12X, "DO iIt00 = ", I2, ", NOc ", $)') nIt
         WRITE(ICOD2, '("   ! loop over external hole(s) in V")')
         !
         ! --- Check coincidence with StringIo
         !
         DO iIo = 1, nIo
            CALL Int2Char(iIo-1, a24, "0")
            WRITE(ICOD2, '(15X, "IF (iIt00 == iIo", A2, ") CYCLE")') a24
         END DO
         !
         WRITE(ICOD2, '(15X, "NStringIt(", I2, ") = iIt00")') nIt
         WRITE(ICOD2, '(15X, "IAddIt00 = 1 + IArcWgtOcc(iIt00, ", I2, ", nIt)")') nIt
         IF (nIt == 1) THEN
            WRITE(ICOD2, '(15X, "IAddIt = IAddIt00")')
         END IF
         DO iIt = (nIt - 1), 1, -1
            CALL Int2Char((nIt - iIt), a21, "0")
            CALL Int2Char((nIt - iIt - 1), a22, "0")
            CALL Int2Char(iIt, a23, " ")
            WRITE(ICOD2, '(12X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iIt"//a21//" = "//a23//", iIt"//a22//" - 1")
            !
            ! --- Check coincidence with StringIo
            !
            DO iIo = 1, nIo
               CALL Int2Char(iIo-1, a24, "0")
               WRITE(ICOD2, '(15X, "IF (iIt", A2, " == iIo", A2, ") CYCLE")') a21, a24
            END DO
            !
            WRITE(ICOD2, '(15X, "NStringIt(", I2, ") = ", $)') iIt
            WRITE(ICOD2, *) TRIM("iIt"//a21)
!072012Bgn
            WRITE(ICOD2, '(15X, "IAddIt", A2, " = IAddIt", A2, " + IArcWgtOcc(iIt", &
     &         A2, ", ", I2, ", nIt)")') a21, a22, a21, iIt
            IF (iIt == 1) THEN
               WRITE(ICOD2, '(15X, "IAddIt = IAddIt", A2)') a21
            END IF
!072012End
         END DO
         !
         ! --- String Io.It
         !
         IF (nIo /= 0) THEN   ! nIt /= 0 and nIo /= 0
            WRITE(ICOD2, '( &
     &         15X, "COIN = .FALSE.", /, &
     &         15X, "CALL CCLib_MergeStrings(NStringIo, NStringIt, ", &
     &         "NStringIn, nIo, nIt, ISgnIn, COIN)", /, &
     &         15X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '(15X, &
     &           "CALL CCLib_StringAddress(NStringIn, nIo+nIt, ""Occ"", IAddIn)")')
!072112     &           18X, "CALL CCLib_StringAddress(NStringIo, nIo, ""Occ"", IAddIo)")')
         ELSE   ! nIt /= 0 and nIo = 0
!072112            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIo, nIo, ""Occ"", IAddIo)")')
            WRITE(ICOD2, '(15X, "ISgnIn = 1", /, &
     &         15X, "IAddIn = IAddIt")')
         END IF
         !
      ELSE   ! nIt == 0
         !
         WRITE(ICOD2, '(12X, "IAddIt = 1")')
         IF (nIo /= 0) THEN   ! nIt = 0 and nIo /= 0
!072112            WRITE(ICOD2, '(15X, "CALL CCLib_StringAddress(NStringIt, nIt, ""Occ"", IAddIt)")')
            WRITE(ICOD2, '(12X, "ISgnIn = 1", /, &
     &                    12X, "IAddIn = IAddIo")')
         ELSE   ! nIt = 0 and nIo = 0
            WRITE(ICOD2, '(12X, "ISgnIn = 1", /, &
     &         12X, "IAddIn = 1")')
         END IF
         !
      END IF
      !
      ! --- Loop over At (ext. particle in T)
      !
      WRITE(ICOD2, '("!")')
      IF (nAt /= 0) THEN
         !
         CALL Int2Char(nAt, a21, "0")
         WRITE(ICOD2, '(12X, "DO iAt00 = ", I2, ", NVr", $)') nAt
         WRITE(ICOD2, '("   ! loop over external particle(s) in V")')
         !
         ! --- Check coincidence with StringAo
         !
         DO iAo = 1, nAo
            CALL Int2Char(iAo-1, a24, "0")
            WRITE(ICOD2, '(15X, "IF (iAt00 == iAo", A2, ") CYCLE")') a24
         END DO
         !
         WRITE(ICOD2, '(15X, "NStringAt(", I2, ") = iAt00")') nAt
         WRITE(ICOD2, '(15X, "IAddAt00 = 1 + IArcWgtVir(iAt00, ", I2, ", nAt)")') nAt
         IF (nAt == 1) THEN
            WRITE(ICOD2, '(15X, "IAddAt = IAddAt00")')
         END IF
         DO iAt = (nAt-1), 1, -1
            CALL Int2Char((nAt-iAt), a21, "0")
            CALL Int2Char((nAt-iAt-1), a22, "0")
            CALL Int2Char(iAt, a23, " ")
            WRITE(ICOD2, '(12X, "DO ", $)')
            WRITE(ICOD2, *) TRIM("iAt"//a21//" = "//a23//", iAt"//a22//" - 1")
            !
            ! --- Check coincidence with StringAo
            !
            DO iAo = 1, nAo
               CALL Int2Char(iAo-1, a24, "0")
               WRITE(ICOD2, '(15X, "IF (iAt", A2, " == iAo", A2, ") CYCLE")') a21, a24
            END DO
            !
            WRITE(ICOD2, '(15X, "NStringAt(", I2, ") = ", $)') iAt
            WRITE(ICOD2, *) TRIM("iAt"//a21)
!072012Bgn
            WRITE(ICOD2, '(15X, "IAddAt", A2, " = IAddAt", A2, " + IArcWgtVir(iAt", &
     &         A2, ", ", I2, ", nAt)")') a21, a22, a21, iAt
            IF (iAt == 1) THEN
               WRITE(ICOD2, '(15X, "IAddAt = IAddAt", A2)') a21
            END IF
!072012End
         END DO
         !
         ! --- String Ao.At
         !
         IF (nAo /= 0) THEN   ! nAt /= 0 and nAo /= 0
            WRITE(ICOD2, '( &
     &         15X, "COIN = .FALSE.",/, &
     &         15X, "CALL CCLib_MergeStrings(NStringAo, NStringAt, ", &
     &         "NStringAn, nAo, nAt, ISgnAn, COIN)",/, &
     &         15X, "IF (COIN) CYCLE")')
            WRITE(ICOD2, '( &
     &         15X, "CALL CCLib_StringAddress(NStringAn, nAo+nAt, ", &
     &         """Vir"", IAddAn)")')
!072212     &         18X,"CALL CCLib_StringAddress(NStringAo, nAo, ", &
!072112     &         """Vir"", IAddAo)")')
         ELSE   ! nAt /= 0 and nAo = 0
!072112            WRITE(ICOD2, '( &
!072121     &         18X, "CALL CCLib_StringAddress(NStringAo, nAo, ", &
!072112     &           """Vir"", IAddAo)")')
            WRITE(ICOD2, '(15X, "ISgnAn = 1", /, &
     &                     15X, "IAddAn = IAddAt")')
         END IF
         !
      ELSE   ! nAt = 0
         !
         WRITE(ICOD2, '(15X, "IAddAt = 1")')
         IF (nAo /= 0) THEN   ! nAt = 0 and nAo /= 0
!072112            WRITE(ICOD2, '(18X, "CALL CCLib_StringAddress(NStringAt, nAt, ""Vir"", IAddAt)")')
            WRITE(ICOD2, '(15X, "ISgnAn = 1", /, &
     &                     15X, "IAddAn = IAddAo")')
         ELSE   ! nAt = 0 and nAo = 0
            WRITE(ICOD2, '(15X, "ISgnAn = 1", /, &
     &                     15X, "IAddAn = 1")')
         END IF
         !
      END IF
      !
      ! --- Substitute appropriate elements of U into W
      !
      WRITE(ICOD2, '("!")')
      IF (Vector) THEN
         IF (Complx) THEN
            WRITE(ICOD2, '(18X, "Alpha = DCMPLX(ISgnAn * ISgnIn)")')
            WRITE(ICOD2, '(18X, "CALL ZAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)")')
         ELSE
            WRITE(ICOD2, '(18X, "Alpha = DBLE(ISgnAn * ISgnIn)")')
            WRITE(ICOD2, '(18X, "CALL DAXPY(mCK, Alpha, U(1,IAddAt,IAddIt), 1, W(1,IAddAn,IAddIn), 1)")')
         END IF
      ELSE
         IF (Complx) THEN
            WRITE(ICOD2, '(18X, "Alpha = DCMPLX(ISgnAn * ISgnIn)")')
         ELSE
            WRITE(ICOD2, '(18X, "Alpha = DBLE(ISgnAn * ISgnIn)")')
         END IF
         WRITE(ICOD2, '(18X, "IAddAtIt = mAt * (IAddIt - 1) + IAddAt")')
         WRITE(ICOD2, '(18X, "W(IAddAn,IAddIn) = W(IAddAn,IAddIn) + Alpha * U(IAddAtIt,IAddAo,IAddIo)")')
      END IF
      !
      ! --- Close loops
      !
      IF (nAt /= 0) THEN
         WRITE(ICOD2, '("!")')
         WRITE(ICOD2, '(15X, "END DO   ! T ext. particles")')
         DO iAt = 2, nAt
            WRITE(ICOD2, '(15X, "END DO")')
         END DO
      END IF
      !
      IF (nIt /= 0) THEN
         WRITE(ICOD2, '("!")')
         WRITE(ICOD2, '(12X, "END DO   ! T ext. holes")')
         DO iIt = 2, nIt
            WRITE(ICOD2, '(12X, "END DO")')
         END DO
      END IF
      !
      IF (nAo /= 0) THEN
         WRITE(ICOD2, '("!")')
         WRITE(ICOD2, '(9X, "END DO   ! V ext. particles")')
         DO iAo = 2, nAo
            WRITE(ICOD2, '(9X, "END DO")')
         END DO
      END IF
      !
      IF (nIo /= 0) THEN
         WRITE(ICOD2, '("!")')
         WRITE(ICOD2, '(6X, "END DO   ! V ext. holes")')
         DO iIo = 2, nIo
            WRITE(ICOD2, '(6X, "END DO")')
         END DO
      END IF
      !
      ! --- Deallocate work array
      !
      WRITE(ICOD2, '(6X, "DEALLOCATE(U)")')
      !
      ! --- End of the SUBROUTINE
      !
      WRITE(ICOD2, '("!", /, 6X, "END SUBROUTINE")')
      !
      CLOSE(ICOD2)
      !
      END SUBROUTINE
