      SUBROUTINE CC_Trn1_Cqrs(G1)
!
      USE CC_Module, ONLY : NVr, NMO, Name, CMOA, CMOB, NOcA, NOcB, NVrA, NVrB, NBF, &
     &   MaxBuf, ERIBuf, NBuf, NLine, Lp, Lq, Lr, Ls
      USE CC_Constant_Module, ONLY : Half
!
!     o 1st virtual transformation (Cq|rs)
!       Integrals are stored as (rs,q,C)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1((NBF*(NBF+1))/2,NBF,*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FName, FBuf
      INTEGER :: ILine, MBuf, IBuf, Ip, Iq, Ir, Is, Indpq, Indrs, IvrA, IVrB, IvrAX, IVrBX, IVrBY
      REAL(8) :: ERI
!
!     o Open ERI information file
!
      FName = TRIM(Name)//".ERI_Info"
      OPEN(UNIT=IO1, FILE=TRIM(FName), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='UNFORMATTED', ERR=9000)
      REWIND(IO1)
      READ(IO1) NLine, NBuf
      CLOSE(IO1)
      GO TO 9010
 9000 STOP 'No ERI_Info file found.'
 9010 CONTINUE
!
!     o Open AO integral file
!
      FBuf = TRIM(Name)//".ERI"
      OPEN(UNIT=IO1, FILE=TRIM(FBuf), STATUS='OLD', ACCESS='SEQUENTIAL', FORM='UNFORMATTED', ERR=9100)
      GO TO 9110
 9100 STOP 'No ERI file found.'
 9110 CONTINUE
!
!     o 1st transformation
!
      CALL CCLib_DClear(G1, (NBF*(NBF+1))/2*NBF*NVr)
      DO ILine = 1, NLine
!
         IF ((ILine /= NLine) .OR. ((ILine == NLine) .AND. (NBuf == 0))) THEN
            MBuf = MaxBuf
         ELSE IF ((ILine == NLine) .AND. (NBuf /= 0)) THEN
            MBuf = NBuf
         END IF
!
         READ(IO1) (Lp(IBuf), Lq(IBuf), Lr(IBuf), Ls(IBuf), ERIBuf(IBuf), IBuf = 1, MBuf)
!
         DO IBuf = 1, MBuf
            Ip = Lp(IBuf)
            Iq = Lq(IBuf)
            Ir = Lr(IBuf)
            Is = Ls(IBuf)
            ERI = ERIBuf(IBuf)
            IF (Ip == Iq) ERI = ERI * Half
            IF (Ir == Is) ERI = ERI * Half
            IF ((Ip == Ir) .AND. (Iq == Is)) ERI = ERI * Half
            Indpq = (Ip * (Ip - 1)) / 2 + Iq
            Indrs = (Ir * (Ir - 1)) / 2 + Is
!
            DO IvrA = 1, NVrA   ! loop over alpha vir. orbs
               IvrAX = NOcA + IvrA
               G1(Indrs,Iq,IvrA) = G1(Indrs,Iq,IvrA) + CMOA(Ip,IvrAX) * ERI
               G1(Indrs,Ip,IvrA) = G1(Indrs,Ip,IvrA) + CMOA(Iq,IvrAX) * ERI
               G1(Indpq,Is,IvrA) = G1(Indpq,Is,IvrA) + CMOA(Ir,IvrAX) * ERI
               G1(Indpq,Ir,IvrA) = G1(Indpq,Ir,IvrA) + CMOA(Is,IvrAX) * ERI
            END DO
            DO IVrB = 1, NVrB   ! loop over beta vir. orbs
               IVrBX = NVrA + IVrB
               IVrBY = NOcB + IVrB
               G1(Indrs,Iq,IVrBX) = G1(Indrs,Iq,IVrBX) + CMOB(Ip,IVrBY) * ERI
               G1(Indrs,Ip,IVrBX) = G1(Indrs,Ip,IVrBX) + CMOB(Iq,IVrBY) * ERI
               G1(Indpq,Is,IVrBX) = G1(Indpq,Is,IVrBX) + CMOB(Ir,IVrBY) * ERI
               G1(Indpq,Ir,IVrBX) = G1(Indpq,Ir,IVrBX) + CMOB(Is,IVrBY) * ERI
            END DO
!
         END DO
!
      END DO
!
!     o Close ERI file
!
      REWIND(IO1)
      CLOSE(IO1)
!
      END SUBROUTINE
