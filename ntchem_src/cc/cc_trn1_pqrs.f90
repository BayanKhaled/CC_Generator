      SUBROUTINE CC_Trn1_Pqrs(G1)
!
      USE CC_Module, ONLY : NMO, Name, CMO, NBF, MaxBuf, ERIBuf, NBuf, NLine, Lp, Lq, Lr, Ls
      USE CC_Constant_Module, ONLY : Half
!
!     o 1st occupied transformation (Pq|rs)
!                                        ^
!       Integrals are stored as (rs,q,P)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1((NBF*(NBF+1))/2,NBF,*)
!
      INTEGER, PARAMETER :: IO1 = 90
      CHARACTER(LEN=255) :: FName, FBuf
      INTEGER :: ILine, MBuf, IBuf, Ip, Iq, Ir, Is ,Indpq, Indrs, NSizeG1, IMO
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
      NSizeG1 = (NBF * (NBF + 1)) / 2 * NBF * NMO
      CALL CCLib_DClear(G1, NSizeG1)
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
            DO IMO = 1, NMO
               G1(Indrs,Iq,IMO) = G1(Indrs,Iq,IMO) + CMO(Ip,IMO) * ERI
               G1(Indrs,Ip,IMO) = G1(Indrs,Ip,IMO) + CMO(Iq,IMO) * ERI
               G1(Indpq,Is,IMO) = G1(Indpq,Is,IMO) + CMO(Ir,IMO) * ERI
               G1(Indpq,Ir,IMO) = G1(Indpq,Ir,IMO) + CMO(Is,IMO) * ERI
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
