      SUBROUTINE CC_AntiSym_IJAB(G1, G2)
!
      USE CC_Module, ONLY : NMO, NMOA, NMOB, NOc, NOcA, NOcB, NVr, NVrA, NVrB
!
!     o Anti-symmetrize (IQ|AP) integrals
!       (IJ|AB) = (IJ|AB) - (IB|AJ) into G1(B,I,A,J)
!
      IMPLICIT NONE
!
      REAL(8), INTENT(OUT) :: G1(NVr,NOc,NVr,*)
      REAL(8), INTENT(IN) :: G2(NMO,NMO,NVr,*)
!
      LOGICAL :: IOcAlpha, IOcBeta, IVAlpha, IVrBeta, SpinMix
      INTEGER :: IOc, IVr, JOc, JVr, JOcEnd, JVrEnd, JOcOff1, JOcOff2, JVrOff1, JVrOff2
!
      CALL CCLib_DClear(G1, NVr*NOc*NVr*NOc)
!
      DO IOc = 1, NOc
         IOcAlpha = (IOc <= NOcA)
         IOcBeta = (IOc > NOcA)
         DO IVr = 1, NVr
            IVAlpha = (IVr <= NVrA)
            IVrBeta = (IVr > NVrA)
!
            SpinMix = ((IOcAlpha .AND. IVrBeta) .OR. (IOcBeta .AND. IVAlpha))
!
            IF (SpinMix) THEN
               IF (IOcAlpha .AND. IVrBeta) THEN
                  JOcEnd = NOcA
                  JVrEnd = NVrB
                  JOcOff1 = 0
                  JVrOff1 = NVrA
                  JOcOff2 = 0
                  JVrOff2 = NMOA + NOcB
               ELSE IF (IOcBeta .AND. IVAlpha) THEN
                  JOcEnd = NOcB
                  JVrEnd = NVrA
                  JOcOff1 = NOcA
                  JVrOff1 = 0
                  JOcOff2 = NMOA
                  JVrOff2 = NOcA
               END IF
               DO JOc = 1, JOcEnd
                  DO JVr = 1, JVrEnd
                     G1((JVr+JVrOff1),IOc,IVr,(JOc+JOcOff1)) = G2((JVr+JVrOff2),(JOc+JOcOff2),IVr,IOc)
                  END DO
               END DO
               IF (IOcAlpha .AND. IVrBeta) THEN
                  JOcEnd = NOcB
                  JVrEnd = NVrA
                  JOcOff1 = NOcA
                  JVrOff1 = 0
                  JOcOff2 = NMOA
                  JVrOff2 = NOcA
               ELSE IF (IOcBeta .AND. IVAlpha) THEN
                  JOcEnd = NOcA
                  JVrEnd = NVrB
                  JOcOff1 = 0
                  JVrOff1 = NVrA
                  JOcOff2 = 0
                  JVrOff2 = NMOA + NOcB
               END IF
               DO JOc = 1, JOcEnd
                  DO JVr = 1, JVrEnd
                     G1((JVr+JVrOff1),IOc,IVr,(JOc+JOcOff1)) = -G2((JOc+JOcOff2),(JVr+JVrOff2),IVr,IOc)
                  END DO
               END DO
            ELSE   ! .not. SpinMix
               IF (IOcAlpha .AND. IVAlpha) THEN
                  JOcEnd = NOcA
                  JVrEnd = NVrA
                  JOcOff1 = 0
                  JVrOff1 = 0
                  JOcOff2 = 0
                  JVrOff2 = NOcA
               ELSE IF (IOcBeta .AND. IVrBeta) THEN
                  JOcEnd = NOcB
                  JVrEnd = NVrB
                  JOcOff1 = NOcA
                  JVrOff1 = NVrA
                  JOcOff2 = NMOA
                  JVrOff2 = NMOA + NOcB
               END IF
               DO JOc = 1, JOcEnd
                  DO JVr = 1, JVrEnd
                     G1((JVr+JVrOff1),IOc,IVr,(JOc+JOcOff1)) = G2((JVr+JVrOff2),(JOc+JOcOff2),IVr,IOc) &
     &                  - G2((JOc+JOcOff2),(JVr+JVrOff2),IVr,IOc)
                  END DO
               END DO
            END IF
         END DO
      END DO
!
      END SUBROUTINE
