      SUBROUTINE CCLib_MergeStrings(NString1, NString2, NString12, N1, N2, ISgn, COIN)
!
!     Merge two strings
!     In : NString1(1:N1), NString2(1:N2) (arranged in ascending order)
!     Out: NString12(1:N1+N2), ISgn(+1 or -1)
!
      IMPLICIT NONE
!
      LOGICAL :: COIN
      INTEGER :: N1, N2, ISgn
      INTEGER :: NString1(N1), NString2(N2), NString12(N1+N2)
!
      INTEGER :: I1, J1, NS1, I2, NS2, Npermu, Nst1, Nst2
!
!     o Check coincidence
!
      COIN = .FALSE.
      DO I1 = 1, N1
         NS1 = NString1(I1)
         DO I2 = 1, N2
            NS2 = NString2(I2)
            IF (NS1 == NS2) THEN
               COIN = .TRUE.
               EXIT
            END IF
         END DO
         IF (COIN) EXIT
      END DO
      IF (COIN) RETURN
!
!     o Merge two strings
!
      Npermu = 0   ! # of permutations
!YA061512      Ncnt = 0   ! # of filled KJ indices
!
      DO I1 = 1, N1
         NString12(I1) = NString1(I1)
      END DO
!
      DO I2 = 1, N2
         Nst2 = NString2(I2)
         DO I1 = N1, 1, -1
            Nst1 = NString1(I1)
!            
            IF (Nst2 < Nst1) THEN
               Npermu = Npermu + 1
               IF (I1 == 1) THEN
                  NString12(I2) = Nst2
                  DO J1 = 1, N1
                     NString12(I2+J1) = NString1(J1)
                  END DO               
               END IF
            ELSE
               NString12(I2-1+I1+1) = Nst2
               DO J1 = I1 + 1, N1
                  NString12(I2+J1) = NString1(J1)
               END DO              
               EXIT
            END IF
!
         END DO
      END DO
!
      ISgn = 1
      IF (MOD(Npermu,2) /= 0) ISgn = -1
!
      END SUBROUTINE
