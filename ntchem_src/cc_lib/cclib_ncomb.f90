      INTEGER FUNCTION CCLib_NComb(N, M)
!
!     o N-combination-n
!
      IMPLICIT NONE
!
      INTEGER :: N, M
!
      INTEGER :: I, NNum, NDen
!
      IF ((M == 0) .OR. (M == N)) THEN
         CCLib_NComb = 1
         GO TO 100
      ELSE IF (M == 1) THEN
         CCLib_NComb = N
         GO TO 100
      ELSE IF (M == 2) THEN
         CCLib_NComb = (N * (N - 1)) / 2
         GO TO 100
      END IF
!
      NNum = N
      NDen = M
      DO I = 2, M
         NNum = NNum * (N - (I - 1))
      END DO
      DO I = 2, M
         NDen = NDen * (M - (I - 1))
      END DO
      CCLib_NComb = NNum / NDen
!
  100 CONTINUE
!
      END FUNCTION
