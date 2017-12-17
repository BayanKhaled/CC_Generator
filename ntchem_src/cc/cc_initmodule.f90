      SUBROUTINE CC_InitModule
!
      USE CC_Module, ONLY : NBF, NMOA, NMOB, NMO, CMOA, CMOB, Ene, CMO
!
!     o Set up variables in CC_Module
!
      IMPLICIT NONE
!
!     o Take variables from SCF module
!
      CALL CC_SetVar
!
!     o Allocate MO and orb.ene. arrays
!
      ALLOCATE(CMOA(NBF,NMOA))
      ALLOCATE(CMOB(NBF,NMOB))
      ALLOCATE(CMO(NBF,NMO))
      ALLOCATE(Ene(NMO))
!
!     o MO coefficients and orbital energies
!
      CALL CC_GetMOandE
!
      END SUBROUTINE
