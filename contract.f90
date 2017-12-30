subroutine contract(nIt, nAt, nIo, nAo, nJ, nB, nK, nC, name_T, name_V, name_W)

  use CC_Module, only : NOc, NVr
  implicit none

  ! Formal
  integer, intent(in) :: nIt, nAt, nIo, nAo, nJ, nB, nK, nC
  character(128), intent(in) :: name_T, name_V, name_W
  ! Local
  integer :: size2, size2_
  integer :: IoLen, AoLen, JLen, BLen, KLen, CLen
  integer :: IoAdd, AoAdd, loc, JAdd, BAdd, KAdd, CAdd, KJAdd, CBAdd
  real(8) :: Fac
  real(8), allocatable :: V2(:), V2_(:)

  ItLen = NumStringOcc(nIt)
  AtLen = NumStringVir(nAt)
  IoLen = NumStringOcc(nIo)
  AoLen = NumStringVir(nAo)
  JLen  = NumStringOcc(nJ)
  BLen  = NumStringVir(nB)
  TLen1 = NumStringVir(nB+nAt)
  TLen2 = NumStringOcc(nJ+nIt)

  allocate(F(BLen,JLen))
  allocate(T(TLen1))
  
  ! open T tensor file
  open(unit=io1, file=trim(name_T), access="direct", recl=TLen1*8, status="old")
  
  do ItAdd = 1, ItLen
     do AtAdd = 1, AtLen
        ! initialize F
        call CCLib_DClear(F, JLen*BLen)
        do JAdd = 1, JLen
           LAdd = MapOcc(nJ,nIt)%map(JAdd,ItAdd)
           if (LAdd == 0) cycle
           Fac0 = sgn(1,LAdd)
           ! Load T tensor
           read(unit=io1, rec=ABS(LAdd)) T
           do BAdd = 1, BLen
              DAdd = MapVir(nB,nAt)%map(BAdd,AtAdd)
              if (DAdd == 0) cycle
              Fac = Fac0 * sgn(1,DAdd)
              F(BAdd,JAdd) = Fac * T(ABS(DAdd))
           end do
        end do
        do IoAdd = 1, IoLen
           InAdd = MapOcc(nIo,nIt)%map(IoAdd,ItAdd)
           if (InAdd == 0) cycle
           do AoAdd = 1, AoLen
              AnAdd = MapVir(nAo,nAt)%map(AoAdd,AtAdd)
              if (AnAdd == 0) cycle
              ! Load W(C,K,B,J) at AoAdd, IoAdd
              ! Load V(C,K,An,In)


              ! Save V(C,K,An,In)
           end do
        end do
     end do
  end do

  ! close files
  close(io1)


end subroutine contract
