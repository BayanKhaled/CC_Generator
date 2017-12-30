subroutine rearrange(nIo, nAo, nJ, nB, nK, nC, name_V, name_V_)

  use CC_Module, only : NOc, NVr
  use String_Module, only : MapOcc, MapVir, NumStringOcc, NumStringVir
  implicit none

  ! Formal
  integer, intent(in) :: nIo, nAo, nJ, nB, nK, nC
  character(128), intent(in) :: name_V, name_V_
  ! Local
  integer :: size2, size2_
  integer :: IoLen, AoLen, JLen, BLen, KLen, CLen
  integer :: IoAdd, AoAdd, loc, JAdd, BAdd, KAdd, CAdd, KJAdd, CBAdd
  real(8) :: Fac, Fac0
  real(8), allocatable :: V2(:), V2_(:)


  IoLen = NumStringOcc(nIo)
  AoLen = NumStringVir(nAo)
  JLen  = NumStringOcc(nJ)
  BLen  = NumStringVir(nB)
!  KLen = MapOcc(nK,nJ)%length
!  CLen = MapVir(nC,nB)%length
  KLen = NumStringOcc(nK)
  CLen = NumStringVir(nC)
  

  size2  = CCLib_NComb(NOc, nK+nJ) * CCLib_NComb(NVr, nB+nC)
  size2_ = CCLib_NComb(nOc, nK) * CCLib_NComb(NOc, nJ) * &
           CCLib_NComb(NVr, nB) * CCLib_NComb(NVr, nC)

  allocate(V2(size2))
  allocate(V2_(size2_))
  
  open(unit=io1, file=trim(name_V), access="direct", recl=size2*8, &
       form="unformatted", status="old")
  open(unit=io2, file=trim(name_V_), access="direct", recl=size2_*8, &
       form="unformatted", status="unknown")

  do IoAdd = 1, IoLen
     do AoAdd = 1, AoLen
        loc = (IoAdd - 1) * AoLen + AoAdd
        ! load V2(*,Ao,Io)
        read(io1, rec=loc) V2(1:size2)
        do JAdd = 1, JLen
           do BAdd = 1, BLen
              do KAdd = 1, KLen
                 ! address and parity of KJ string
                 KJAdd = MapOcc(nK,nJ)%map(KAdd,JAdd)
                 if (KJAdd == 0) cycle
                 Fac0 = sgn(1, kJAdd)
                 do CAdd = 1, CLen
                    ! address and parity of CB string
                    CBAdd = MapVir(nC,nB)%map(CAdd,BAdd)
                    if (CBAdd == 0) cycle
                    Fac = Fac0 * sgn(1, CBAdd)
                    V2_(CAdd,KAdd,BAdd,JAdd) = V2(ABS(CBAdd),ABS(KJAdd)) * Fac
                 end do
              end do
           end do
        end do

        ! save V2_
        write(io2, rec=loc) V2_(1:size2_)
     end do
  end do

  close(io2)
  close(io1)
  deallocate(V2)
  deallocate(V2_)

end subroutine rearrange
