subroutine rearrange(nIo, nAo, nJ, nB, nK, nC, name_V, name_V_)

  use CC_Module, only : NOc, NVr, NOcA, NVrA
  
  implicit none

  integer, intent(in) :: nIo, nAo, nJ, nB, nK, nC
  character(128), intent(in) :: name_V, name_V_


  allocate(Jstring(nJ))
  allocate(Bstring(nB))
  allocate(Kstring(nK))
  allocate(Cstring(nC))
  allocate(KJstring(nK+nJ))
  allocate(CBstring(nC+nB))

  IoLen = stringLengthOcc(nIo)
  AoLen = stringLengthVir(nAo)
  JLen  = stringLengthOcc(nJ)
  BLen  = stringLengthVir(nB)
  KLen  = stringLengthOcc(nK)
  CLen  = stringLengthVir(nC)

  size2  = C(NOc, nK+nJ) * C(NVr, nB+nC)
  size2_ = C(nOc, nK) * C(NOc, nJ) * C(NVr, nB) * C(NVr, nC)
  
  open(unit=io1, file=trim(name_V), access="direct", recl=size2*8, form="unformatted")
  open(unit=io2, file=trim(name_V_), access="direct", recl=size2_*8, form="unformatted")

  do IoAdd = 1, IoLen
     do AoAdd = 1, AoLen
        loc = (IoAdd - 1) * stringLengthVir(nAo) + AoAdd
        ! load V2(*,Ao,Io)
        read(io1, rec=loc) V2(1:size2)

        do JAdd = 1, stringLengthOcc(nJ)
           ! load J string
           Jstring = 
           do BAdd = 1, stringLengthVir(nB)
              ! load B string
              Bstring = 
              do KAdd = 1, stringLengthOcc(nK)
                 ! load K string
                 Kstring = 
                 ! merge K and J; get address
                 do CAdd = 1, stringLengthVir(nC)
                    ! load C string
                    Cstring = 
                    ! merge C and B; get address
                    V2_(CAdd,KAdd,BAdd,JAdd) = V2(CBAdd, KJAdd) * Fac
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

end subroutine rearrange
