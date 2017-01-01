module cmd_line

  implicit none
  integer,parameter :: rk=selected_real_kind(10,20)
  integer,parameter :: MAXBUF=200
  character(len=MAXBUF),private :: argu

contains

  ! 
  ! Define here functions 
  ! 
  ! cmd2real(i) 
  ! of type real(rk) that reads the ith command line
  ! argument, converts it to real(rk) and returns this value
  !
  ! cmd2int(i) 
  ! of type integer that reads the ith command line
  ! argument, converts it to integer and returns this value
  ! 
  ! Note that constant 'rk' and variable 'argu' can be used here. Use
  ! 'internal io' to do the conversion.
  !
  real function cmd2real(i)
    implicit none
    integer ::i
    character(len=80):: arg
    real(kind=rk) ::x

    call get_command_argument(i,arg)
    read(arg,*) x
    cmd2real=x
  end function cmd2real

  integer function cmd2int(i)
    implicit none
    integer ::i,x
    character(len=80) :: arg

    call get_command_argument(i,arg)
    read(arg,*) x
    cmd2int=x
    end function cmd2int
end module cmd_line

    
