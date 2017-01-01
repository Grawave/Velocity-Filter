module vectors
  implicit none
  type :: vector
     real :: x,y,z
  end type vector

  interface operator(+)
     module procedure vAdd
  end interface operator(+)

  interface operator(-)
     module procedure vSub
  end interface operator(-)

  !CrossProduct
  interface operator(.X.)
     module procedure vCross
  end interface operator(.X.)

  interface operator(*)
     module procedure scalarMult
     module procedure scalarMult2
  end interface operator(*)

  interface operator(/)
     module procedure scalarDiv
  end interface operator(/)
  
contains

  type(vector) function scalarDiv(vec,scal)
    type(vector), intent(in) :: vec
    real, intent(in) :: scal
    scalarDiv%x = vec%x/scal
    scalarDiv%y = vec%y/scal
    scalarDiv%z = vec%z/scal
  end function scalarDiv
  
  type(vector) function vSub(a,b)
    type(vector), intent(in) ::a,b
    vSub%x = a%x - b%x
    vSub%y = a%y - b%y
    vSub%z = a%z - b%z
  end function vSub

  type(vector) function scalarMult2(vec,scalar)
    implicit none
    type(vector), intent(in) :: vec
    real, intent(in) :: scalar
    scalarMult2%x=vec%x*scalar
    scalarMult2%y=vec%y*scalar
    scalarMult2%z=vec%z*scalar
  end function scalarMult2
  
  type(vector) function scalarMult(scalar,vec)
    implicit none
    type(vector), intent(in) :: vec
    real, intent(in) :: scalar
    scalarMult%x=vec%x*scalar
    scalarMult%y=vec%y*scalar
    scalarMult%z=vec%z*scalar
  end function scalarMult
  
  type(vector) function vAdd(a,b)
    implicit none
    type(vector), intent(in) ::a,b
    vAdd%x = a%x + b%x
    vAdd%y = a%y + b%y
    vAdd%z = a%z + b%z
  end function vAdd

  !Crossproduct
  type(vector) function vCross(a,b)
    implicit none
    type(vector), intent(in) :: a,b
    vCross%x = a%y*b%z - a%z*b%y
    vCross%y = a%z*b%x - a%x*b%z
    vCross%z = a%x*b%y - a%y*b%x
    return
  end function vCross  
end module vectors
