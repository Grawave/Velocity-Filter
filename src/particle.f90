module particles
  use vectors
  implicit none
  type :: particle
     integer :: id
     real :: q,m
     type(vector) :: r,v
   contains
     procedure :: move => moveParticle
     procedure :: accelerate => accelerateParticle
     procedure :: printInfo => printParticleInfo
  end type particle

contains

  subroutine printParticleInfo(this)
    implicit none
    class(particle),target :: this
    print '(a,i0,a,E11.4,a,E11.4)', 'id= ',this%id, ', charge = ', this%q, ', mass= ', this%m
    print '(a,E11.4,a,E11.4,a,E11.4)', 'x=  ',this%r%x,', y= ',this%r%y,', z= ', this%r%z
    print '(a,E11.4,a,E11.4,a,E11.4)','vx= ', this%v%x, ', vy= ',this%v%y ,', vz= ', this%v%z
  end subroutine printParticleInfo
  
  subroutine accelerateParticle(this,timestep,acceleration)
    implicit none
    class(particle),target :: this
    real, intent(in) :: timestep
    type(vector), intent(in) :: acceleration
    this%v=this%v + (acceleration * timestep)
  end subroutine accelerateParticle
  
  subroutine moveParticle(this,timestep)
    implicit none
    class(particle),target :: this
    real, intent(in) :: timestep
    this%r=this%r + this%v*timestep
  end subroutine moveParticle
end module particles
