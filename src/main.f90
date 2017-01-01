
program velocityFilter
  use particles
  use vectors
  use cmd_line
  use inputfile_handler
  implicit none
  integer :: n,i,ios, madeItThrough
  type(particle),allocatable :: particleArray(:)
  type(vector) :: elecF,magF
  real :: timestep
  !Dimensions are the same as in the assignment.
  real, parameter :: boxSizeX=0.019,boxSizeY=0.076,boxSizeZ=0.019
  
  !Setup!
  !Here we let the io handler module read data from the file.
  call readFile()
  n = getNumberOfParticles()
  particleArray=getParticles()
  !Reading the crucial values from the command line using cmd_line module.
  timestep = cmd2real(1)
  elecF = vector(cmd2real(2),cmd2real(3),cmd2real(4))
  magF = vector(cmd2real(5),cmd2real(6),cmd2real(7))
  madeItThrough=0

  !output result data goes to scratch and is later transferred to stable file with extra info.
  open(unit=2,status='SCRATCH',action='readwrite')
  write(2,'(9a11)') 'id|','charge|','mass|','vx|','vy|','vz|','x|','y|','z|'
  !this is the stable output file.
  open(unit=3,file='output.txt',status='replace',action='write')
  
  !Here we setup scratch and stable file to store the particle trajectories
  open(unit=4,status='SCRATCH',action='readwrite')
  open(unit=5,file='trajectories.xyz', status='replace',action='write')


  
  !Running the simulation...
  do i=1,n
     call runSimulationForParticle(particleArray(i))
  end do

  !cleaning up
  call saveOutput()
  call closeFiles()

  
contains
  
  subroutine closeFiles()
    implicit none
    CLOSE(2)
    CLOSE(3)
    CLOSE(4)
    CLOSE(5)
  end subroutine closeFiles
  
  subroutine runSimulationForParticle(p)
    implicit none
    type(particle) :: p
    integer :: stepCounter
    type(vector) :: F,a
    stepCounter=0
    do
       !If particle is no longer in the box, the loop terminates.
       if (.NOT. inTheBox(p)) then
          !Check wheter the particle made it through or not.
          if((p%r%y > boxSizeY)) then
             call documentResult(p)
             madeItThrough=madeItThrough + 1
          end if
          call documentStep(p)
          call saveTrajectory(stepCounter/10)
          exit
       end if

       F=force(p,elecF,magF)
       a=acceleration(p,F)
       call p%accelerate(timestep,a)
       call p%move(timestep)

       !We record every 10th step, as not to slow down the program too much.
       !This also keeps the trajectory file reasonably sized.
       if(MOD(stepCounter,10)==0) then
          call documentStep(p)
       end if

       stepCounter = stepCounter + 1
    end do
  end subroutine runSimulationForParticle

  !This subroutine takes the output from the scratch file
  !and puts is to a stable output file, adding how many particles got through.
  subroutine saveOutput()
    implicit none
    integer :: ios,i,frames,j
    character(len=130) :: scratchLine
    rewind(2)
    WRITE(3,*) madeItThrough
    do
       READ(2,'(a)',iostat=ios) scratchLine
       if(ios/=0) then
          exit
       end if   
       WRITE(3,'(a)') scratchLine
    end do
  end subroutine saveOutput

  !This subroutine transfers trajectory info from a scratch file
  !to a stable file, adding the number of frames to the beginning.
  subroutine saveTrajectory(frames)
    implicit none
    integer :: frames,i,j
    character(len=130) :: scratchLine
    !pointer to the beginning of scratchfile for reading.
    rewind(4)
    !how many frames are there
    WRITE(5,'(i0)') frames
    !comment
    WRITE(5,'(a,15x,3(a,16x))'),'#id','x','y','z'
    
    !Reads the scratchfile, copying the info to the stable file.
    do i=1,frames
       READ(4,'(a)',iostat=ios) scratchLine
       if(ios/=0) then
          exit
       end if
       WRITE(5,*) scratchLine
    end do
    
    !closes, and reopens the scratchfile for the next trajectory.
    close(4)
    OPEN(4,status='SCRATCH',action='readwrite')
  end subroutine saveTrajectory

  !Documents the position of a particle after a step, to a trajectory scratch file.
  subroutine documentStep(p)
    implicit none
    type(particle), intent(in) :: p
    character(len=130) :: toBeWritten
    
    WRITE(toBeWritten,'(i0,10x,3(E12.6,5x))') p%id,p%r%x,p%r%y,p%r%z
    WRITE(4,'(a)') toBeWritten
  end subroutine documentStep

  !For all the particles that made it through the box, this
  !subroutine documents it's data to a scratchfile.
  subroutine documentResult(p)
    implicit none
    type(particle), intent(in) :: p
    character(len=130) :: toBeWritten
    WRITE(toBeWritten,'(i10,a,8(E10.4,a))') p%id,'|',p%q,'|',p%m,'|',p%v%x,'|',p%v%y,'|',p%v%z,'|',p%r%x,'|',p%r%y,'|',p%r%z,'|'
    WRITE(2,'(a)') toBeWritten
  end subroutine documentResult

  !Returns true if the particles locations is within the boundary conditions.
  logical function inTheBox(p)
    implicit none
    type(vector), pointer :: r
    type(particle), intent(in),target :: p
    r=> p%r
    if(r%x < 0 .OR. r%x > boxSizeX) then
       inTheBox = .FALSE.
    else if(r%y < 0 .OR. r%y > boxSizeY) then
       inTheBox = .FALSE.
    else if(r%z < 0 .OR. r%z > boxSizeZ) then
       inTheBox = .FALSE.
    else
       inTheBox = .TRUE.
    end if
    nullify(r)
  end function inTheBox

  !Calculates the Lorentz force affecting the particle.
  type(vector) function force(particl,elecF,magF)
    implicit none
    type(vector), intent(in) :: elecF,magF
    type(particle), intent(in) :: particl
    !.X. is the crossproduct operator.
    force=particl%q*(elecF+(particl%v .X. magF))
    return
  end function force

  !Calculates the acceleration affecting the particle.
  type(vector) function acceleration(particl,force)
    implicit none
    type(particle), intent(in) :: particl
    type(vector), intent(in) :: force
    acceleration = force/(particl%m)
  end function acceleration
  
end program velocityFilter
