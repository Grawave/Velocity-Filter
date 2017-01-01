module inputfile_handler
  use vectors
  use particles
  character(len=80), parameter :: inputFile='input.txt',outputFile='output.txt'
  integer :: number
  type(particle),allocatable :: particleArrayIn(:)
  type(vector),parameter ::  startLoc = vector(0.0095,0.00,0.0095)

contains
  
  subroutine readFile()
    integer :: i,ios,id
    type(particle) :: p
    real :: storage(5)
    type(vector) :: velocity
    !Opening the input file
    
    open(unit=1,file=inputFile,iostat=ios,status='old')
    if (ios/=0) then
       print '(a,a)', '*** Error in opening file ', 'input.txt'
       stop
    end if

    !first line contains info about how many particles there are.
    read(1,*,iostat=ios) number
    if (ios/=0) then
       print *,'Did not find the number of particles on the first line of input file'
       stop
    end if
    ALLOCATE(particleArrayIn(number))

     !Now we read lines from the input file. One line, one particle data. We read total of 'n' lines here.
    do i=1,number
       read(1,*,iostat=ios) id,storage(1),storage(2),storage(3),storage(4),storage(5)
       !error handling
       if(ios/=0) then
          print '(a,i0,a,i0)','error reading from input file, iteration ',i,', given number of inputs ', number
          stop
       end if
       !Here we create a particle - p - from the input data, and we add this particle to an array.
       velocity= vector(storage(3),storage(4),storage(5))
       p = particle(id,storage(1),storage(2),startLoc, velocity)
       particleArrayIn(i)=p
    end do
    close(unit=1,status='keep')
  end subroutine readFile
  
  integer function getNumberOfParticles()
    getNumberOfParticles=number
  end function getNumberOfParticles

  function getParticles()
    type(particle) :: getParticles(number)
    getParticles = particleArrayIn
  end function getParticles  
end module inputfile_handler
