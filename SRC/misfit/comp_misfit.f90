        program comp_misfit

        ! This routine is used to compare the misfits for the current and  updated model
        ! How to run:
        !  rm -rf comp_misfit
        !  gfortran comp_misfit.f90 -o comp_misfit
        !  ./comp_misfit
        !
        ! YanhuaY@princeton.edu 07-01-2012

! parameters

INTEGER ::  step_back
INTEGER ::  step_loop
INTEGER ::  Dir
INTEGER ::  st
INTEGER ::  j
real :: misfit_current, misfit_update
character(len=100) :: filename
character(len=10)  :: arg
character(len=200) :: directory
       
       ! input
       j=1; call getarg(j,arg); read(arg,*) Dir
       j=2; call getarg(j,arg); read(arg,*) step_loop
       j=3; call getarg(j,arg); read(arg,*) step_back
       j=4; call getarg(j,directory)


     ! read misfit for the current model
     filename = ''//trim(directory)//'/misfit_current.dat'
     OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_current
     end if
      close(1)

     ! read misfit for the updatedcurrent model
     filename = ''//trim(directory)//'/misfit_update.dat'
     OPEN (UNIT=2,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(2,*) misfit_update
     end if
      close(2)



    ! compare the misfits and decide linear search direction
    ! Dir=0 : stop all iterations
    ! Dir=1 : search backward, not save current results and proceed backward
    ! Dir=2 : search forward, save current results and proceed forward
    ! Dir=3 : stop searching forward for current iteration, save previous results
    ! Dir=4 : stop searching backward for current iteration, save current results  
   ! if(step_back .ge. 4) then
   ! Dir=0
    if(step_back .eq. 0) then
        if(misfit_update<misfit_current) then
             Dir=2
        elseif (step_loop .ne. 0) then
             Dir=3
        elseif (step_loop .eq. 0) then
             Dir=1
        end if
    elseif (step_back .gt. 0) then
        if(misfit_update<misfit_current) then
             Dir=4
        else
             Dir=1
        endif
    end if
  !  print*, 'search direction : ', Dir


     !!save search direction in a file 
     filename = ''//trim(directory)//'/search_direction'
     OPEN (UNIT=5, FILE=filename)
        write(5,*) Dir
   !  print*,'Successfully write search direction into ',filename
     close(5);

 end program comp_misfit

