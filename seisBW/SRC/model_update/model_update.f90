    program model_update
                ! this routine is used to update model from current model in the search direction by conkugate method
                ! How to run:
                ! gfortran model_update.f90 -o model_update
                ! ./model_update
                ! yanhuay@princeton.edu 08-16-2012

implicit none

! parameters
! index used
INTEGER :: i,j
! status of file
INTEGER :: st
integer :: step_back
! number of points in the model
INTEGER, PARAMETER :: NX=120000 
! step length
real :: step_length=0.02 
! initial model    
real*4 :: model(NX,6)
! search direction
real*4 :: direction(NX,2)
! normalized factor
real*4 :: nf
character(len=200) :: filename
CHARACTER(len=10) :: arg
character(len=200) :: directory


              ! input
               j=1; call getarg(j,arg); read(arg,*) step_back
               j=2; call getarg(j,directory)
               
               step_length=step_length/(2**step_back)               

               ! read current model
               filename = ''//trim(directory)//'/model_current.dat'
                 OPEN(UNIT=1,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
              read(1,*) model(i,1),model(i,2),model(i,3),model(i,4),model(i,5),model(i,6)
               end do
               end if
               close(1)
               !   print*,'Successfully read current model from:',filename

              ! read update search_direction
                filename =''//trim(directory)//'/search_direction.dat'
                 OPEN(UNIT=3,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
                   read(3,*) direction(i,1),direction(i,2)
               end do
               end if
               close(3)
              !    print*,'Successfully previous search direction from  ',filename

              ! normalized direction
               nf=max(maxval(abs(direction(:,1))),maxval(abs(direction(:,2))))
               direction(:,1)=direction(:,1)/nf
               direction(:,2)=direction(:,2)/nf
              ! nf=maxval(abs(direction(:,1)))
              ! direction(:,1)=direction(:,1)/nf
              ! nf=maxval(abs(direction(:,2)))
              ! direction(:,2)=direction(:,2)/nf
 
             

              ! update Vp and Vs in direction of (have been normalized direction)
               model(:,5)=model(:,5)*(1+step_length*direction(:,1))
             ! update Vs
               model(:,6)=model(:,6)*(1+step_length*direction(:,2))

                        
               ! write new models
               filename = ''//trim(directory)//'/model_update.dat'
              ! print*,filename
                OPEN(UNIT=3,FILE=filename)
               do i=1,NX
                write(3,*) model(i,1),model(i,2),model(i,3),model(i,4),model(i,5),model(i,6)
               end do
               close(3)
!                  print*,'Successfully write update model into:',filename

               end program model_update


