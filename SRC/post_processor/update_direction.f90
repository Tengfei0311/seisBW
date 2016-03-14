    program update_direction

                ! this routine is used to calculate update direction
                ! yanhuay@princeton.edu 03-14-2013

implicit none

! parameters
! index used
INTEGER :: i,j
! status of file
INTEGER :: st
INTEGER :: maxscale
INTEGER :: iteration
INTEGER :: CG
INTEGER :: reset
! number of points in the model
INTEGER, PARAMETER :: NX=120000 
real :: beta
! store the kernel                
real*4 :: kernel(NX,5)
real*4 :: kernel_current(NX,5)
real*4 :: kernel_pre(NX,5)
real*4 :: sum1,sum2
! search direction
double precision :: direction(NX,2)
! normalized factor
character(len=100) :: filename
character(len=2) :: fscale
character(len=3) :: fiteration
character(len=3) :: fiteration1
CHARACTER(len=10) :: arg
character(len=200) :: directory


              ! input
               j=1; call getarg(j,arg); read(arg,*) maxscale
               j=2; call getarg(j,arg); read(arg,*) iteration
               j=3; call getarg(j,arg); read(arg,*) CG
               j=4; call getarg(j,arg); read(arg,*) reset
               j=5; call getarg(j,directory)

              write(fscale,"(i2)") maxscale
              write(fiteration,"(i3)") iteration
        
             !! define search direction
              if(CG .eq. 0) then  ! steepest descent method
              print*, 'steepest descent method'
              filename =''//trim(directory)//'/Result'//trim(adjustl(fscale))//&
               &'/kernel_smoothed'//trim(adjustl(fiteration))//'.dat'
            !   print*,filename                 
               OPEN(UNIT=2,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
                   read(2,*) kernel(i,1),kernel(i,2),kernel(i,3),kernel(i,4),kernel(i,5)
               end do
               end if
               close(2)
               print*,'Successfully read current kernel from:',filename
               ! use k_\alpha instead
              ! kernel(:,5)=kernel(:,4)/1.732
               direction(:,1)=-1.0*kernel(:,4)
               direction(:,2)=-1.0*kernel(:,5)


             else if(CG .eq. 1) then ! conjugate method          
             print*, 'conjugate gradient method'
              ! read kernel
              if (reset .eq. 0) then ! steepest descent method  
              print*, 'steepest descent for iter= ', iteration                 
               filename =''//trim(directory)//'/Result'//trim(adjustl(fscale))//&
               &'/kernel_smoothed'//trim(adjustl(fiteration))//'.dat'
            !   print*,filename                 
               OPEN(UNIT=2,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
                   read(2,*) kernel(i,1),kernel(i,2),kernel(i,3),kernel(i,4),kernel(i,5)
               end do
               end if
               close(2)
               print*,'Successfully read current kernel from:',filename
              ! kernel(:,5)=kernel(:,4)/1.732
               direction(:,1)=-1.0*kernel(:,4)
               direction(:,2)=-1.0*kernel(:,5)

    
              else ! conjugate gradient method
              print*, 'conjugate gradient method for iter= ', iteration
             ! read current kernel
               filename =''//trim(directory)//'/Result'//trim(adjustl(fscale))//&
               &'/kernel_smoothed'//trim(adjustl(fiteration))//'.dat'
             !  print*,filename              
               OPEN(UNIT=2,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
                   read(2,*) kernel_current(i,1),kernel_current(i,2),kernel_current(i,3),kernel_current(i,4),kernel_current(i,5)
               end do
               end if
               close(2)
               print*,'Successfully read current kernel from:',filename
             ! kernel_current(:,5)=kernel_current(:,4)/1.732

             ! read previous kernel
               write(fiteration1,"(i3)") iteration-1
                   filename =''//trim(directory)//'/Result'//trim(adjustl(fscale))//&
                   &'/kernel_smoothed'//trim(adjustl(fiteration1))//'.dat'
              ! print*,filename                 
                   OPEN(UNIT=2,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
                   read(2,*) kernel_pre(i,1),kernel_pre(i,2),kernel_pre(i,3),kernel_pre(i,4),kernel_pre(i,5)
               end do
               end if
               close(2)
               print*,'Successfully read previous kernel from:',filename
             !  kernel_pre(:,5)=kernel_pre(:,4)/1.732

              ! read previous search_direction
                filename =''//trim(directory)//'/search_direction.dat'
              ! print*,filename
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
              print*,'Successfully previous search direction from  ',filename
               
               ! conjugate gradient
               sum1=sum(kernel_current(:,4)*(kernel_current(:,4)-kernel_pre(:,4)))+&
                   &sum(kernel_current(:,5)*(kernel_current(:,5)-kernel_pre(:,5)))
               sum2=sum(kernel_pre(:,4)*kernel_pre(:,4))+sum(kernel_pre(:,5)*kernel_pre(:,5))
               beta=sum1/sum2
         !      print*,'beta=',beta
               direction(:,1)=-1.0*kernel_current(:,4)+beta*direction(:,1)
               direction(:,2)=-1.0*kernel_current(:,5)+beta*direction(:,2)
         end if
         end if
               ! remember to update search direction file
                filename =''//trim(directory)//'/search_direction.dat'
              ! print*,filename
                OPEN(UNIT=3,FILE=filename)
               do i=1,NX
                write(3,*) direction(i,1),direction(i,2)
               end do
               close(3)
               print*,'Successfully save update search direction into:',filename

               end program update_direction


