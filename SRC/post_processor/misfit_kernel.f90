        program misfit_kernel
        ! This routine is used to calculate misfit kernel(sum up of all event
        ! kernels)
        ! How to run:
        !  rm -rf misfit_kernel
        !  gfortran misfit_kernel.f90 -o misfit_kernel
        !  ./misfit_kernel


! parameters
! sampling
INTEGER, PARAMETER :: NX=120000 
! number of receiver
INTEGER, PARAMETER :: NREC=361 
! number of source
INTEGER, PARAMETER :: NSRC=112 
integer, parameter :: smooth=1

! length of Gaussian function is 2*nlen+1
INTEGER, parameter :: nlen=20
! index used
INTEGER :: itime, st, isrc,i,j

! event kernel, unsmoothed misfit kernel and smoothed misfit kernel
real*4 :: kernel(NX,5), mkernel(NX,5)=0.d0
real*4 :: skernel1(NX)=0.0d0,skernel2(NX)=0.0d0, weight(NX)=0.0d0
! parameters for Gaussian functions
real*4 :: distance=0.0d0, sigma=150 
! sigma is the full-width of Gaussian function (half-wdith determine the smooth
! length which is equal to the smallest heterogeneity that can be resolved)
! filename
character(len=200) :: filename
character(len=6) :: source
character(len=200) :: directory


j=1; call getarg(j,directory)


    ! source loop
    do isrc=1, NSRC
        write(source, "(i6)") isrc-1

filename =''//trim(directory)//'/event_kernel'//trim(adjustl(source))//'.dat'
     OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
     do itime=1,NX
         read(1,*) kernel(itime,1),kernel(itime,2),kernel(itime,3),kernel(itime,4),kernel(itime,5)
         mkernel(itime,1)=kernel(itime,1)
         mkernel(itime,2)=kernel(itime,2)
         mkernel(itime,3)=kernel(itime,3)
     !   if (mkernel(itime,2)<=100) then
     !  mkernel(itime,4)=0.0
     !  mkernel(itime,5)=0.0
     !   else
         mkernel(itime,4)=mkernel(itime,4)+kernel(itime,4)  ! Vp
         mkernel(itime,5)=mkernel(itime,5)+kernel(itime,5)  ! Vs
     !  endif
     end do
     end if
      close(1)
    !  print*,'Successfully read kernel from ',filename
   end do

    !! write unsmoothed misfit kernels
         filename = ''//trim(directory)//'/kernel_unsmoothed.dat'
         OPEN(UNIT=3,FILE=filename)
         do itime=1,NX
         write(3,*)mkernel(itime,1),mkernel(itime,2),mkernel(itime,3),mkernel(itime,4),mkernel(itime,5)
         end do
         close(3)
!         print*,'Successfully write data into file: ', filename

! smooth Vp and Vs kernel
if(smooth .eq. 1) then
   do i=1,NX
!    weight
     weight(:)=0.0
     do j=1,NX
        distance=(mkernel(i,1)-mkernel(j,1))**2+(mkernel(i,2)-mkernel(j,2))**2
        weight(j)=exp(-4*distance/(sigma**2))
     end do
     ! smoothing
     skernel1(i)=sum(mkernel(:,4)*weight(:))/sum(weight(:))
     skernel2(i)=sum(mkernel(:,5)*weight(:))/sum(weight(:))
   end do
       mkernel(:,4)=skernel1(:)
       mkernel(:,5)=skernel2(:)
end if


!    !! write smoothed misfit kernels
         filename = ''//trim(directory)//'/kernel_smoothed.dat'
         OPEN(UNIT=3,FILE=filename)
         do itime=1,NX
         write(3,*) mkernel(itime,1),mkernel(itime,2),mkernel(itime,3),mkernel(itime,4),mkernel(itime,5)
         end do
         close(3)
!        print*,'Successfully write data into file: ', filename
!
         end program misfit_kernel

