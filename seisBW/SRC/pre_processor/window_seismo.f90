     program window_seismo

! window data

! input parameters:
  character(len=200) :: file_in,file_out

! data parameter
! sample interval
real*4 :: deltat=5.0d-4
real*4 :: t0=0.0d0
INTEGER, PARAMETER :: NSTEP=12000 
! number of receivers
INTEGER, PARAMETER :: NREC=361 
integer :: irec,itime
!! data sets
real*4 :: seism_input(NSTEP,NREC),seism_output(NSTEP,NREC),trace(NSTEP)

!! windowing parameter
INTEGER, PARAMETER :: issurface=1
INTEGER :: B1,B2,B3,B4
integer :: shift_len=500,tail_len=300
! receiver and source x-cooridinate
real*4 :: x_source
real*4 :: x_receiver
real*4 :: dis_sr
! group velocity
real*4 :: Vg_1=1000,Vg_2=780  ! m/s
! norm 
real*4 :: norm_input,norm_output
character(len=10)   :: arg

        ! get input parameters
        j=1; call getarg(j,file_in)
        j=2; call getarg(j,file_out)
        j=3; call getarg(j,arg); read(arg,*) x_source


!! load 2D data
     open(unit=1,file=trim(file_in),status='old',access='direct',recl=4)
     do irec=1,NREC
         do itime=1,NSTEP
             read(1,rec=(irec-1)*NSTEP+itime) seism_input(itime,irec)
         enddo
     enddo
   close(1)


!! process of data
   do irec=1,NREC ! trace loop
     trace(:)=seism_input(:,irec)
    ! receiver location
     x_receiver=(irec-1)*25+100 
! window surface waves (using group velocities)
     if(issurface .eq. 1) then
      dis_sr=abs(x_source-x_receiver)
      B2=max(int((dis_sr/Vg_1+t0)/deltat),1)
      B1=max(B2-tail_len,1)
      B3=min(int((dis_sr/Vg_2+t0)/deltat)+shift_len,NSTEP)
      B4=min(B3+tail_len,NSTEP)
    if(B1<=B2 .and. B2<=B3 .and. B3<=B4) then
! print*,'trace=',irec,'B1=',B1,'B2=',B2,'B3=',B3,'B4=',B4 
    norm_input=sqrt(sum(trace(:)**2))
     call  window(trace,NSTEP,B1,B2,B3,B4)
    norm_output=sqrt(sum(trace(:)**2))
 !   print*,'norm of surface waves / norm of original data is',norm_output/norm_input

    if(B1>1 .and. B4<NSTEP) then ! not touch the boundary
    if(norm_output/norm_input<0.5) then
    print*,'x_source= ',x_source,', receiver ',irec
    print*,'Warning: norm of surface waves / norm of original data is less than 50%: ',&
norm_output,'/',norm_input,'=',norm_output/norm_input
    endif  !  if(norm_output/norm_input<0.8)
    endif  !  if(B1>1 && B4<NSTEP)
    else
   trace(:)=0.0;
    endif  !  if(B1<=B2 && B2<=B3 && B3<=B4)
    endif !  if(issurface .eq. 1)
    seism_output(:,irec)=trace(:)
   enddo


!! write 2D data
    open(unit=3,file=trim(file_out),status='unknown',access='direct',recl=4)
     do irec=1,NREC
         do itime=1,NSTEP
             write(3,rec=(irec-1)*NSTEP+itime) seism_output(itime,irec)
         enddo
     enddo
     close(3)

     end program window_seismo
