   program adj_binary
! parameters
! input parameters:
! kernel tyoe:
!kernel=1, banana doughnut kernel; 
!kernel=2 cross-correlation traveltime;
!kernel=3 waveform difference;
!kernel=4 envelope difference 
!kernel=7 cross-correlation of envelope traveltime
!kernel=8 cross-correlation traveltime weighted by envelope time shift;
!kernel=9 adjoint source as envelope subtraction
! kernel =10 :: waveform difference of cross-correlation
  INTEGER :: kernel
! write adjoint source or not?
  integer :: is_adjoint

!! data parameter
! sampling points in time
INTEGER, PARAMETER :: NSTEP=12000 
! sample interval
real*4 :: deltat=5.0d-4
real*4 :: t0=0.0d0
! number of dimension ( x y z)
INTEGER, PARAMETER :: ND=3
! P_SV COMPONENTS (1) OR SH WAVES (0)
INTEGER, PARAMETER :: P_SV=1 
! number of components: 2 if PSV waves; 1 if SH wave
INTEGER, PARAMETER :: NC=2 
! number of receivers
INTEGER, PARAMETER :: NREC=361 
! number of sources
INTEGER, PARAMETER :: NSRC=112 
! which receiver ? (valid if only one source-receiver kernel is
! considered)
INTEGER, PARAMETER :: ireceiver=50
! indexes used:
!  INTEGER :: isrc
  INTEGER :: irec
  INTEGER :: itime
  INTEGER :: icomp
  INTEGER :: j

!! misfit catagory
! waveform difference misfit for entire seismograms before preprocessing 
real*4 ::   misfit_WD_original=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD_original=1 
! waveform difference misfit for entire seismograms after preprocessing
real*4 ::   misfit_WD_preprocessing=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=1 
! traveltime misfit
real*4 ::   misfit_TT=0.d0
INTEGER, PARAMETER :: SAVE_misfit_TT=0 
! waveform difference
real*4 ::   misfit_WD=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD=1 
! envelope difference
real*4 ::   misfit_ED=0.d0
INTEGER, PARAMETER :: SAVE_misfit_ED=1 
! envelope traveltime
real*4 ::   misfit_ET=0.d0
INTEGER, PARAMETER :: SAVE_misfit_ET=0 

!! adjoint estimation
! maximum cc
real*4 :: cc_max
! corresponding shift
integer :: ishift
! time lag related to maximum cross-correlation
  real*4 :: MT=0.
! constant factor
  real*4 :: Mtr=0.
! cross-correlation length
  INTEGER, parameter :: Mlen=500

!! For thresholding or regularization
! sum of absolute values as a criterion 
  real*4 ::   Sr=0.
! threshold
  double precision, parameter :: eps=1.d-10   ! eps
! regularization factor
  real*4 :: epslon

!! data sets
 real*4 :: seism_obs(NSTEP,3),seism_syn(NSTEP,3)
 real*4 :: seism_comp(NSTEP)
 real*4 :: adjoint(NSTEP),seism_vel(NSTEP),seism_acc(NSTEP)
! for hilbert transformation
  real*4 :: E_obs(NSTEP),E_syn(NSTEP)
  real*4 :: E_ratio(NSTEP)=0.d0,hilbt(NSTEP)=0.d0
  complex ( kind = 4 )  c(NSTEP)
  integer ( kind = 4 ) ier
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) lenc
  integer ( kind = 4 ) lensav
  integer ( kind = 4 ) lenwrk
  real ( kind = 4 ), allocatable, dimension ( : ) :: work
  real ( kind = 4 ), allocatable, dimension ( : ) :: wsave

!! cross-corretation and convolution
integer, parameter :: cc_length=2*NSTEP-1
integer, parameter :: conv_length=NSTEP
real :: conv(conv_length)=0.0
real :: cc_obs(cc_length)=0.0,cc_syn(cc_length)=0.0


  character(len=3) ::   comp_psv(2),comp_sh(1),comp(3)
  character(len=200) :: filename
  character(len=5)   :: seisname
!  character(len=6)   :: source
  character(len=10)   :: arg
        ! get input parameters
        j=1; call getarg(j,arg); read(arg,*) kernel
        j=2; call getarg(j,arg); read(arg,*) is_adjoint

        comp_psv = (/"BXX","BXZ"/)     ! P-SV
        comp_sh = (/"BXY"/)            ! SH
        comp = (/"BXX","BXY","BXZ"/)   ! 3 components


!! entire seismograms before preprocessing
if (SAVE_misfit_WD_original==1) then
misfit_WD_original=0.d0
if(NC .eq. 2) then
!! read obs
     open(unit=1,file='DATA_obs/Ux_file_single.bin',status='old',access='direct',recl=4)
     open(unit=2,file='DATA_obs/Uz_file_single.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Ux_file_single.bin',status='old',access='direct',recl=4)
     open(unit=4,file='DATA_syn/Uz_file_single.bin',status='old',access='direct',recl=4)
do irec=1,NREC
    do itime=1,NSTEP
    read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,1)
    read(2,rec=(irec-1)*NSTEP+itime) seism_obs(itime,2)
    read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,1)
    read(4,rec=(irec-1)*NSTEP+itime) seism_syn(itime,2)
  enddo
  do icomp=1,NC
     misfit_WD_original=misfit_WD_original+sum((seism_syn(:,icomp)-seism_obs(:,icomp))**2)
  enddo
end do
  close(1)
  close(2)
  close(3)
  close(4)
elseif(NC .eq. 1) then
!! read obs
     open(unit=1,file='DATA_obs/Uy_file_single.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Uy_file_single.bin',status='old',access='direct',recl=4)
do irec=1,NREC
    do itime=1,NSTEP
    read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,1)
    read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,1)
  enddo
    misfit_WD_original=misfit_WD_original+sum((seism_syn(:,1)-seism_obs(:,1))**2)
end do
  close(1)
  close(3)
endif

    !! SAVE_misfit_WD_original
     filename = 'misfit_WD_original.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_WD_original
     close(5)
endif

if(SAVE_misfit_WD_preprocessing==1) then
!! entire seismograms after preprocessing
misfit_WD_preprocessing=0.d0
if(NC .eq. 2) then
!! read obs
     open(unit=1,file='DATA_obs/Ux_file_single_processed.bin',status='old',access='direct',recl=4)
     open(unit=2,file='DATA_obs/Uz_file_single_processed.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Ux_file_single_processed.bin',status='old',access='direct',recl=4)
     open(unit=4,file='DATA_syn/Uz_file_single_processed.bin',status='old',access='direct',recl=4)
do irec=1,NREC
    do itime=1,NSTEP
    read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,1)
    read(2,rec=(irec-1)*NSTEP+itime) seism_obs(itime,2)
    read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,1)
    read(4,rec=(irec-1)*NSTEP+itime) seism_syn(itime,2)
  enddo
  do icomp=1,NC
     misfit_WD_preprocessing=misfit_WD_preprocessing+sum((seism_syn(:,icomp)-seism_obs(:,icomp))**2)
  enddo
end do
  close(1)
  close(2)
  close(3)
  close(4)
elseif(NC .eq. 1) then
!! read obs
     open(unit=1,file='DATA_obs/Uy_file_single_processed.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Uy_file_single_processed.bin',status='old',access='direct',recl=4)
do irec=1,NREC
    do itime=1,NSTEP
    read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,1)
    read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,1)
  enddo
    misfit_WD_preprocessing=misfit_WD_preprocessing+sum((seism_syn(:,1)-seism_obs(:,1))**2)
end do
  close(1)
  close(3)
endif
    !! save entire misfit
     filename = 'misfit_WD_preprocessing.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_WD_preprocessing
     close(5)
endif

!! load processed data files
if(NC .eq. 2) then
!! read obs
     open(unit=1,file='DATA_obs/Ux_file_single_processed.bin',status='old',access='direct',recl=4)
     open(unit=2,file='DATA_obs/Uz_file_single_processed.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Ux_file_single_processed.bin',status='old',access='direct',recl=4)
     open(unit=4,file='DATA_syn/Uz_file_single_processed.bin',status='old',access='direct',recl=4)
elseif(NC .eq. 1) then
!! read obs
     open(unit=1,file='DATA_obs/Uy_file_single_processed.bin',status='old',access='direct',recl=4)
!! read syn
     open(unit=3,file='DATA_syn/Uy_file_single_processed.bin',status='old',access='direct',recl=4)
endif

!! adjoint source and misfit
   ! station loop
    do irec=1, nrec
        write(seisname,"(a,i4.4)") 'S',irec

! If use P-SV components
  if(NC .eq. 2) then
   ! observation
     do itime=1,NSTEP
         read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,1)
         read(2,rec=(irec-1)*NSTEP+itime) seism_obs(itime,3)
  ! synthetics
        read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,1)
        read(4,rec=(irec-1)*NSTEP+itime) seism_syn(itime,3)
    end do
    seism_obs(:,2) = 0.d0
    seism_syn(:,2) = 0.d0

   ! If use SH component 
   else if(NC .eq. 1) then
   ! observation
     do itime=1,NSTEP
         read(1,rec=(irec-1)*NSTEP+itime) seism_obs(itime,2)
     ! synthetics
       read(3,rec=(irec-1)*NSTEP+itime) seism_syn(itime,2)
    end do
    seism_obs(:,1) = 0.d0
    seism_obs(:,3) = 0.d0
    seism_syn(:,1) = 0.d0
    seism_syn(:,3) = 0.d0
   end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! adjoint source calculation !!!!!!!!!!!!!!!!!!!!!!
  do icomp = 1, ND
! pre-processing
    adjoint(:)=0.d0
    seism_comp(:) = seism_obs(:,icomp)
    Sr=0.d0
    Sr=sum(abs(seism_comp),1)
   ! if not a zero trace
    if(Sr>eps) then

!!!!!!!!!!!!!!!!!!!!!!! Choice of misfit !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if(kernel .eq. 1) then
!      print*,'sensitivity kernel' 
     seism_comp(:) = seism_syn(:,icomp)
     ! compute velocity
     do itime=2,NSTEP-1
        seism_vel(itime)=(seism_comp(itime+1)-seism_comp(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_vel(1)=(seism_comp(2)-seism_comp(1))/deltat
        seism_vel(NSTEP)=(seism_comp(NSTEP)-seism_comp(NSTEP-1))/deltat
     ! compute acceleration
     do itime=2,NSTEP-1
        seism_acc(itime)=(seism_vel(itime+1)-seism_vel(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_acc(1)=(seism_vel(2)-seism_vel(1))/deltat
        seism_acc(NSTEP)=(seism_vel(NSTEP)-seism_vel(NSTEP-1))/deltat
     ! constant factor
        Mtr=sum(seism_comp*seism_acc)*deltat
 !       print*,'MTR =', Mtr

       adjoint(:)=seism_vel(:)/Mtr
       ! normalized by the power of syn
   !    nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
   !    adjoint=adjoint*nf
   !      print*,nf

    elseif(kernel .eq. 2) then
!     print*,'cross correlation kernel'
     seism_comp(:) = seism_syn(:,icomp)
     ! compute velocity
     do itime=2,NSTEP-1
        seism_vel(itime)=(seism_comp(itime+1)-seism_comp(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_vel(1)=(seism_comp(2)-seism_comp(1))/deltat
        seism_vel(NSTEP)=(seism_comp(NSTEP)-seism_comp(NSTEP-1))/deltat
     ! compute acceleration
     do itime=2,NSTEP-1
        seism_acc(itime)=(seism_vel(itime+1)-seism_vel(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_acc(1)=(seism_vel(2)-seism_vel(1))/deltat
        seism_acc(NSTEP)=(seism_vel(NSTEP)-seism_vel(NSTEP-1))/deltat
     ! constant factor
        Mtr=sum(seism_comp*seism_acc)*deltat
!        print*,'MTR =', Mtr

   ! traveltime shift between syn and obs
call xcorr_calc(seism_syn(:,icomp),seism_obs(:,icomp),NSTEP,1,NSTEP,ishift,cc_max)
     MT=ishift*deltat
     misfit_TT=misfit_TT+MT**2
! waveform difference misfit
     misfit_WD=misfit_WD+sum((seism_syn(:,icomp)-seism_obs(:,icomp))**2)

    ! adjoint source   
       adjoint(:)=MT*seism_vel(:)/Mtr
       ! normalized by the power of syn
      ! nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
      ! adjoint=adjoint*nf

     elseif(kernel .eq. 3) then   ! waveform inversion
       ! adjoint source
       adjoint(:)=seism_syn(:,icomp)-seism_obs(:,icomp)
       ! waveform difference misfit
       misfit_WD=misfit_WD+sum((seism_syn(:,icomp)-seism_obs(:,icomp))**2)
   ! traveltime shift between syn and obs
call xcorr_calc(seism_syn(:,icomp),seism_obs(:,icomp),NSTEP,1,NSTEP,ishift,cc_max)
     MT=ishift*deltat
     misfit_TT=misfit_TT+MT**2


     elseif(kernel .eq. 4) then ! envelope difference
!    print*,'envelope difference kernel'
!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
  lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP

! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)

! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)
  hilbt=imag(c)
  epslon=0.05*maxval(abs(E_syn),1)
!  print*, 'epslon=',epslon

! E_ratio
  E_ratio=(E_obs-E_syn)/(E_syn+epslon)

  ! hilbert transform for E_ratio*hilbt
     c(:)=E_ratio(:)*hilbt(:)
     call hilbert(NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier)
     hilbt=imag(c)
! adjoint source
  adjoint(:)=-E_ratio(:)*seism_syn(:,icomp)+hilbt(:)
       ! normalized by the power of syn
     !  nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
     !  adjoint=adjoint*nf

! envelope difference misfit
     misfit_ED=misfit_ED+sum((E_obs(:)-E_syn(:))**2)

! envelope traveltime misfit
call xcorr_calc(E_syn,E_obs,NSTEP,1,NSTEP,ishift,cc_max)
MT=ishift*deltat
    misfit_ET=misfit_ET+MT**2

! release 
  deallocate ( work )
  deallocate ( wsave )

    elseif(kernel .eq. 5) then ! logarithmic envelope ratio
!    print*,' logarithmic envelope ratio'
!!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
  lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP
!
!! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)
!
! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)
  hilbt=imag(c)
  epslon=0.05*maxval(abs(E_syn),1)

! E_ratio
  E_ratio=(log(E_obs+epslon)-log(E_syn+epslon))/((E_syn+epslon)**2)
!
  ! hilbert transform for E_ratio*hilbt
     c(:)=E_ratio(:)*hilbt(:)
     call hilbert(NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier)
     hilbt=imag(c)


!! adjoint source
  adjoint(:)=-E_ratio(:)*seism_syn(:,icomp)+hilbt(:)
!! normalized by the power of syn
    !   nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
    !   adjoint=adjoint*nf
!       print*,nf
!
!! release
  deallocate ( work )
  deallocate ( wsave )

     elseif(kernel .eq. 6) then ! relative envelope difference
!    print*,'relative envelope difference kernel'
!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
 lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP
! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)

!! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)
  hilbt=imag(c)
  epslon=0.05*maxval(abs(E_syn),1)

! E_ratio
  E_ratio=E_obs*(E_obs-E_syn)/((E_syn+epslon)**4)

! hilbert transform for E_ratio*hilbt
     c(:)=E_ratio(:)*hilbt(:)
     call hilbert(NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier)
     hilbt=imag(c)

! adjoint source
  adjoint(:)=-E_ratio(:)*seism_syn(:,icomp)+hilbt(:)
! normalized by the power of syn
    !   nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
!    !   adjoint=adjoint*nf
!!       print*,nf
!
!! release 
  deallocate ( work )
  deallocate ( wsave )

    elseif(kernel .eq. 7) then 
!    print*,'cross-correlation traveltime of envelopes'
!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
  lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP
! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)

! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)
  hilbt=imag(c)
  epslon=0.05*maxval(abs(E_syn),1)
!  print*, 'epslon=',epslon


! to get the normalized factor Mtr
     seism_comp = E_syn
     ! compute velocity
     do itime=2,NSTEP-1
        seism_vel(itime)=(seism_comp(itime+1)-seism_comp(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_vel(1)=(seism_comp(2)-seism_comp(1))/deltat
        seism_vel(NSTEP)=(seism_comp(NSTEP)-seism_comp(NSTEP-1))/deltat
     ! compute acceleration
     do itime=2,NSTEP-1
        seism_acc(itime)=(seism_vel(itime+1)-seism_vel(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_acc(1)=(seism_vel(2)-seism_vel(1))/deltat
        seism_acc(NSTEP)=(seism_vel(NSTEP)-seism_vel(NSTEP-1))/deltat
      ! constant factor
       Mtr=sum(seism_comp*seism_acc)*deltat
!       print*,'MTR =', Mtr

! cross-correlation of obs and syn envelopes
call xcorr_calc(E_syn,E_obs,NSTEP,1,NSTEP,ishift,cc_max)
MT=ishift*deltat

! E_ratio
  E_ratio=-MT/Mtr*seism_vel/(E_syn+epslon)

  ! hilbert transform for E_ratio*hilbt
     c(:)=E_ratio(:)*hilbt(:)
     call hilbert(NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier)
     hilbt=imag(c)
! adjoint source
  adjoint(:)=-E_ratio(:)*seism_syn(:,icomp)+hilbt(:)
! normalized by the power of syn
     !  nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
     !  adjoint=adjoint*nf
!       print*,nf
! envelope traveltime misfit
     misfit_ET=misfit_ET+MT**2
! envelope difference misfit
     misfit_ED=misfit_ED+sum((E_obs(:)-E_syn(:))**2)


! release 
  deallocate ( work )
  deallocate ( wsave )

    elseif(kernel .eq. 8) then 
!    print*,'cross-correlation traveltime weighted by envelopes shift'
!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
  lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP
! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)

! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)
  hilbt=imag(c)

! to get the normalized factor Mtr
     seism_comp(:) = seism_syn(:,icomp)
     ! compute velocity
     do itime=2,NSTEP-1
        seism_vel(itime)=(seism_comp(itime+1)-seism_comp(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_vel(1)=(seism_comp(2)-seism_comp(1))/deltat
        seism_vel(NSTEP)=(seism_comp(NSTEP)-seism_comp(NSTEP-1))/deltat
     ! compute acceleration
     do itime=2,NSTEP-1
        seism_acc(itime)=(seism_vel(itime+1)-seism_vel(itime-1))/(2*deltat)
     enddo
     ! boundaries
        seism_acc(1)=(seism_vel(2)-seism_vel(1))/deltat
        seism_acc(NSTEP)=(seism_vel(NSTEP)-seism_vel(NSTEP-1))/deltat
     ! constant factor
         Mtr=sum(seism_comp*seism_acc)*deltat
!      print*,'MTR =', Mtr

! cross-correlation of obs and syn envelopes
call xcorr_calc(E_syn,E_obs,NSTEP,1,NSTEP,ishift,cc_max)
     MT=ishift*deltat

! adjoint source
    adjoint(:)=MT*seism_vel(:)/Mtr
! release 
  deallocate ( work )
  deallocate ( wsave )


    elseif(kernel .eq. 9) then 
!    print*,'evelope subtraction as adjoint source'
!  Allocate the work arrays.
  lenwrk = 2 * NSTEP
  lensav = 2 * NSTEP + int ( log ( real ( NSTEP, kind = 4 ) ) / log (2.0E+00 ) ) + 4
  allocate ( work(1:lenwrk) )
  allocate ( wsave(1:lensav) )
  inc = 1
  lenc = NSTEP
! hilbert for obs
  c(:)=seism_obs(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_obs=cabs(c)

! hilbert for syn
  c(:)=seism_syn(:,icomp)
  call hilbert ( NSTEP, inc, c, lenc, wsave, lensav, work, lenwrk, ier )
  E_syn=cabs(c)

! adjoint source
    adjoint(:)=E_syn(:)-E_obs(:)
       ! normalized by the power of syn
  !     nf=sqrt(sum(seism_syn(:,icomp)**2)/sum(adjoint**2))
  !     adjoint=adjoint*nf
!       print*,nf

! release 
  deallocate ( work )
  deallocate ( wsave )
    elseif(kernel .eq. 10) then
!print* 'cross correlation difference '
! calculate cross-correlation time series
call xcorr(seism_obs(:,icomp),seism_obs(:,icomp),NSTEP,NSTEP,cc_obs,ishift,cc_max)
call xcorr(seism_syn(:,icomp),seism_obs(:,icomp),NSTEP,NSTEP,cc_syn,ishift,cc_max)
! convolution with seism_obs
call xconv(seism_obs(:,icomp),cc_syn-cc_obs,NSTEP,cc_length,conv,conv_length)
! normalization
!nf=sqrt(sum(seism_syn(:,icomp)**2))/sqrt(sum(conv(:)**2))
!adjoint(:)=conv(:)*nf

    endif  ! if kernel

  endif  !! if of  if(Sr>eps) then

     !! write adjoint source into SEM
   if (is_adjoint .eq. 1) then
    filename='SEM/'//trim(seisname)//'.AA.'//comp(icomp) //'.adj'
     OPEN (UNIT=5, FILE=filename)
     do itime=1,NSTEP
        write(5,*) (itime-1)*deltat-t0, adjoint(itime)
     end do
     close(5);
   end if
     end do  ! end loop of components
     ! end loop of recievers 
     end do


!! close all open files
if(NC .eq. 2) then
        close(1)
        close(2)
        close(3)
        close(4)
elseif(NC .eq. 1) then
        close(1)
        close(3)
endif

     !!save waveform difference misfit 
     if (SAVE_misfit_WD==1) then
     filename = 'misfit_WD.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_WD
     close(5)
     endif
     !!save traveltime misfit
     if (SAVE_misfit_TT==1) then
     filename = 'misfit_TT.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_TT
     close(5)
     endif

     !!save envelope difference misfit 
     if (SAVE_misfit_ED==1) then
     filename = 'misfit_ED.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_ED
     close(5)
     endif
     !!save envelope traveltime misfit
     if (SAVE_misfit_ET==1) then
     filename = 'misfit_ET.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) misfit_ET
     close(5)
     endif

end program adj_binary

