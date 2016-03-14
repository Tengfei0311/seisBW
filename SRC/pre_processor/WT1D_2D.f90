   program WT1D_2D
! 1D wt partial reconstruction to 2D shot gather

! input parameters:
! maximal scale
  integer :: maxscale
! current level under reconstruction 
  integer :: lev
! number of basis functions
  integer :: NA
  character(len=200) :: file_in,file_out

! sampling points in time
INTEGER, PARAMETER :: NSTEP=12000 
! number of receivers
INTEGER, PARAMETER :: NREC=361 
integer :: irec,itime
integer :: st
!! data sets
real*4 :: seism(NSTEP,NREC)
real*4 :: wt_seism(NSTEP,NREC)=0.d0
real*4 :: basis(NSTEP),trace(NSTEP)
real*4 :: wc
character(len=200) :: fname
character(len=200) :: filename
character(len=10)   :: arg

        ! get input parameters
        j=1; call getarg(j,arg); read(arg,*) maxscale
        j=2; call getarg(j,arg); read(arg,*) lev
        j=3; call getarg(j,arg); read(arg,*) NA
        j=4; call getarg(j,file_in)
        j=5; call getarg(j,file_out)
 
!! load 2D data
     open(unit=1,file=trim(file_in),status='old',access='direct',recl=4)
     do irec=1,NREC
         do itime=1,NSTEP
             read(1,rec=(irec-1)*NSTEP+itime) seism(itime,irec)
         enddo
     enddo
   close(1)

if(maxscale .le. 0  .or. lev .le. 0 .or.  NA .le. 0) then
    wt_seism=seism
else
! wavelet transform
do i=1,NA
    ! load basis functions
   write(fname,"(a,i0,a,i0,a,i0,a,i0)") 'basis_Daubechies6_N',NSTEP,'_maxscale',maxscale,'_iscale',lev,'_',i
   filename='basis/'//trim(fname)//'.dat'
   OPEN (UNIT=20,FILE=filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
     do itime=1,NSTEP
         read(20,*) basis(itime)
     end do
     end if
    close(20)

   do irec=1,NREC ! trace loop
     ! forward wavelet transform to get wt coeff
     trace(:)=seism(:,irec)
     wc=dot_product(trace,basis)

     ! inverse wavelet transform to construct data
     wt_seism(:,irec)=wt_seism(:,irec)+wc*basis(:)
   enddo ! end of trace loop
 enddo
end if
!! write 2D data
    open(unit=3,file=trim(file_out),status='unknown',access='direct',recl=4)
     do irec=1,NREC
         do itime=1,NSTEP
             write(3,rec=(irec-1)*NSTEP+itime) wt_seism(itime,irec)
         enddo
     enddo
     close(3)
end program WT1D_2D


