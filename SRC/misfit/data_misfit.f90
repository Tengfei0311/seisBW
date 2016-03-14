               program data_misfit
! sum of all event misfits

! parameters
! number of sources
INTEGER, PARAMETER :: NSRC=112 

! indexes used:
INTEGER :: isrc
INTEGER :: j
INTEGER :: st


!! misfit catagory
! waveform difference misfit for seismograms before preprocessing 
real*4 ::   misfit_WD_original=0.d0
real*4 ::   sum_misfit_WD_original=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD_original=1 
! waveform difference misfit for seismograms after preprocessing
real*4 ::   misfit_WD_preprocessing=0.d0
real*4 ::   sum_misfit_WD_preprocessing=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=1 
! traveltime misfit
real*4 ::   misfit_TT=0.d0
real*4 ::   sum_misfit_TT=0.d0
INTEGER, PARAMETER :: SAVE_misfit_TT=0 
! waveform difference
real*4 ::   misfit_WD=0.d0
real*4 ::   sum_misfit_WD=0.d0
INTEGER, PARAMETER :: SAVE_misfit_WD=1 
! envelope difference
real*4 ::   misfit_ED=0.d0
real*4 ::   sum_misfit_ED=0.d0
INTEGER, PARAMETER :: SAVE_misfit_ED=1 
! envelope traveltime
real*4 ::   misfit_ET=0.d0
real*4 ::   sum_misfit_ET=0.d0
INTEGER, PARAMETER :: SAVE_misfit_ET=0 

character(len=200) :: filename
character(len=6)   :: source
character(len=200) :: directory


j=1; call getarg(j,directory)

!!!! source loop
    do isrc=1, nsrc
       write(source, "(i6)") isrc-1

! waveform difference misfit for seismograms before preprocessing
if(SAVE_misfit_WD_original==1) then
 filename &
=''//trim(directory)//'/misfit_WD_original'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_WD_original
     end if
      close(1)
   sum_misfit_WD_original=sum_misfit_WD_original+misfit_WD_original
endif


! waveform difference misfit for seismograms after preprocessing
if(SAVE_misfit_WD_preprocessing==1) then
 filename & 
= ''//trim(directory)//'/misfit_WD_preprocessing'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_WD_preprocessing
     end if
      close(1)
   sum_misfit_WD_preprocessing=sum_misfit_WD_preprocessing+misfit_WD_preprocessing
endif

! waveform difference misfit
if(SAVE_misfit_WD==1) then
 filename &
=''//trim(directory)//'/misfit_WD'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_WD
     end if
      close(1)
   sum_misfit_WD=sum_misfit_WD+misfit_WD
endif

! Traveltime misfit
if(SAVE_misfit_TT==1) then
 filename &
=''//trim(directory)//'/misfit_TT'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_TT
     end if
      close(1)
   sum_misfit_TT=sum_misfit_TT+misfit_TT
endif

! envelope difference misfit
if(SAVE_misfit_ED==1) then
 filename &
=''//trim(directory)//'/misfit_ED'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_ED
     end if
      close(1)
   sum_misfit_ED=sum_misfit_ED+misfit_ED
endif

! Traveltime misfit
if(SAVE_misfit_ET==1) then
 filename &
=''//trim(directory)//'/misfit_ET'//trim(adjustl(source))//'.dat'
    OPEN (UNIT=1,FILE= filename,STATUS='OLD',action='read',iostat=st)
     if(st>0) then
        print*,'Error opening file. File load status:', st
        stop
     else
         read(1,*) misfit_ET
     end if
      close(1)
   sum_misfit_ET=sum_misfit_ET+misfit_ET
endif

! end source loop
 end do

     !!save misfit 
    !! SAVE_misfit_WD_original
    if(SAVE_misfit_WD_original==1) then
     filename =''//trim(directory)//'/sum_misfit_WD_original.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_WD_original)
     close(5)
     print*,'sum_misfit_WD_original is', sqrt(sum_misfit_WD_original)
  endif

   if(SAVE_misfit_WD_preprocessing==1) then
     filename = ''//trim(directory)//'/sum_misfit_WD_preprocessing.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_WD_preprocessing)
     close(5)
         print*,'sum_misfit_WD_preprocessing is', sqrt(sum_misfit_WD_preprocessing)
    endif

     !!save waveform difference misfit 
     if (SAVE_misfit_WD==1) then
     filename = ''//trim(directory)//'/sum_misfit_WD.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_WD)
     close(5)
     print*,'sum_misfit_WD is', sqrt(sum_misfit_WD)
     endif
     !!save traveltime misfit
     if (SAVE_misfit_TT==1) then
     filename = ''//trim(directory)//'/sum_misfit_TT.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_TT)
     close(5)
    print*,'sum_misfit_TT is ', sqrt(sum_misfit_TT)
     endif

     !!save envelope difference misfit 
     if (SAVE_misfit_ED==1) then
     filename = ''//trim(directory)//'/sum_misfit_ED.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_ED)
     close(5)
    print*,'sum_misfit_ED is ',sqrt(sum_misfit_ED)
     endif
     !!save envelope traveltime misfit
     if (SAVE_misfit_ET==1) then
     filename = ''//trim(directory)//'/sum_misfit_ET.dat'
     OPEN (UNIT=5, FILE=filename)
     write(5,*) sqrt(sum_misfit_ET)
     close(5)
     print*, 'sum_misfit_ET is ', sqrt(sum_misfit_ET)
     endif

end program data_misfit



