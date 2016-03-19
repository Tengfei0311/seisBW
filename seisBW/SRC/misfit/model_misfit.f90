    program model_misfit
! model misfit

implicit none

! parameters
! index used
INTEGER :: i,j
! status of file
INTEGER :: st
! number of points in the model
INTEGER, PARAMETER :: NX=120000 
! models   
real*4 :: model(NX,6),model0(NX,6)
real*4 :: model_misfit_Vp=0.0, model_misfit_Vs=0.0,sum_model_misfit=0.0
character(len=200) :: filename
character(len=200) :: directory


              ! input
               j=1; call getarg(j,directory)
 
               ! read target model
               filename = ''//trim(directory)//'/model_target.dat'
                 OPEN(UNIT=1,FILE=filename,STATUS='old',action='read',iostat=st)
               if(st>0) then
                    print*,'Error opening file. File load status',st
                    stop
               else
               do i=1,NX
              read(1,*) model0(i,1),model0(i,2),model0(i,3),model0(i,4),model0(i,5),model0(i,6)
               end do
               end if
               close(1)

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

              model_misfit_Vp=sum((model(:,5)-model0(:,5))**2)
              model_misfit_Vs=sum((model(:,6)-model0(:,6))**2)
              sum_model_misfit=model_misfit_Vp+model_misfit_Vs

  print*,'model misfit for Vp, Vs, Vp+Vs are:',sqrt(model_misfit_Vp),sqrt(model_misfit_Vs), sqrt(sum_model_misfit)

                    
               ! write new models
               filename = ''//trim(directory)//'/model_misfit.dat'
              ! print*,filename
                OPEN(UNIT=3,FILE=filename)
               write(3,*) sqrt(model_misfit_Vp), sqrt(model_misfit_Vs), sqrt(sum_model_misfit)
               close(3)

               end program model_misfit


