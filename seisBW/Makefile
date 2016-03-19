#!/bin/bash

currentdir=`pwd`
NSTEP=12000                            # number of samples in data
NX=120000                               # number of samples in model
NREC=361                               # number of receivers
NSRC=112                               # number of sources
NC=2                                  # component: P: NC=2; SH NC=1
step_length=0.02
#misfit catagory
# waveform difference misfit for entire seismograms before preprocessing 
SAVE_misfit_WD_original=1
# waveform difference misfit for entire seismograms after preprocessing
SAVE_misfit_WD_preprocessing=1
# traveltime misfit
SAVE_misfit_TT=0
# waveform difference
SAVE_misfit_WD=1
# envelope difference
SAVE_misfit_ED=1
# envelope traveltime
SAVE_misfit_ET=0

SOURCE_DIR="$currentdir/SRC"          # source directory
EXE_DIR="$currentdir/bin"             # exacutable files directory


############################# Fortran source codes ############################################################### 
###-------------------------- preprocessing
cd $SOURCE_DIR/pre_processor
## wavelet transform
   FILE="WT1D_2D.f90"
   sed -e "s#^INTEGER, PARAMETER :: NSTEP=.*#INTEGER, PARAMETER :: NSTEP=$NSTEP #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NREC=.*#INTEGER, PARAMETER :: NREC=$NREC #g"  $FILE > temp;  mv temp $FILE
## window
   FILE="window_seismo.f90"
   sed -e "s#^INTEGER, PARAMETER :: NSTEP=.*#INTEGER, PARAMETER :: NSTEP=$NSTEP #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NREC=.*#INTEGER, PARAMETER :: NREC=$NREC #g"  $FILE > temp;  mv temp $FILE 
## detrend 

## demean 


###-------------------------- adjoint source 
   cd $SOURCE_DIR/adjoint_source 
   FILE="adj_binary.f90"
   sed -e "s#^INTEGER, PARAMETER :: NX=.*#INTEGER, PARAMETER :: NX=$NX #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NSTEP=.*#INTEGER, PARAMETER :: NSTEP=$NSTEP #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NC=.*#INTEGER, PARAMETER :: NC=$NC #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NREC=.*#INTEGER, PARAMETER :: NREC=$NREC #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NSRC=.*#INTEGER, PARAMETER :: NSRC=$NSRC #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD_original=.*#INTEGER, PARAMETER :: SAVE_misfit_WD_original=$SAVE_misfit_WD_original #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=.*#INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=$SAVE_misfit_WD_preprocessing #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_TT=.*#INTEGER, PARAMETER :: SAVE_misfit_TT=$SAVE_misfit_TT #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD=.*#INTEGER, PARAMETER :: SAVE_misfit_WD=$SAVE_misfit_WD #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_ED=.*#INTEGER, PARAMETER :: SAVE_misfit_ED=$SAVE_misfit_ED #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_ET=.*#INTEGER, PARAMETER :: SAVE_misfit_ET=$SAVE_misfit_ET #g"  $FILE > temp;  mv temp $FILE

###--------------------------  misfit
cd $SOURCE_DIR/misfit
## data misfit
   FILE="data_misfit.f90"
   sed -e "s#^INTEGER, PARAMETER :: NSRC=.*#INTEGER, PARAMETER :: NSRC=$NSRC #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD_original=.*#INTEGER, PARAMETER :: SAVE_misfit_WD_original=$SAVE_misfit_WD_original #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=.*#INTEGER, PARAMETER :: SAVE_misfit_WD_preprocessing=$SAVE_misfit_WD_preprocessing #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_TT=.*#INTEGER, PARAMETER :: SAVE_misfit_TT=$SAVE_misfit_TT #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_WD=.*#INTEGER, PARAMETER :: SAVE_misfit_WD=$SAVE_misfit_WD #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_ED=.*#INTEGER, PARAMETER :: SAVE_misfit_ED=$SAVE_misfit_ED #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: SAVE_misfit_ET=.*#INTEGER, PARAMETER :: SAVE_misfit_ET=$SAVE_misfit_ET #g"  $FILE > temp;  mv temp $FILE
## model misfit
   FILE="model_misfit.f90"
   sed -e "s#^INTEGER, PARAMETER :: NX=.*#INTEGER, PARAMETER :: NX=$NX #g"  $FILE > temp;  mv temp $FILE
## compare misfit
   FILE="comp_misfit.f90"

###------------------------- postprocessing
cd $SOURCE_DIR/post_processor
#  misfit_kernel
   FILE="misfit_kernel.f90"
   sed -e "s#^INTEGER, PARAMETER :: NX=.*#INTEGER, PARAMETER :: NX=$NX #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NREC=.*#INTEGER, PARAMETER :: NREC=$NREC #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^INTEGER, PARAMETER :: NSRC=.*#INTEGER, PARAMETER :: NSRC=$NSRC #g"  $FILE > temp;  mv temp $FILE
# update direction
    FILE="update_direction.f90"
   sed -e "s#^INTEGER, PARAMETER :: NX=.*#INTEGER, PARAMETER :: NX=$NX #g"  $FILE > temp;  mv temp $FILE


###------------------------- model update
cd $SOURCE_DIR/model_update 
   FILE="model_update.f90"
   sed -e "s#^INTEGER, PARAMETER :: NX=.*#INTEGER, PARAMETER :: NX=$NX #g"  $FILE > temp;  mv temp $FILE
   sed -e "s#^real :: step_length=.*#real :: step_length=$step_length #g"  $FILE > temp;  mv temp $FILE

cd $currentdir
########################################  Bash scripts ###########################################################
 FILE="Marmousi_submit.bash"


