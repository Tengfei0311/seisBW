#!/bin/bash
#PBS -l nodes=7:ppn=16,walltime=2:00:00
#PBS -N marmousi_project4-1
#PBS -o job_info/output
#PBS -e job_info/error
ulimit -s unlimited

cd $PBS_O_WORKDIR

NUMPROCS=`wc -l <$PBS_NODEFILE`
let NUMPROCS=$NUMPROCS
cat  $PBS_NODEFILE  >  ./job_info/nodes
echo "$PBS_JOBID"   >  ./job_info/id

#########################################################################################
#                                 input parameters                                      #
#########################################################################################
# number of time steps ("nt" in SPECFEM2D)
NSTEP=12000
# number of receivers
NREC=361                              # number of receivers
# number of sources
NSRC=$NUMPROCS                                   # number of sources
scale=9
level=10
NA=35
### kernel type:
#kernel=1, banana doughnut kernel; 
#kernel=2 cross-correlation traveltime;
#kernel=3 waveform difference; 
#kernel=4 envelope difference  (10 iterations saved in Result_ED)
#kernel=7 cross-correlation of envelope traveltime (10 iterations, saved in Result_ET1)
#kernel=8 cross-correlation traveltime weighted by envelope time shift;
#kernel=9 adjoint source as envelope subtraction
# kernel =10 :: waveform difference of cross-correlation 
kernel=3
#misfit catagory
# waveform difference misfit for entire seismograms before preprocessing 
SAVE_misfit_WD_original=0
# waveform difference misfit for entire seismograms after preprocessing
SAVE_misfit_WD_preprocessing=0
# traveltime misfit
SAVE_misfit_TT=0
# waveform difference
SAVE_misfit_WD=0
# envelope difference
SAVE_misfit_ED=0
# envelope traveltime
SAVE_misfit_ET=0
iter_start=0
iter_end=0
reset=0                                           # reset conjugate gradient 
reset_rate=100
CG=1                                              # conjugate gradient
DIR_SCRIPTS=$PBS_O_WORKDIR                        # global directory
LOCAL_DIR="/scratch/yanhuay/project4-1"   # directory on local nodes, where specfem runs
DIR_RESULTS="/tigress/yanhuay/RESULTS/project4-1" # directory on global nodes to save return results from local nodes
rm -rf $DIR_RESULTS
mkdir -p $DIR_RESULTS
mkdir -p $DIR_RESULTS/Result$scale

echo " ---------- Start Marmousi project4 -----------------------------"
echo

cp $DIR_SCRIPTS/bin/DATA/model_initial.dat   $DIR_RESULTS/model_current.dat
cp $DIR_SCRIPTS/bin/DATA/model_target.dat   $DIR_RESULTS/model_target.dat


for kernel in 2 3 4;
do

echo " Forward simulation for target model ...... "
 pbsdsh $DIR_SCRIPTS/TargetForwardSimulation_pbsdsh $NSTEP $scale $level $kernel $NA $NREC $NSRC $DIR_SCRIPTS $DIR_RESULTS $LOCAL_DIR 2> ./job_info/error_target
    echo "finish forward simulation for target model"
    echo

echo "***************************************************************************************"
echo "******************Welcome scale $scale from $iter_start to $iter_end iterations***********"
echo

for (( iter=$iter_start;iter<=$iter_end;iter++ ))
do
echo "***************************************************************************************"
echo "******************Welcome iteration $iter**********************************************"
echo
let reset=`(expr $iter % $reset_rate)`
echo "reset= $reset"
### preparation
cp $DIR_RESULTS/model_current.dat $DIR_RESULTS/Result$scale/model_$iter.dat
echo " model misfit ...... "
   ./bin/model_misfit.exe $DIR_RESULTS 2> ./job_info/error_model_misfit_$iter
cp $DIR_RESULTS/model_misfit.dat $DIR_RESULTS/Result$scale/model_misfit_$iter.dat

echo " Forward/Adjoint simulation for current model ...... "
 pbsdsh $DIR_SCRIPTS/CurrentForwardAdjoint_pbsdsh $NSTEP $scale $level $kernel $NA $NREC $NSRC $DIR_SCRIPTS $DIR_RESULTS $LOCAL_DIR $SAVE_misfit_WD_original $SAVE_misfit_WD_preprocessing $SAVE_misfit_TT $SAVE_misfit_WD $SAVE_misfit_ED $SAVE_misfit_ET 2> ./job_info/error_current_simulation
    echo "finish Forward/Adjoint simulation for current model"
    echo

echo " misfit kernel for current model ...... "
   ./bin/misfit_kernel.exe $DIR_RESULTS 2> ./job_info/error_kernel_$iter
   cp $DIR_RESULTS/kernel_smoothed.dat $DIR_RESULTS/kernel_current.dat
   cp $DIR_RESULTS/kernel_smoothed.dat $DIR_RESULTS/Result$scale/kernel_smoothed$iter.dat
   cp $DIR_RESULTS/kernel_unsmoothed.dat $DIR_RESULTS/Result$scale/kernel_unsmoothed$iter.dat
   echo "finish misfit kernel for current model"
   echo
   cp $DIR_RESULTS/kernel_smoothed.dat     $DIR_RESULTS/misfit_kernel_I1_scale${scale}_kernel${kernel}_P0.dat

echo "******************finish iteration $iter*******************************"
done

echo "******************finish kernel $kernel**************"
done
echo "******************finish all kernels*******************************"

## clean up local nodes
 pbsdsh $DIR_SCRIPTS/Clean_pbsdsh $LOCAL_DIR 2> ./job_info/error_clean 

echo "******************well done*******************************"


