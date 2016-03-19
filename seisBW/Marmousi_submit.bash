#!/bin/bash
#PBS -l nodes=7:ppn=16,walltime=1:00:00
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
iter_start=0
iter_end=1
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

echo " misfit for current model ...... "
   ./bin/data_misfit.exe $DIR_RESULTS 2> ./job_info/error_misfit_current_$iter
if [ "$SAVE_misfit_WD_original" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_WD_original.dat $DIR_RESULTS/misfit_WD_original_current.dat
   cp $DIR_RESULTS/misfit_WD_original_current.dat $DIR_RESULTS/Result$scale/misfit_WD_original_$iter.dat
fi
if [ "$SAVE_misfit_WD_preprocessing" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_WD_preprocessing.dat $DIR_RESULTS/misfit_WD_preprocessing_current.dat
   cp $DIR_RESULTS/misfit_WD_preprocessing_current.dat $DIR_RESULTS/Result$scale/misfit_WD_preprocessing_$iter.dat
fi
if [ "$SAVE_misfit_WD" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_WD.dat $DIR_RESULTS/misfit_WD_current.dat
   cp $DIR_RESULTS/misfit_WD_current.dat $DIR_RESULTS/Result$scale/misfit_WD_$iter.dat
fi
if [ "$SAVE_misfit_TT" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_TT.dat $DIR_RESULTS/misfit_TT_current.dat
   cp $DIR_RESULTS/misfit_TT_current.dat $DIR_RESULTS/Result$scale/misfit_TT_$iter.dat
fi
if [ "$SAVE_misfit_ED" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_ED.dat $DIR_RESULTS/misfit_ED_current.dat
   cp $DIR_RESULTS/misfit_ED_current.dat $DIR_RESULTS/Result$scale/misfit_ED_$iter.dat
fi
if [ "$SAVE_misfit_ET" -eq 1 ]; then
   cp $DIR_RESULTS/sum_misfit_ET.dat $DIR_RESULTS/misfit_ET_current.dat
   cp $DIR_RESULTS/misfit_ET_current.dat $DIR_RESULTS/Result$scale/misfit_ET_$iter.dat
fi
if [ "$kernel" -eq 2 ]; then
   cp $DIR_RESULTS/sum_misfit_TT.dat $DIR_RESULTS/misfit_current.dat
elif [ "$kernel" -eq 3 ]; then
   cp $DIR_RESULTS/sum_misfit_WD.dat $DIR_RESULTS/misfit_current.dat
fi
if [ "$kernel" -eq 4 ]; then
   cp $DIR_RESULTS/sum_misfit_ED.dat $DIR_RESULTS/misfit_current.dat
elif [ "$kernel" -eq 7 ]; then
   cp $DIR_RESULTS/sum_misfit_ET.dat $DIR_RESULTS/misfit_current.dat
fi
   echo "finish misfit calculation for current model"
   echo


echo " misfit kernel for current model ...... "
   ./bin/misfit_kernel.exe $DIR_RESULTS 2> ./job_info/error_kernel_$iter
   cp $DIR_RESULTS/kernel_smoothed.dat $DIR_RESULTS/kernel_current.dat
   cp $DIR_RESULTS/kernel_smoothed.dat $DIR_RESULTS/Result$scale/kernel_smoothed$iter.dat
   cp $DIR_RESULTS/kernel_unsmoothed.dat $DIR_RESULTS/Result$scale/kernel_unsmoothed$iter.dat
   echo "finish misfit kernel for current model"
   echo

echo " ######################  model update ##########################################"
echo " First : decide update direction ......"
    ./bin/update_direction.exe $scale $iter $CG $reset $DIR_RESULTS 2> ./job_info/error_update_direction_$iter
   echo "  obtained update direction .... "
   echo

echo 'line search to update model along the update direction ......'
    # linearsearch_direction=0: stop all iterations without saving
## misfit for current model
    # linearsearch_direction=1: search by halving current step length: backward
    # linearsearch_direction=2: search by doubling current step length: forward
    # linearsearch_direction=3: stop searching for current iteration wth saving
    # linearsearch_direction=4: stop searching for current iteration without saving
  ### 'step_loop': successful iteration steps

  # initialization 
   let linearsearch_direction=2
   let step_loop=0
   let step_back=0


   while [ $linearsearch_direction -ne 3 ]
   do

  echo " update model ......"
   ./bin/model_update.exe $step_back $DIR_RESULTS 2> ./job_info/error_model_update_$iter
   echo "  finish updating model"
   echo


echo " Forward simulation for update model ...... "
 pbsdsh $DIR_SCRIPTS/UpdateForwardSimulation_pbsdsh $NSTEP $scale $level $kernel $NA $NREC $NSRC $DIR_SCRIPTS $DIR_RESULTS $LOCAL_DIR $SAVE_misfit_WD_original $SAVE_misfit_WD_preprocessing $SAVE_misfit_TT $SAVE_misfit_WD $SAVE_misfit_ED $SAVE_misfit_ET 2> ./job_info/error_update_simulation
    echo "finish Forward simulation for update model"
    echo  

echo " misfit for update model ......"
   ./bin/data_misfit.exe $DIR_RESULTS 2> ./job_info/error_misfit_update_$iter
if [ "$kernel" -eq 2 ]; then
   cp $DIR_RESULTS/sum_misfit_TT.dat $DIR_RESULTS/misfit_update.dat
elif [ "$kernel" -eq 3 ]; then
   cp $DIR_RESULTS/sum_misfit_WD.dat $DIR_RESULTS/misfit_update.dat
fi
if [ "$kernel" -eq 4 ]; then
   cp $DIR_RESULTS/sum_misfit_ED.dat $DIR_RESULTS/misfit_update.dat
elif [ "$kernel" -eq 7 ]; then
   cp $DIR_RESULTS/sum_misfit_ET.dat $DIR_RESULTS/misfit_update.dat
fi
   echo "finish misfit calculation for update model"
   echo

echo " compare misfit for current and update ...... "
    ./bin/comp_misfit.exe $linearsearch_direction $step_loop $step_back $DIR_RESULTS  2> ./job_info/error_comp_$iter

     linearsearch_direction=`cat $DIR_RESULTS/search_direction`

  #  if [ $linearsearch_direction -eq 0 ]; then
  #          break
  #  fi

    if [ $linearsearch_direction -eq 1 ]; then
          let step_back=$step_back+1
    fi

    if [ $linearsearch_direction -eq 2 ]; then
            let step_loop=$step_loop+1
            echo " remember to update current model and misfit "
   cp $DIR_RESULTS/misfit_update.dat $DIR_RESULTS/misfit_current.dat
   cp $DIR_RESULTS/model_update.dat $DIR_RESULTS/model_current.dat 
    fi
    if [ $linearsearch_direction -eq 3 ];then
            break
    fi
    if [ $linearsearch_direction -eq 4 ];then
            let step_back=$step_back+1
            let step_loop=$step_loop+1
            echo " remember to update current model and misfit ...... "
               cp $DIR_RESULTS/misfit_update.dat $DIR_RESULTS/misfit_current.dat
               cp $DIR_RESULTS/model_update.dat $DIR_RESULTS/model_current.dat 
            break
    fi

     echo "search direction is $linearsearch_direction, step_loop=$step_loop step_back=$step_back"
     echo 

  done

  #  if [ $linearsearch_direction -eq 0 ]; then
  #          break
  #  fi

echo "******************finish iteration $iter*******************************"
done

echo "******************finish all iterations for scale $scale **************"
#done
echo "******************finish all scales*******************************"

## clean up local nodes
 pbsdsh $DIR_SCRIPTS/Clean_pbsdsh $LOCAL_DIR 2> ./job_info/error_clean 

echo "******************well done*******************************"


