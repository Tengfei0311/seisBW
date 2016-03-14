#!/bin/bash
# surface wave removal
nt=$1
scale=$2
level=$3
NA=$4 

#### parameters for dipfiltering
slope1=-0.0011
slope2=-0.0009
slope3=-0.0008
slope4=0.0000
slope5=0.0007
slope6=0.0009
slope7=0.0011
amp1=0.0
amp2=0.0
amp3=1.0
amp4=1.0
amp5=1.0
amp6=0.0
amp7=0.0
bias_neg=-0.00085
bias_pos=0.00080

######################## X comp ###################################################################################
input_file=OUTPUT_FILES/Ux_file_single.bin
output_file1=OUTPUT_FILES/Ux_file_single_processed.bin
output_file2=OUTPUT_FILES/Ux_file_single_WT.bin

cp $input_file $output_file1
  ## wavelet transform
  ./WT.exe $scale $level $NA $output_file1 $output_file2

######################## Z comp ##################################################################################
input_file=OUTPUT_FILES/Uz_file_single.bin
output_file1=OUTPUT_FILES/Uz_file_single_processed.bin
output_file2=OUTPUT_FILES/Uz_file_single_WT.bin


cp $input_file $output_file1
  ## wavelet transform
  ./WT.exe $scale $level $NA $output_file1 $output_file2
##################################################################################################################

