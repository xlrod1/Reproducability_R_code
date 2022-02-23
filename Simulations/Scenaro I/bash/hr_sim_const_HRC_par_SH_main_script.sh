#!/bin/bash
for i in `seq 1 12`;
do
echo Submitting job $i
qsub -v "j_id=$i" /a/home/cc/stud_math/axelrod1/simulations_on_cluster/New_only_two_scenarios/without_confounder/pbs/hr_sim_const_HRC_wrapper_parallel.pbs
done
