#!/bin/bash
for i in `seq 1 72`;
do
echo Submitting job $i
qsub -v "j_id=$i" /a/home/cc/stud_math/axelrod1/conditional_hazard_of_Cox/pbs/hr_con_cox_wrapper_parallel.pbs
done
