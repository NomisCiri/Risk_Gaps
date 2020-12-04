#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyJobs=100
HowManyModels=2
#j=2;
#i=1;
#k=5;

PATH_LOG_E=./C_PowerAnalysis/logsE/$(date '+%Y%m%d_%H%M%S')
PATH_LOG_O=./C_PowerAnalysis/logsY/$(date '+%Y%m%d_%H%M%S')
# path to the text file with all subject ids:
PATH_SUB_LIST="${PATH_SCRIPT}/subs.txt"
# CREATE RELEVANT DIRECTORIES:
# ==============================================================================
# create output directory:
if [ ! -d ${PATH_LOG_E} ]; then
	mkdir -p ${PATH_LOG_E}
fi
# create directory for log files:
if [ ! -d ${PATH_LOG_O} ]; then
	mkdir -p ${PATH_LOG_O}
fi


for i in `seq 1 $HowManyJobs`;
do
	for j in `seq 2 $HowManyModels`;

	do
		echo "$i  $Subjects";
		#here i specify the job for the cluster.
		#for input_file in INPUT/* ; do
			#echo "#PBS -m n"                         > job.pbs
			echo "#PBS -N Bayesian_Power$i$j" > job.pbs
			echo "#PBS -l mem=64gb" 				>> job.pbs
			echo "#PBS -j oe"                       >> job.pbs
			echo "#PBS -l walltime=200:00:0"        >> job.pbs
			echo "#PBS -m n"                         >> job.pbs
     			echo "#PBS -e ${PATH_LOG_E}"              >> job.pbs
      			echo "#PBS -o ${PATH_LOG_O}"              >> job.pbs
			echo "#PBS -d ."                        >> job.pbs
			echo "#PBS -l nodes=1:ppn=4"            >>job.pbs
			echo "module load R/3.5; Rscript Power_Sims.R $i $j"	>> job.pbs
			qsub job.pbs
			rm -f job.pbs
		done
	done
