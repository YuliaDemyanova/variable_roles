#~/bin/bash
output_roles=$OUTPUT_ROLES_FILE
output_metrics=$OUTPUT_METRICS_FILE

while getopts "f:r:m:" opt; do
	case $opt in
		f) input_file=$OPTARG;;
		r) output_roles=$OPTARG;;
		m) output_metrics=$OPTARG;;
	esac
done

if [ "$input_file" == "" ]; then
cat << EOF
usage: `basename "$0"` -f <input> [-r <output_roles>] [-m <output_metrics>]
  input          - name of input file
  output_roles   - name of output file with role assignments
  output_metrics - name of output file with metrics
EOF
exit 1
fi

CFLAGS=-I"${CLANG_HEADERS}"
tmp=$(mktemp)
echo "tmp (run_varroles_new) = $tmp"

trans_rel=${tmp}_transition_relation
logic_prog=${tmp}_input.lp
ROLE_SPECS=`ls ${ROLES_DIR}/*.txt`

mkdir -p ${RESULTS_DIR}
log=${RESULTS_DIR}/log
clingo_log=$RESULTS_DIR/clingo_log

translate_code()
{
	#echo -n $input_file 'feature extraction...'

#	STARTTIME=$(date +"%s%3N")
	$translator $input_file -- $CFLAGS  2>> ${RESULTS_DIR}/err > $trans_rel
	if [ $? -ne 0 ]; then
		echo ERROR: feature extraction failed on $input_file
		cat ${RESULTS_DIR}/err
		exit 1;
	fi

#	ENDTIME=$(date +"%s%3N")
#	duration1=$(($ENDTIME - $STARTTIME))
}

compute_roles()
{
	sort $trans_rel | uniq | cat - ${ROLE_SPECS} > $logic_prog

	FLAG="-n 1 --heuristic=Domain --out-hide-aux"
	STARTTIME=$(date +"%s%3N")
	$CLINGO $logic_prog $FLAG 2>> $clingo_log > $log
	ENDTIME=$(date +"%s%3N")
	duration2=$(($ENDTIME - $STARTTIME))

#echo clingo_time $duration1 ms

}

extract_metrics()
{
	roles=`cat $log | tr ' ' '\n' | sed -n 's/[lg]fp(\([^)]*\))/\1 /p' | sort | uniq`
#	echo roles=$roles
	if [ ! -e $output_metrics ] || [ ! -s $output_metrics ]; then echo file $roles > $output_metrics; fi

	echo $input_file >> $output_roles
	echo -n $input_file" " >> $output_metrics

	id_num_total=`cat $log | tr ' ' '\n' | grep identifier | wc -l`
	#counts for both ext_func and ext_func_param
	id_num_external=`cat ${RESULTS_DIR}/log | tr ' ' '\n' | grep ext_func | wc -l` 
	let id_num_nonext=$id_num_total-$id_num_external
	metrics_str=""

	for role in $roles; do
		vars=`cat $log | tr ' ' '\n' | sed -n 's/result("\(.*\)",\s*'$role')/\1 /p' | grep -v _datalog_tmp`
		echo $role : $vars >> $output_roles
		var_num=`echo $vars | wc -w`
		frac=`echo "scale=2; (100*$var_num/$id_num_nonext)" | bc`
		echo -n "$frac " >> $output_metrics
#		echo role=$role vars=$vars frac=$frac
	done
#	ENDTIME=$(date +"%s%3N")
	duration3=$(($ENDTIME - $STARTTIME))
#	echo translation: $duration1 ms, solving: $duration3 ms
	echo $input_file 
	echo >> $output_roles
	echo >> $output_metrics
}

translate_code
compute_roles
extract_metrics
#	echo -n ' ('$duration1's) solving asp program...'
#	STARTTIME=$(date +"%s%3N")


#/bin/rm ${tmp}* log*
