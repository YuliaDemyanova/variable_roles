export VARROLES_ROOT=`pwd`

######################################
export LLVM_PATH=~/tools/clang_3.4/
export CLANG_HEADERS=~/tools/clang_3.4/build/lib/clang/3.4.2/include/
export STANDARD_GCC_HEADERS=/usr/lib/gcc/x86_64-linux-gnu/4.8/include
#####################################


export ROLES_DIR=${VARROLES_ROOT}/src/roles
export SCRIPTS_DIR=${VARROLES_ROOT}/src/scripts
export RESULTS_DIR=${VARROLES_ROOT}/results

export CLINGO=${VARROLES_ROOT}/tools/clingo-4.5.4-linux-x86_64/clingo

export var_roles=${SCRIPTS_DIR}/run_varroles.sh
export run_clingo=${SCRIPTS_DIR}/run_clingo.sh
export translator=${VARROLES_ROOT}/src/translator/translator

export OUTPUT_ROLES_FILE=${RESULTS_DIR}/roles
export OUTPUT_METRICS_FILE=${RESULTS_DIR}/metrics
