PREREQUISITES:
  - gcc
  - clang 3.4

RUNNING the tool:

1. Edit file src/scripts/export_paths.sh
   Variable CLANG_PATH should point to the
   installation directory of clang.

   Variable STANDARD_GCC_HEADERS should point to the directory which
   containts gcc headers

2. Export variables:
   $ . src/scripts/export_paths.sh

3. Build the tool:
   $ make

   Check that no errors are generated and the binary file src/translator/translator
   has been created:
   $ file src/translator/translator

4. Run the tool:
   $ src/scripts/run_varroles.sh -f <input> [-r <output_roles>] [-m <output_metrics>]

	where <input> is the name of input C file
		<output_roles> is the name of output file with role assignments
		<output_metrics> is the name of output file with metrics

   The result will be stored in results/metrics and results/roles