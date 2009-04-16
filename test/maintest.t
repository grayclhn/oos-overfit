#! perl -w

use DBI;
use Test::More 'no_plan';

$R = "R CMD BATCH -q --no-save --no-restore-data";

ok(system("$R makeDataMatrices.R") == 0,
   "check data manipulation for empirical work.");

ok(system("$R rcw.R") == 0,
   "check the function that generates data for clark and west mc");

ok(system("$R rTestStat.R") == 0,
   "check function to generate test statistics for table 3 mc");
#ok(system("$R cw_prederrors.R") == 0,
#   "check the function that generates prediction errors " .
#   "for clark and west mc");
#
#ok(system("$R cw_eloss.R") == 0, "check the cw.eloss function");



#ok(system("$R cw_table.R") == 0,
#   "reproduce Clark and West's table --- may be time consuming.");
