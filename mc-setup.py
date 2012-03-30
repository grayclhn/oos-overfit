# The basic idea of this python script is to set up the right
# dependencies for GNU make so that the monte carlo simulations are
# run in parallel.  Running this file will print out text for a
# makefile, so it might be easiest to just look at its output.

nsims = 2000 # this is the number of simulations
njobs = 7    # I set this to equal the number of processors on my 
             # computer.  Changing its value will change the results
             # of the simulations, since they'll be seeded differently.
def maindb(sql):
    print("\t$(sqlite) $(sqliteFlags) mc/simulations.db \"" + sql + ";\"")

# The main idea is that we're creating a separate temporary database
# file for each job (to avoid problems that RSQLite has with
# concurrant writing).  The list 'dbnames' contains the name of each
# of those temporary databases, and the list 'dbfiles' is the
# corresponding file name.
dbnames = ["oosstats" + str(i + 1) + "of" + str(njobs) for i in range(njobs)]
dbfiles = ["mc/db/" + d + ".db" for d in dbnames]

intervaltable = "interval"
difftable     = "diff"
ftesttable    = "ftest"
# split the simulations across the jobs; any left over simulations (if
# there are 10 simulations and 8 jobs, there will be 2 simulations
# "left over") are added to the first job.
jsims = [nsims/njobs + (i==0) * (nsims % njobs) for i in range(njobs)]

# There's no reason to keep the initial database files around, since
# their contents are immediately transferred to the main database.
#print ".INTERMEDIATE: " + " ".join(dbfiles)

# This is the set of commands to put all of the simulations into a
# single database.
def movetable(tablename, dbs):
    return("create table " + tablename + " as " +
           " union all ".join(["select * from " + d + "." + tablename for d in dbs]) + "; ")

def droptable(tablename):
    return("drop table if exists " + tablename + "; ")

def makeview(tablename, query):
    maindb("drop view if exists view" + tablename + "; " +
           "create view view" + tablename + " as select * from (" + query + ") s " +
           "join nobs n join coefficients c on n.i=s.isim and c.i=s.idgp;")

def makeindex(indexname, sql):
    maindb("drop index if exists " + indexname + "; " + 
           "create index " + indexname + " on " + sql)

print "mc/db/oosstats.created: " + " ".join(dbfiles)
maindb(droptable(intervaltable) + droptable(difftable) + droptable(ftesttable) +
       "".join(["attach database '" + d1 + "' as " + d2 + "; " 
                for (d1,d2) in zip(dbfiles,dbnames)]) +
       movetable(intervaltable, dbnames) + 
       movetable(difftable, dbnames) + 
       movetable(ftesttable, dbnames) + 
       "".join(["detach database " + d + "; " for d in dbnames]))
# These commands create views and indices for the new tables
makeindex("intervalIndex", "interval (label, idgp)")
# makeindex("ftestIndex", "ftest (isim, idgp, label, jobnumber, simIndex)")
# makeindex("diffIndex", "diff (isim, idgp, transform, ntest, jobnumber, simIndex)")
makeview(intervaltable, "select ntest, isim, idgp, transform, label, avg(reject) as reject " +
         "from " + intervaltable + " group by ntest, isim, idgp, transform, label")
makeview(difftable, "select * from " + difftable)
makeview(ftesttable, "select isim, idgp, label, avg(reject) as reject from " 
         + ftesttable + " group by isim, idgp, label")
print "\ttouch $@"

# This is the set of make commands to create the temporary databases.
for (db, n, j) in zip(dbnames, jsims, range(njobs)):
    print ("mc/db/" + db + ".db: mc/db/oosstats.R mc/db/nobs.created mc/db/coefficients.created\n"
           + "\techo 'dbname <- \"$@\"; "
           + "nsim <- " + str(n) + "; " 
           + "jobnumber <- " + str(j+1) + "; "
           + "njobs <- " + str(njobs) + "; "
           + "intervaltable <- \"" + intervaltable + "\"; "
           + "ftesttable <- \"" + ftesttable + "\"; "
           + "difftable <- \"" + difftable + "\"; "
           + "' | cat - $< | Rscript $(RFLAGS) - ")
