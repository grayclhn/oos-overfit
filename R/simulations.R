## The variables nsim, njobs, jobnumber, and dbname are appended to
## the beginning of this file before execution.  This code *should*
## set up random number generation for the parallel processes
## correctly.

## I apologize to all future readers about the quality of this code. I
## have no idea why I thought these ideas were good, but there's no
## time to do anything about it now...

require(rlecuyer)
.lec.SetPackageSeed(c(4586, 8197, 5954,   15, 2896, 1839))
jobnames <- LETTERS[1:njobs]
.lec.CreateStream(jobnames)
.lec.CurrentStream(jobnames[jobnumber])
               
require(fwPackage, lib.loc = "package") ## look here for function definitions.

dbmain <- dbConnect(dbDriver("SQLite"), "data/simulations.db")

mcDesign <- dbGetQuery(dbmain,
"select nobs.i as isim, n, kNull, kAlt, norm, coefficients.i as idgp from nobs cross join coefficients")

dbDisconnect(dbmain)
rm(dbmain)

dbc <- dbConnect(dbDriver("SQLite"), dbname = dbname)
confidence <- .9

calcvar <- function(vcv, vcvvec) drop(crossprod(vcvvec, crossprod(vcv, vcvvec)))
locfn <- new.env(hash=TRUE)
varfn <- new.env(hash=TRUE)
## each element of these environments (ie hashes) is going to be a
## function of the form function(L, vcv)
##
## where L[1] and L[2] are something like the average out-of-sample loss

locfn[["difference"]] <- function(L) L[1] - L[2]
varfn[["difference"]] <- function(L, vcv) calcvar(vcv, c(1, -1))

locfn[["ratio"]] <- function(L) L[1] / L[2]
varfn[["ratio"]] <- function(L, vcv) calcvar(vcv, c(1/L[2], -L[1]/L[2]^2))

locfn[["rmse.difference"]] <- function(L) sqrt(L[1]) - sqrt(L[2])
varfn[["rmse.difference"]] <-
  function(L, vcv) calcvar(vcv, c(0.5/sqrt(L[1]), -0.5/sqrt(L[2])))

locfn[["rmse.ratio"]] <- function(L) sqrt(L[1] / L[2])
varfn[["rmse.ratio"]] <-
  function(L, vcv) calcvar(vcv, c(0.5/sqrt(L[1]*L[2]), -0.5*sqrt(L[1]/(L[2]^3))))

locfn[["log.difference"]] <- function(L) log(L[1]) - log(L[2])
varfn[["log.difference"]] <- function(L, vcv) calcvar(vcv, c(1/L[1], -1/L[2]))

getmoments <- function(x, lfn, vfn, scheme) {
  mapply(function(e1, e2) {
           oos.loss <- cbind(e1^2, e2^2)
           avg.loss <- colMeans(oos.loss)
           loc.loss <- lfn(avg.loss)
           var.loss <- vfn(avg.loss, var(oos.loss))
           c(loc = loc.loss, var = var.loss)
         },
         e1 = ForecastErrors(model.null(x), scheme = scheme),
         e2 = ForecastErrors(model.alt(x),  scheme = scheme))
}

cw.moments <- function(x, scheme,...) {
  mapply(function(e1, e2, f1, f2) {
           oos.loss <- e1^2 - e2^2 + (f1 - f2)^2
           avg.loss <- mean(oos.loss)
           var.loss <- var(oos.loss)
           c(loc = avg.loss, var = var.loss)
         },
         e1 = ForecastErrors(model.null(x), scheme = scheme),
         e2 = ForecastErrors(model.alt(x), scheme = scheme),
         f1 = forecasts(model.null(x), scheme = scheme),
         f2 = forecasts(model.alt(x), scheme = scheme))
}

tstat <- function(n, moments, mu)
  (moments[1,] - mu) * sqrt((n-1)/moments[2,])

ztest <- function(n, moments, mu, conf.level,
                  one.sided=rep(TRUE, length(conf.level))) {
  int <- cbind(- Inf, qnorm(conf.level + ifelse(one.sided, 0, {1 - conf.level} / 2)))
  int[!one.sided, 1] <- - int[!one.sided, 2]

  zstat <- tstat(n, moments, mu)
  zstat < int[,1] | zstat > int[,2]
}

initTable <- function(db, tablename, tablefn, remove.table = TRUE) {
  ## tablefn is any function that constructs a data.frame.  the
  ## initTable function removes the listed table from the database db
  ## (if remove.table is true), then returns a modified version of
  ## 'tablefn' that writes to the table in the database instead of
  ## creating a data.frame.
  ##
  ## We also insert a column "jobnumber" into the database that we'll
  ## use for parallelizing the code.
  if (remove.table & dbExistsTable(db, tablename)) dbRemoveTable(dbc, tablename)
  retfn <- function() {
    dbWriteTable(db, tablename,
                 data.frame(do.call("tablefn", as.list(match.call())[-1L]),
                            jobnumber = as.integer(jobnumber)),
                 append = TRUE, row.names = FALSE)
  }
  formals(retfn) <- formals(tablefn)
  retfn
}

add.teststat <- initTable(dbc, intervaltable,
                          function(statlist, isim, idgp, simIndex,
                                   ntest, label, scheme) {
                            if (length(label) == length(statlist)) {
                              do.call(rbind, lapply(seq_along(statlist), function(x) {
                                data.frame(ntest = as.integer(ntest),
                                           isim = as.integer(isim),
                                           idgp = as.integer(idgp),
                                           simIndex = as.integer(simIndex),
                                           reject = statlist[[x]],
                                           transform = names(statlist[x]),
                                           label = unname(label[x]),
                                           scheme = scheme)}))
                            } else {
                              do.call(rbind, lapply(seq_along(statlist), function(x) {
                                data.frame(ntest = as.integer(ntest),
                                           isim = as.integer(isim),
                                           idgp = as.integer(idgp),
                                           simIndex = as.integer(simIndex),
                                           reject = statlist[[x]],
                                           transform = names(statlist[x]),
                                           label = unname(label),
                                           scheme = scheme)}))}})

add.normalizeddiff <-
  initTable(dbc, difftable, function(statlist, isim, idgp, simIndex, ntest) {
    do.call(rbind, lapply(seq_along(statlist),
                          function(x) {
                            data.frame(ntest = as.integer(ntest),
                                       isim = as.integer(isim),
                                       idgp = as.integer(idgp),
                                       simIndex = as.integer(simIndex),
                                       value = statlist[[x]],
                                       transform = names(statlist[x]))}))})

add.ftest <- initTable(dbc, ftesttable,
                       function(testresult, isim, idgp, simIndex, label) {
                         data.frame(reject = testresult,
                                    isim = as.integer(isim),
                                    idgp = as.integer(idgp),
                                    simIndex = as.integer(simIndex),
                                    label)})

## These first few functions define the data generating process.
rX <- function(n, k) matrix(c(rep(1, n), rnorm(n * (k - 1))), n, k)
rY <- function(X, b) drop(tcrossprod(b, X)) + rnorm(nrow(X))

transvec <- ls(locfn)
nullerror <- sapply(transvec, function(trans) locfn[[trans]](c(1,1)),
                    simplify = FALSE)

for (r in rows(mcDesign)) {
  print(r)
  b.vector <- c(rep.int(sqrt(1/r$kNull), r$kNull),
                rep.int(sqrt(r$norm/(r$n)), r$kAlt - r$kNull))
  simIndex <- sizecount <- powercount <- 0
  repeat {
    simIndex <- simIndex + 1
    
    X <- rX(r$n, r$kAlt)
    Y <- rY(X, b.vector)
    oosDifference <- GenerateOosPair(X, Y, round(r$n/3, -1) + 10,
                                     r$kNull, r$kAlt, b.vector, TRUE)

    moments.fix <-
      sapply(transvec, function(trans)
             getmoments(oosDifference, locfn[[trans]], varfn[[trans]], "fix"),
             USE.NAMES = TRUE, simplify = FALSE)
    moments.rec <-
      sapply(transvec, function(trans)
             getmoments(oosDifference, locfn[[trans]], varfn[[trans]], "rec"),
             USE.NAMES = TRUE, simplify = FALSE)
    
    generror <- sapply(transvec, function(trans) {
      locfn[[trans]](c(expected.loss.future(model.null(oosDifference)),
                       expected.loss.future(model.alt(oosDifference))))},
                       USE.NAMES = TRUE, simplify = FALSE)
    if (generror[[1]] <= nullerror[[1]]) {
      simType <- "size"
      sizecount <- sizecount + 1
    } else {
      simType <- "power"
      powercount <- powercount + 1
    }
      
    testerror <- sapply(transvec, function(trans) {
      mapply(function(L1, L2) locfn[[trans]](cbind(L1, L2)),
             L1 = expected.loss.test(model.null(oosDifference)),
             L2 = expected.loss.test(model.alt(oosDifference)))
    }, simplify = FALSE)

    ntest <- ntest(oosDifference)
    
    ## Add one-sided test that mu = E_R D1-bar to database
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.fix[[trans]], testerror[[trans]],
            confidence, one.sided=TRUE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "test.error.1", "fix")

    ## Add two-sided test that mu = E_R D1-bar to database
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.fix[[trans]], testerror[[trans]],
            confidence, one.sided=FALSE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "test.error.2", "fix")

    ## Add one-sided test that mu = E_T D2-bar to database
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.fix[[trans]], generror[[trans]],
            confidence, one.sided=TRUE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "gen.error.1", "fix")
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.rec[[trans]], generror[[trans]],
            confidence, one.sided=TRUE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "gen.error.1", "rec")
      
    ## Add two-sided test that mu = E_T D2-bar to database
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.fix[[trans]], generror[[trans]],
            confidence, one.sided=FALSE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "gen.error.2", "fix")
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.rec[[trans]], generror[[trans]],
            confidence, one.sided=FALSE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, "gen.error.2", "rec")

    ## Add one-sided test that mu = 0 to database
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.fix[[trans]], nullerror[[trans]],
            confidence, one.sided=TRUE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, simType, "fix")
    add.teststat(sapply(transvec, function(trans) {
      ztest(ntest, moments.rec[[trans]], nullerror[[trans]],
            confidence, one.sided=TRUE)
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest, simType, "rec")

    ## Add McCracken's test that mu = 0 to database
    if (r$kAlt - r$kNull <= 10) {
      add.teststat(list(mccracken = tstat(ntest, moments.fix[["difference"]], 0) >
                        qmccracken(r$kAlt - r$kNull, ntest / (r$n - ntest), "fix")),
                   r$isim, r$idgp, simIndex, ntest, simType, "fix")
      add.teststat(list(mccracken = tstat(ntest, moments.rec[["difference"]], 0) >
                        qmccracken(r$kAlt - r$kNull, ntest / (r$n - ntest), "rec")),
                   r$isim, r$idgp, simIndex, ntest, simType, "rec")
    }
    ## Add Clark and West's test that mu=0 to database
    add.teststat(list(clarkwest = ztest(ntest, cw.moments(oosDifference, "fix"), 0,
                        confidence, one.sided=TRUE)),
                 r$isim, r$idgp, simIndex, ntest, simType, "fix")
    add.teststat(list(clarkwest = ztest(ntest, cw.moments(oosDifference, "rec"), 0,
                        confidence, one.sided=TRUE)),
                 r$isim, r$idgp, simIndex, ntest, simType, "rec")
    
    ## Add difference E_T D2-bar - E_R D1-bar normalized by variance
    add.normalizeddiff(sapply(transvec, function(trans) {
      sqrt(ntest/moments.fix[[trans]][2,]) * (generror[[trans]] - testerror[[trans]])
    }, simplify = FALSE), r$isim, r$idgp, simIndex, ntest)

    ## Add F-test to database
    add.ftest(f.test(oosDifference) < 1 - confidence,
              r$isim, r$idgp, simIndex, simType)

    ##    cat(sizecount, powercount, "--")
    if (r$norm == 1 & {sizecount >= nsim & powercount >= nsim}) break
    if (r$norm == 0 & {sizecount >= nsim}) break
    if (r$norm == 2 & {powercount >= nsim}) break
  }
}
dbDisconnect(dbc)
