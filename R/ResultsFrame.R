## The function 'ResultsFrame' takes a string that contains the text
## for an SQL query and returns a dataframe giving its results and
## adding ordered factors that label the coefficient values, number of
## observations, and number of regressors.
setGeneric("ResultsFrame", function(object,...) standardGeneric("ResultsFrame"))
setMethod("ResultsFrame", "character",
          function(object,...) {
            nlabel <- dbGetQuery(dbc, "select distinct nlabel from nobs order by n;")$nlabel
            altlabel <- dbGetQuery(dbc, "select distinct altlabel from nobs order by kAlt;")$altlabel
            nulllabel <- dbGetQuery(dbc, "select distinct nulllabel from nobs order by kNull;")$nulllabel
            normlabel <- dbGetQuery(dbc, "select distinct normlabel from coefficients order by norm;")$normlabel
            d <- dbGetQuery(dbc, paste("select * from (", object, ") s, pars p ",
                                       "on s.isim = p.isim and s.idgp = p.idgp;"))
            d$ntotal <- d$n
            k0 <- ordered(d$nulllabel, nulllabel)
            ka <- ordered(d$altlabel, altlabel)
            d$k <- k0:ka
            d$b <- ordered(d$normlabel, normlabel)
            d$n <- ordered(d$nlabel, nlabel)
            subset(d, select = c(-isim, -idgp, -nlabel, -kNull, -nulllabel, -kAlt, 
                        -altlabel, -norm, -normlabel))
          })
