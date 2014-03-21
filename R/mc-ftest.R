library(fwPackage, lib.loc="package")
library(tikzDevice)
library(lattice)
library(dbframe)

ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme  
ltheme$strip.background$col <- "transparent" ## change strip bg  
lattice.options(default.theme = ltheme)      ## set as default  

dbc <- dbConnect(dbDriver("SQLite"), dbname = "data/simulations.db")
d <- dbGetQuery(dbc, "select reject, n, kNull, nulllabel, altlabel, norm
                      from viewftest where label = 'size'")
dbDisconnect(dbc)

d$nulllabel[d$nulllabel == "K0=2"] <- "$2$"
d$nulllabel[d$nulllabel == "K0=n/50"] <- "$T/50$"
d$altlabel[d$altlabel == "K=3"] <- "$3$"
d$altlabel[d$altlabel == "K=T/10"] <- "$T/10$"

dl <- lapply(unique(d$norm), function(dnorm) d[d$norm == dnorm,])
dd <- dl[[1]][, c("nulllabel", "altlabel", "n", "reject")]
names(dd) <- c("$K_1$", "$K_2$", "$T$", "$| \\theta_2 |_2 = 0$")
dd[,"$| \\theta_2 |_2 = 1$"] <- dl[[2]]$reject

tabscode <- booktabs(dd,
                     align = c("l", "l", "C", "C", "C"),
                     digits = c(0, 0, 0, 3, 3),
                     numberformat = c(FALSE, FALSE, TRUE, TRUE, TRUE))
tabscode <- gsub("\\\\toprule", "\\\\toprule &&&
 \\\\multicolumn{2}{c}{Conditional rejection probability (size)} \\\\\\\\
 \\\\cmidrule(l){4-5}", tabscode)

cat(file = "floats/mc-ftest.tex", "\\begin{table}[tb]\n", tabscode,
    "\\caption{Simulated rejection probabilities for the $F$-test under
the null hypothesis that the benchmark model will forecast better,
$\\E_T \\oosB \\leq 0$. Nominal size is 10\\% and values greater than
10\\% indicate that the test rejects the benchmark model too often.
See Section~\\ref{sec:simulation-design} for a discussion of the
simulation design.}
\\label{fig:ftest}
\\end{table}")
