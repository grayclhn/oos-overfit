library(fwPackage, lib.loc="package")
library(tikzDevice)
library(lattice)

ltheme <- canonical.theme(color = FALSE)     ## in-built B&W theme  
ltheme$strip.background$col <- "transparent" ## change strip bg  
lattice.options(default.theme = ltheme)      ## set as default  

dbc <- dbConnect(dbDriver("SQLite"), dbname = "data/simulations.db")
d <- dbGetQuery(dbc,
                "select reject, 'T=' || n as nlabel, altlabel, norm from viewftest 
                 where label='size' and norm in (0,1)")
d$norm <- as.factor(d$norm)

tikz(file = "floats/mc-ftest.tex", width = 5.5, height = 4)
dotplot(norm ~ reject | interaction(altlabel,nlabel, sep = ", "),
              dat = d, las = 1,
              xlab = "Rejection Probability", ylab = "c",
              xlim = c(-.1, 1.1),
              panel = function(x, y, ...) {
                panel.dotplot(x, y, ..., type = "n")
                panel.text(x, as.numeric(y), labels = sprintf("%.2f", x))
              },
              par.strip.text = list(cex = .55),
              layout = c(2,4),
              index.cond = list(c(3,4,7,8,5,6,1,2)))
dev.off()
dbDisconnect(dbc)
