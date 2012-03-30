R := R CMD BATCH -q --no-restore-data --no-save
comma := ,

.PHONY: all

all: quadQQ.png blankPlot.png quadWindow.png quadTest.png overfitPlot.png overfitHist.png overfitQQ.png quadHist.png handout.pdf overfitAsymptotic.png approximationQQ.png approximationHist.png

handout.pdf: handout.tex
	- pdflatex handout.tex --quiet
	- pdflatex handout.tex --quiet

blankPlot.png quadWindow.png quadTest.png overfitPlot.png overfitAsymptotic.png: outofsamplePlots.R mcGlobal.Rdata
	$(R) $<

quadHist.png quadQQ.png: quadQQ.R simulations.Rdata mcGlobal.Rdata
	$(R) $<

overfitHist.png overfitQQ.png: overfitQQ.R simulations.Rdata mcGlobal.Rdata
	$(R) $<

simulations.Rdata: outofsampleMC.R mcGlobal.Rdata
	$(R) $<

mcGlobal.Rdata: makeMCGlobal.R
	$(R) $<

approximation.Rdata: approximationMC.R mcGlobal.Rdata
	$(R) $<

approximationQQ.png approximationHist.png: approximationQQ.R approximation.Rdata mcGlobal.Rdata
	$(R) $<