12/23/2010

All of the analyses for the paper, "Out-of-Sample Comparisons of
Overfit Models" (Calhoun, 2010) can be run automatically through the
Makefile.  If you're unfamiliar with Makefiles, you can find the
manual for GNU make (which is what I use) at

http://www.gnu.org/software/make/manual/make.html

Please let me know if the url is broken.  The command

make -n

lists all of the commands that need to be executed.  The command

make

runs them all.  This can take a while, since it runs the Monte Carlo
too.  Some of the key files are:

paper.pdf         - the final version of the paper

slides.org	  - outline of the slides I've used when presenting
		    this paper.  Open this file with GNU Emacs for the
		    best results (it uses org-mode).

Makefile          - specifies the order of code execution for the paper.
		    Please see this file for details.

mc-setup.py	  - additional dependencies that allow the Monte Carlo
		    to be parallelized.  This file controls part of
		    the Makefile.

mc/simulations.db - contains all of the simulated data for the paper's
		    monte carlo exercises.  

mc/db/*		  - files that control the creation of
		    mc/simulations.db

mc/*.Rnw	  - files creating different plots for the Monte
		    Carlo.  This code is also presented in the
		    corresponding .pdf file, and the plots are stored
		    in mc/plots

empirics/oos-anlaysis.R - code to run empirical exercise based on
			  Goyal and Welch (2008).

empirics/tables   - tables for the empirical section
empirics/plots	  - plots for the empirical section

Please contact me if you find any errors or have any questions: 
Gray Calhoun <gcalhoun@iastate.edu>
