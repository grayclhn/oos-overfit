# Copyright 2015-2016, Gray Calhoun
# MIT license. See the file README.md

# Makefile instructions to write git version information to the file
# VERSION.tex. This creates the new LaTeX macro "\VERSION".

VERSION.tex:
	echo "\newcommand\VERSION{$$(./version_git.sh)}" > $@
