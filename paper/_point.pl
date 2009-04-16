open MASTER, "<calhoun_fixed_window.tex" or die "Can't even start!";
open BIGBOY, ">calhoun_outline.tex" or die "can't open to write!";
open TEMP, ">temp.txt";

chomp($line = <MASTER>);
until ($line =~ /\\section/) {
    chomp($line = <MASTER>);
}

print BIGBOY "\\documentclass[11pt]{article} \n" .
    "\\usepackage{amssymb,amsmath,amsthm,harvard,graphicx} \n" .
    "\\setcounter{MaxMatrixCols}{10} \n" .
    "\\setlength{\\voffset}{0.22in} \n" .
    "\\setlength{\\hoffset}{-0.06in} \n ".
    "\\setlength{\\topmargin}{0in} \n" .
    "\\setlength{\\headheight}{0in}\n" .
    "\\setlength{\\headsep}{0in}\n" .
    "\\setlength{\\textheight}{7.33in}\n" .
    "\\setlength{\\oddsidemargin}{0in}\n" .
    "\\setlength{\\textwidth}{5.67in}\n" .
    "\\input{../bib/definitions}\n" .
    "\\input{def}\n" . 
    "\\newcommand{\\pp}[1]{\\textbf{#1}}\n";

print BIGBOY "\\begin{document}";
print BIGBOY $line . "\\begin{itemize}\n";
while (<MASTER>) {
    chomp;
    if (/\\(\w)*section/) {
	print BIGBOY "\\end{itemize}\n" . $_ . "\n\\begin{itemize}\n";
    } elsif (/\\context/) {
	print BIGBOY "\\end{itemize}\n " .
	    "\\subsection{Shared Context} \n\\begin{itemize}\n";
    } elsif (/\\problem/) {
	print BIGBOY "\\end{itemize}\n " .
	    "\\subsection{Statement of Problem} \n\\begin{itemize}\n";
    } elsif (/\\sowhat/) {
	print BIGBOY "\\end{itemize}\n " .
	    "\\subsection{So What?} \n\\begin{itemize}\n";
    } elsif (/\\solution/) {
	print BIGBOY "\\end{itemize}\n " .
	    "\\subsection{Statement of Solution} \n\\begin{itemize}\n";
    } elsif (/\\begin{(thm|lem|asmp)}/) {
	print BIGBOY "\\item[$1]\n";
	$match = $1;
	$inline = <MASTER>;
	until ($inline =~ m/\\end{$1}/) {
	    print BIGBOY $inline;
	    $inline = <MASTER>;
	}
    } elsif (/(\\intro|\\pp|\\point)/) {
	$inline = $_;
	$mtemp = $1;
	if ($mtemp =~ /\\intro/) {
	    print BIGBOY "\\item[Intro:] ";
	    $inline =~ s/.*\\intro{//;
	} elsif ($mtemp =~ /\\point/) {
	    print BIGBOY "\\end{itemize}\n \\subsection{Key Point} \n ";
	    $inline =~ s/\\point{//;
	} else {
	    print BIGBOY "\\item ";
	    $inline =~ s/.*\\pp{//;
	}
	$bracket = 1;
	while ($bracket > 0) {
	    print TEMP "1) Inline is: $inline\n";
	    $inline =~ s/(\{|\})//;
	    $m = $1;
	    $prematch =  $`;
	    $postmatch = $';
	    print TEMP "2) Prematch is: $prematch\n";
	    if ($m =~ m/{/) {
	        $inline = $postmatch;
		$bracket = $bracket + 1;
		print BIGBOY "$prematch$m\n";
	    } elsif ($m =~ m/}/) {
	        $inline = $postmatch;
		$bracket = $bracket - 1;
		print BIGBOY "$prematch$m\n";
	    } else {
		print BIGBOY "$inline \n";
		chomp($inline = <MASTER>);
	    }
	    print TEMP "3) Now inline is : $inline\n";
	}
    }
}
print BIGBOY "\\end{itemize}\n\\end{document}";

close MASTER;
close BIGBOY;

system("pdflatex calhoun_outline.tex -interaction=nonstopmode -quiet");

