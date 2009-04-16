open MASTER, "<calhoun_fixed_window.tex" or die "Can't even start!";
open BIGBOY, ">calhoun_draft.tex" or die "can't open to write!";

while (<MASTER>) {
    chomp;
    if (/^\\input\{(\w+)\}/) {
	open ADJUNCT, "<$1.tex" or die "can't open $1.tex";
	while (<ADJUNCT>) {
	    unless (/^\%\%\%/) {
		print BIGBOY;
	    }
	}
	close ADJUNCT;
    } else {
	print BIGBOY $_ . "\n";
    }
}

close MASTER;
close BIGBOY;
