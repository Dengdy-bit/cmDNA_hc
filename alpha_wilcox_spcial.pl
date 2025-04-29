#!usr/bin/perl
use strict ;
my $indexdir = shift ;
#my $outdir = shift ;
#use Getopt::Long ;
#my ($infile,$outdir) ;
#GetOptions(
#	"infile:s" =>\$infile,
#	"outdir:s"=>\$outdir
#);
#usageall()if(!defined $infile || !defined $outdir) ;
#sub usageall{
#	die qq/
#	perl wilcox.pl -infile <index tex file> -outdir <out die file> 
#\n/;
#}

#my @indexs = glob "$indexdir/*/*.txt"

my @arrys = ("chao1","shannon","simpson") ;
foreach my $index(@arrys){
my $infile = "$indexdir/$index/$index.txt" ;
my $outdir = "$indexdir/$index" ;
#print "$outdir\n";
my $key ;
open IN,"$infile" or die;
my @gs ;
my %ha ;
my $flag;
my $head = <IN> ;
chomp $head ;
my @l = split (/\t/,$head);
$key = $l[1] ;
while(<IN>){
	chomp ;
	my @l = split (/\t/,$_) ;
	if($l[0]=~/-/){$flag=1;$l[0]=~s/-/_/g;}	
#	if (!defined @{$ha{$l[0]}}){my @{$ha{$l[0]}};}
	push (@{$ha{$l[0]}},$l[1]) ;
	push (@gs,$l[0]) ;
}
close IN ;
my %count ;
my @uniq_times = grep { ++$count{$_} < 2; } @gs;
#print "@uniq_times\n" ;
#foreach my $g(@uniq_times){
#	print "$g\t@{$ha{$g}}\n";
#}
#system("rm $outdir/test.txt") ;
my $i ;
for ($i=0;$i<=@uniq_times-1;$i++){
	my $m ;
	for ($m = $i+1 ;$m<= @uniq_times-1;$m++){
		my $gname1 = $uniq_times[$i] ; 
		my $gname2 = $uniq_times[$m] ;
		my $linex .= "c\(";
		my $liney .= "c\(";
	#	my $len = @{$ha{$gname1}} -1 ;
	#	print "$len\n";
		my $x ;
		for ($x=0;$x<= @{$ha{$gname1}} -1;$x++){
	#		print "$x\t${$ha{$gname1}[$x]}\n" ;
			if ($x == 0){
			#	print "....${$ha{$gname1}}[$x]\t$linex\n";
				$linex .= "${$ha{$gname1}}[$x]" ;
			}else{
				$linex .= " \,${$ha{$gname1}}[$x]" ;
			}
		}
		my $y ;
		for ($y=0;$y<=@{$ha{$gname2}}-1;$y++){
			if ($y ==0 ){
				$liney .= "${$ha{$gname2}}[$y]" ;
			}else{
				$liney .= " \,${$ha{$gname2}}[$y]" ;
			}
		}
		$linex .= "\)";
		$liney .= "\)";
		$gname1="S$uniq_times[$i]"; 
		$gname2="S$uniq_times[$m]";
		open R, ">$outdir/$gname1.vs.$gname2.r" or die ;
		print R "$gname1 <- $linex\n";
		print R "$gname2 <- $liney\n";
		print R "wilcox.test($gname1, $gname2,exact = FALSE, correct = FALSE,conf.int = TRUE, parameter=T,statistic=T)\n";
#		print R "write.table(data,file=\"$gname1.vs.$gname2.txt\")\n";
		system ("Rscript $outdir/$gname1.vs.$gname2.r >>$outdir/test.txt") ;
		system ("rm $outdir/$gname1.vs.$gname2.r" ) ;
	}
}
open OUT,">$outdir/$key.wilcox.txt" ;
print OUT "Group\tDifference\tpvalue\tsig.\tLCL\tUCL\n";
open IN,"$outdir/test.txt" or die ;
while (<IN>) {
	my ($name,$dif,$pv,$sig,$LCL,$UCL) ;
	if (/data/){
		my $a = $_ ; chomp $a ;
		my @l = split (/\s+/,$a) ; if($flag==1){ $l[1]=~s/\_/-/g; $l[3]=~s/\_/-/g; } $l[1]=~s/^S//; $l[3]=~s/^S//;   $name = "$l[1] - $l[3] " ;
		my $b = <IN> ;chomp $b ;
			my @l = split (/\s+/,$b) ; $pv = $l[-1] ;
			if ($pv <= 0.05 ){$sig = "*";}else{$sig = "";}
		my $c = <IN> ;chomp $c ;
		my $d = <IN> ;chomp $d ;
		my $e = <IN> ;chomp $e ;
			my @l = split (/\s+/,$e) ; $LCL = $l[1] ;$UCL = $l[2] ;
		my $f = <IN> ;chomp $f ;
		my $g = <IN> ;chomp $g ;
		my $h = <IN> ;chomp $h ;
			$dif = $h ; $dif =~ s/\s+//g ;
		
		print OUT "$name\t$dif\t$pv\t$sig\t$LCL\t$UCL\n" ;
	}else {
		next ;
	}
}
close IN ;
close OUT ;
system("rm  $outdir/test.txt ") ;
}
