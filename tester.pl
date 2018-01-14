#!/usr/bin/perl -w

# Exemple d'utilisation :
# ./tester.pl simulateur/simul simulateur/exemples/FullAdder.net < description/FullAdder.test

$nb_tests = 0;
$tests_ok = 0;

if($#ARGV + 1 != 2) {die "Exemple d'utilisation :\n./tester.pl simulateur/simul simulateur/exemples/FullAdder.net < description/FullAdder.test\n"}

while($entree = <STDIN>) {
    if($entree !~ m/^> ([01]*)$/ ) {die "Fichier mal formé."}
    $in = $1;
    if(!($sortie = <STDIN>) || $sortie !~ m/^< ([01]*)$/) {die "Fichier mal formé."}
    $out = $1;

    $nb_tests++;
    open(SIMUL, "echo $in | $ARGV[0] -b -n 1 $ARGV[1]|");
    chop($reponse = <SIMUL>);
    close(SIMUL);
    $reponse =~ s/ //g;

    if($reponse ne $out) {
	print "Mauvaise réponse : entrée = ", $in, ", sortie = ", $reponse, ", sortie attendue = ", $out, ".\n"
    } else {
	$tests_ok++;
    }
}

print "Tests passés : ", $tests_ok, " sur ", $nb_tests, ".\n";
