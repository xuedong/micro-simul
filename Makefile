all:
	ocamlbuild assembleur/as.d.byte processeur/alu.d.byte simulateur/simul_main.native simulateur/simpl.d.byte -I description
	g++ -c afficheur/afficheur.cpp
	g++ -o afficheur/afficheur afficheur.o -lsfml-graphics -lsfml-window -lsfml-system
	./alu.d.byte > proco.net
	cat montre.asm |  ./as.d.byte > montre_fast.ram
	cat montre.asm | sed "s/^;RT//g" | ./as.d.byte > montre_rt.ram

clean:
	ocamlbuild -clean
	rm -f proco.net
	rm -f montre_fast.ram
	rm -f montre_rt.ram
	rm afficheur/afficheur afficheur.o

