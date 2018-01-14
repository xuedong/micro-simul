yes | ./simul_main.native proco.net --ram $1 --time | grep -E 'r19|r20|r21|r22|r24|r25|r26|r27' | cut -d ' ' -f 5 | ./afficheur/afficheur
