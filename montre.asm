;;; documentation:
;;; heure : r25:r26:r27
;;; jour de la semaine : r24 (0 = lundi)
;;; date : r20/r21/r22+2000
        j start
        
        .align 2
        .word 951778200
        .fill 8

        .align 16
start:
        load r30, zero, 2
        ;; constantes pour division par 60, merci hacker's delight
        addlo r4, zero, 60
        addhi r3, zero, 0x8889
        addlo r3, r3, 0x8889
        ;; la division
        mul zero, r29, r30, r3
        saradd r29, r29, r30, zero
        sar r29, r29, 5
        mul r5, zero, r29, r4
        muladd r27, r30, -1, r5
        ;; redivision par 60 pour avoir les minutes
        mul zero, r28, r29, r3
        saradd r28, r28, r29, zero
        sar r28, r28, 5
        mul r5, zero, r28, r4
        muladd r26, r29, -1, r5
        ;; division par 24 pour avoir les heures
        addlo r4, zero, 24
        addhi r3, zero, 0x2AAB
        addlo r3, r3 , 0xAAAB
        ;; la division
        mul zero, r29, r28, r3
        sar r29, r29, 2
        mul r5, zero, r29, r4
        muladd r25, r28, -1, r5
     
;;; halfway there ! plus qu'à se charger du jour/mois/année
        addlo r5, zero, 10957
        muladd r29, r29, -1, r5
        ; on réaligne au 1er janvier 2000
        ;; division par 7 (jour de la semaine)
        ;; le premier janvier 2000 était un samedi
        addlo r6, r29, 5
        addlo r4, zero, 7
        addhi r3, zero, 0x9249
        addlo r3, r3, 0x2493

        mul zero, r28, r6, r3
        saradd r28, r28, r6,zero
        ; notre multiplication maintenant signée donc c'est nécessaire
        sar r28, r28, 2 
     
        mul r5, zero, r28, r4
        muladd r24, r6, -1, r5

        ;; division par 1461
        addlo r4, zero, 1461
        addhi r3, zero, 0x166F
        addlo r3, r3, 0xB073
        ;; la vraie division
        mul zero, r28, r29, r3
        sar r28, r28, 7
        mul r5, zero, r28, r4
        muladd r23, r29, -1, r5

        ;; il faut trouver la vraie année maintenant
        addlo r22, zero, 0

        addlo r4, zero, 366
        cmp r23, r4
        if lt j annee_end
        if gte addlo r23, r23, -366
        if gte addlo r22, r22, 1

        addlo r4, zero, 365
        cmp r23, r4
        if lt j annee_end
        if gte addlo r23, r23, -365
        if gte addlo r22, r22, 1

        addlo r4, zero, 365
        cmp r23, r4
        if lt j annee_end
        if gte addlo r23, r23, -365
        if gte addlo r22, r22, 1

annee_end:      
        
        addlo r7, r22, 0
        slladd r22, r22, r28, 2

        ;; puis le mois
        la r8, months
        cmp r7, zero
        if eq la r8, bisextile
        addlo r21, zero, 0
        addlo r20, r23, 0
     
monthloop:
        addlo r21, r21, 1
        load r9, r8, 0
        cmp r20, r9
        if gte addlo r8, r8, 1
        if gte muladd r20, r20, -1, r9
        if gte j monthloop

        ;; et enfin le jour du mois
        addlo r20, r20, 1

;; la boucle principale de la montre
;; r19 est à 1 lorsque les valeurs des registres sont stables
watchloop:
	addlo r19, zero, 1
	addlo r19, zero, 0
	addlo r27, r27, 1

	;; minute suivante ?
	addlo r4, zero, 60
	cmp r27, r4
	if lt j waitloop
	addlo r26, r26, 1
	addlo r27, zero, 0

	;; heure suivante ?
	cmp r26, r4
	if lt j waitloop
	addlo r25, r25, 1
	addlo r26, zero, 0

	;; jour suivant ?
	addlo r4, zero, 24
	cmp r25, r4
	if lt j waitloop
	addlo r20, r20, 1
	addlo r25, zero, 0
	addlo r24, r24, 1

	;; semaine suivante ?
	addlo r4, zero, 7
	cmp r24, r4
	if gte addlo r24, zero, 0

	;; mois suivant ?
	cmp r20, r9
	if lte j waitloop
	addlo r21, r21, 1
	addlo r20, zero, 1
        addlo r8, r8, 1
        load r9, r8, 0

	;; année suivante ?
	addlo r4, zero, 12
	cmp r21, r4
	if lte j waitloop
	addlo r22, r22, 1
	addlo r21, zero, 1
	slr r23, r22, 30
        la r8, months
        cmp r23, zero
        if eq la r8, bisextile
        load r9, r8, 0

        ;; attendre
waitloop:

;RT        load r29, zero, 2
;RT        cmp r29, r30 
;RT        if lte j waitloop

        addlo r30, r30, 1
        j watchloop

months:
        .word 31
        .word 28
        .word 31
        .word 30
        .word 31
        .word 30
        .word 31
        .word 31
        .word 30
        .word 31
        .word 30
        .word 31
bisextile:
        .word 31
        .word 29
        .word 31
        .word 30
        .word 31
        .word 30
        .word 31
        .word 31
        .word 30
        .word 31
        .word 30
        .word 31
       
