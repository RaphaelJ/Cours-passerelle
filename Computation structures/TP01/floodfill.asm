|; Variables globales
image: STORAGE(1)
IMW: STORAGE(1)
IMH: STORAGE(1)

|; Alias aux registres
Ry = R1			|; Ordonnee
Rx = R3			|; Abscisse
Rh = R2			|; Hauteur de l'image 
Rw = R2			|; Largeur de l'image (ecrase Rh)
Rcond = R0		|; Utilise pour les tests et conditions
Rindex = R0		|; Indice de la cellule de 32 bits qui contient le pixel
Rimage = R9		|; Valeur du pointeur image
Rbit = R4 		|; Numero du bit dans la cellule de 32 bits
Rcell = R5		|; Contenu de la cellule de 32 bits
Rpixel = R6		|; Contenu du bit dans la cellule de 32 bits
Rjump = R7		|; Addresse de retour pour la pseudo procedure set_black_if_not
Rright = R8		|; Indice du premier pixel noir a droite
Ry2 = R7		|; Ry2 = y + 1 pour l'iteration des cas recursifs (== Rjump)

floodfill:
	PUSH(LP) PUSH(BP)
	MOVE(SP, BP)

	|; if y < 0 || y >= h then returns
		PUSH(Ry)
		LD(BP, -16, Ry) 		|; Ry = y

		|; y < 0
		CMPLTC(Ry, 0, Rcond) 		|; Ne sauve pas Rcond qui est R0
		BT(Rcond, top_border)

		|; y >= h => not (y < h)
		PUSH(Rh) LD(IMH, Rh) 		|; Rh = IMH
		CMPLT(Ry, Rh, Rcond)
		BF(Rcond, bottom_border)
	
	|; if isblack (x, y) then returns else setblack (x, y)
		PUSH(Rx) LD(BP, -12, Rx) 	|; Rx = x
		PUSH(Rimage) PUSH(Rbit) PUSH(Rcell) PUSH(Rpixel) PUSH(Rjump)

		LD(IMW, Rw) 			|; Rw ecrase Rh, qui n'est plus utilise dans la fonction

		|; Lit l'adresse de l'image
		LD(image, Rimage)
		
		BR(set_black_if_not, Rjump)	|; Rpixel vaut != 0 si le pixel etait deja noir

		BT(Rpixel, is_already_black) 	|; Retourne si deja noir


	|; Explore horizontalement, vers la droite
	explore_right:
		|; Itere avec Rx (a cause de la procedure set_black_if_not)
		ADDC(Rx, 1, Rx) 		|; Rx++;

		CMPLT(Rx, Rw, Rcond) 		|; stops if Rx >= Rw
		BF(Rcond, explore_right_done)

		BR(set_black_if_not, Rjump) 	|; Rpixel vaut != 0 si le pixel etait deja noir

		BT(Rpixel, explore_right_done) 	|; Stoppe si deja noir
		BR(explore_right) 		|; Sinon, passe au pixel de droite
	explore_right_done:
		|; Place la valeur de Rx dans Rright et recupere la valeur originale de Rx
		|; Rright contient le premier pixel non modifie a droite
		PUSH(Rright) MOVE(Rx, Rright) 	|; Rright = Rx
		LD(BP, -12, Rx) 		|; Rx = x

	|; Explore horizontalement, vers la gauche.
	explore_left:
		|; Itere avec Rx (a cause de la procedure set_black_if_not)
		SUBC(Rx, 1, Rx) 		|; Rx--;

		CMPLTC(Rx, 0, Rcond) 		|; stops if Rx < 0
		BT(Rcond, explore_left_done)

		BR(set_black_if_not, Rjump) 	|; Rpixel vaut != 0 si le pixel etait deja noir

		BT(Rpixel, explore_left_done) 	|; Stoppe si deja noir
		BR(explore_left) 		|; Sinon, passe au pixel de droite
	explore_left_done:

	|; Rx contient le premier pixel non modifie a gauche
	
	|; Explore les pixels en y-1 et y+1 avec x dans ]Rx; Rright[.
	|; Contrairement au code de l'ennonce, il n'est pas necessaire de visiter 
	|; Rx et Rright etant donne qu'ils sont soit hors de l'image, soit noirs.
	|; De plus, les deux boucles de l'ennonce iterent recursent deux fois sur 
	|; (x, y-1) et (x, y+1), la boucle suivante ne visite qu'une fois ce point.
	SUBC(Ry, 1, Ry) 			|; Ry  = y - 1
	ADDC(Ry, 2, Ry2) 		|; Ry2 = y + 1
	explore_verticaly:
		ADDC(Rx, 1, Rx) 		|; Rx++

		CMPLT(Rx, Rright, Rcond) 	|; Stops if Rx >= Rright  
		BF(Rcond, done)

		|; floodfill(Rx, y-1)
		PUSH(Ry)
		PUSH(Rx)
		CALL(floodfill, 2)

		|; floodfill(Rx, y+1)
		PUSH(Ry2)
		PUSH(Rx)
		CALL(floodfill, 2)

		BR(explore_verticaly)

|; Branches de terminaison de la fonction floodfill
done:
	POP(Rright)

is_already_black:
	POP(Rjump) 				|; == POP(Ry2)
	POP(Rpixel) 
	POP(Rcell) POP(Rbit)
	POP(Rimage) POP(Rx) 
	
bottom_border:
	POP(Rh) 				|; == POP(Rw)

top_border:
	|; POP(Rcond) == POP(RIndex) == R0 => pas besoin de restaurer
	POP(Ry)
	POP(BP)	POP(LP)
	RTN()

|; Pseudo-procedure qui met le pixel (Rx, Ry) a 1 s'il ne l'est pas deja.
|; Evite ainsi de recalculer deux fois les coordonnees d'un meme point si les
|; fonctions isblack() et setblack() etait implementees separement.
|; Place une valeur non nulle dans Rpixel si le pixel etait a 1.
|; Modifie les valeurs de Rbit, Rindex et Rcell et jumpe sur Rjump.
set_black_if_not:
	|; Calcule l'index de la cellule de 32 bits
	|; Rindex = (Ry * Rw + Rx) / 8 :
		MUL(Ry, Rw, Rindex) ADD(Rindex, Rx, Rindex)
		SHRC(Rindex, 3, Rindex)		|; == DIVC(Rindex, 8, Rindex)

	|; Charge la cellule de 32 bits
	ADD(Rindex, Rimage, Rindex)		|; Rcell = image[Rindex]
	LD(Rindex, 0, Rcell) 			|; Rindex & OxFFFFFF00 inutile

	|; Extrait le pixel de la cellule de 32 bits
	|; Rpixel = Rcell & (1 << (Rx % 32)) :
		ANDC(Rx, 0x1F, Rbit)  		|; == MODC(Rx, 32, Rbit) == Rbit = Rx % 32
		ADDC(R31, 1, Rpixel)   		|; Rpixel = 1
		SHL(Rpixel, Rbit, Rbit) 	|; Rbit = 1 << (Rx % 32)
		AND(Rcell, Rbit, Rpixel)  	|; Rpixel = Rcell & Rbit

	|; Rpixel != 0 => jump_set_black_if_not
	BT(Rpixel, jump_set_black_if_not)

	|; setblack (x, y)
		OR(Rcell, Rbit, Rcell) 		|; Rcell |= Rbit
		ST(Rcell, 0, Rindex)   		|; image[Rindex] = Rcell

	|; Retourne a l'endroit ou la pseudo-procedure a ete appellee
	jump_set_black_if_not:	
		JMP(Rjump)