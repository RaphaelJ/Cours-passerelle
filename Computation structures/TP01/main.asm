.include beta.uasm

CMOVE(stack, SP) 				|; Initalise SP
BR(main)         				|; Lance le programme

|; Aligne l'image sur 256 bits pour s'afficher correctement sur la 
|; visualisation de la VRAM.
.align 32 |; align <x> aligne sur un multiple de <x> bytes

image:
	.include images/pikachu.asm
	|; .include images/nyancat.asm
	|; .include images/BIGBOOBS.asm

x: LONG(0)
y: LONG(1)
 
main:
	LD(x, R0) PUSH(R0)
	LD(y, R0) PUSH(R0)
	CALL(floodfill)

	.breakpoint
	HALT()

|; MODC(Ra, C, Rc)       			|; Rc = Ra % C = Ra - ((Ra / C) * C)
.macro MODC(Ra, C, Rc) 		DIVC(Ra, C, Rc) MULC(Rc, C, Rc) SUB(Ra, Rc, Rc)

Rx = R1
Ry = R0
Rh = R2
Rw = R3
Rcond = R4
Rindex = R4
Rbit = R5
Rcell = R6
Rpixel = R7
Rjump = R8
Rright = R9
Rleft = R10
Ry2 = R11

floodfill:
	PUSH(LP) PUSH(BP)
	MOVE(SP, BP)

	|; if y < 0 || y >= h then returns
		PUSH(Rcond)	 		|; Ne sauve pas Ry qui est r0
		LD(BP, -12, Ry) 		|; Ry = y

		|; y < 0
		CMPLTC(Ry, 0, Rcond)
		BT(Rcond, top_border)

		|; y >= h => not (y < h)
		PUSH(Rh) LD(IMH, Rh) 		|; Rh = IMH
		CMPLT(Ry, Rh, Rcond)
		BF(Rcond, bottom_border)
	
	|; if isblack (x, y) then returns else setblack (x, y)
		PUSH(Rx) LD(BP, -16, Rx) 	|; Rx = x
		PUSH(Rbit) PUSH(Rcell) PUSH(Rpixel) PUSH(Rjump)

		LD(IMW, Rw) 			|; Rw ecrase Rh, qui n'est plus utilise dans la fonction
		
		BR(set_black_if_not, Rjump) 	|; Rpixel vaut != 0 si le pixel etait deja noir

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
		PUSH(Rright) 
		MOVE(Rx, Rright) 		|; Rright = Rx
		LD(BP, -16, Rx) 		|; Rx = x

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
		|; Place la valeur de Rx dans Rleft. 
		|; Rleft contient le premier pixel non modifie a gauche
		PUSH(Rleft) 
		MOVE(Rx, Rleft) 		|; Rleft = Rx
	
	|; Explore les pixels en y-1 et y+1 avec x dans ]Rleft; Rright[.
	|; Contrairement au code de l'ennonce, il n'est pas necessaire de visiter 
	|; Rleft et Rright etant donne qu'ils sont soit hors de l'image, soit noirs.
	|; De plus, les deux boucles de l'ennonce iterent recursent deux fois sur 
	|; (x, y-1) et (x, y+1).
	MOVE(Rx, Rleft)
	SUBC(Ry, 1, Ry) 			|; Ry = y - 1
	PUSH(Ry2) ADDC(Ry, 2, Ry2) 		|; Ry2 = y + 1
	explore_verticaly:
		ADDC(Rx, 1, Rx) 		|; Rx++

		CMPLE(Rx, Rright, Rcond) 	|; Stops if Rx >= Rright  
		BF(Rcond, done)
		
		PUSH(Rx)

		|; floodfill(Rx, y-1)
		PUSH(Ry)
		CALL(floodfill)
		DEALLOCATE(1) 			|; Libere Ry
		|; floodfill(Rx, y+1)
		PUSH(Ry2)
		CALL(floodfill)
		DEALLOCATE(2) 			|; Libere Ry et Rx

		BR(explore_verticaly)

|; Branches de terminaison de la fonction floodfill
done:
	POP(Ry2) POP(Rleft) POP(Rright)

is_already_black:
	POP(Rjump) POP(Rpixel) POP(Rcell) POP(Rbit)
	POP(Rx) 
	
bottom_border:
	POP(Rh) 				|; == POP(Rw)

top_border:
	POP(Rcond) 				|; == POP(Rindex)
	|; POP(Ry) == R0 => pas besoin de restaurer
	POP(BP)	POP(LP)
	RTN()

|; Pseudo-procedure qui met le pixel (Rx, Ry) a 1 s'il ne l'est pas deja.
|; Evite ainsi de recalculer deux fois les coordonnees si les fonctions
|; isblack() et setblack() etait implementees separement.
|; Place une valeur non nulle dans Rpixel si le pixel etait a 1.
|; Modifie les valeurs de Rbit, Rindex et Rcell et jumpe sur Rjump.
set_black_if_not:
	|; Calcule l'index de la cellule de 32 bits
	|; Rindex = (Ry * Rw + Rx) / 4 :
		MUL(Ry, Rw, Rindex) ADD(Rindex, Rx, Rindex)
		DIVC(Rindex, 4, Rindex)

	|; Charge la cellule de 32 bits
	LD(image+8*4, Rcell) 
	.breakpoint
	LD(Rindex, image, Rcell) 		|; Rcell = image[Rindex]
	.breakpoint

	|; Extrait le pixel de la cellule de 32 bits
	|; Rpixel = Rcell & (1 << (Rx % 32)) :
		MODC(Rx, 32, Rbit)  		|; Rbit = Rx % 32
		ADDC(R31, 1, Rpixel)   		|; Rpixel = 1
		SHL(Rpixel, Rbit, Rbit) 	|; Rbit = 1 << (Rx % 32)
		AND(Rcell, Rbit, Rpixel)  	|; Rpixel = Rcell & Rbit

	|; Rpixel != 0 => jump_set_black_if_not
	BT(Rpixel, jump_set_black_if_not)

	|; setblack (x, y)
		OR(Rcell, Rbit, Rcell) 		|; Rcell |= Rbit
		ST(Rcell, image, Rindex)   	|; image[Rindex] = Rcell

	|; Retourne a l'endroit ou la pseudo-procedure a ete appellee
	jump_set_black_if_not:	
		JMP(Rjump)

LONG(0xdeadcafe)
stack:
	STORAGE(1024)