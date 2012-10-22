.include beta.uasm

CMOVE(stack, SP) |; Initalise SP
BR(main)         |; Lance le programme

|; Aligne l'image sur 256 bits pour s'afficher correctement sur la 
|; visualisation de la VRAM.
.align 32 |; align <x> aligne sur un multiple de <x> bytes

image:
	.include images/pikachu.asm
	|; .include images/nyancat.asm
	|; .include images/BIGBOOBS.asm

x: LONG(10)
y: LONG(15)
 
main:
	LD(y, R0) PUSH(R0)
	LD(x, R0) PUSH(R0)
	CALL(floodfill)

	.breakpoint
	HALT()

|; MODC(Ra, C, Rc)       	| Rc = Ra % C = Ra - ((Ra / C) * C)
.macro MODC(Ra, C, Rc) 		DIVC(Ra, C, Rc) MULC(Rc, C, Rc) SUB(Ra, Rc, Rc)

Rx = R0
Ry = R1
Rh = R2
Rw = R3
Rcond = R4
Rindex = R4
Rbit = R5
Rcell = R6
Rpixel = R7
Rjump = R8

floodfill:
	PUSH(LP) PUSH(BP)
	MOVE(SP, BP)

	|; if y < 0 || y >= h then returns
		PUSH(Ry) PUSH(Rcond) |; Sauve Ry & RCond
		LD(BP, -16, Ry) |; Ry = y

		|; y < 0
		CMPLTC(Ry, 0, Rcond)
		BT(Rcond, left_border)

		|; y >= h => not (y < h)
		PUSH(Rh) LD(h, Rh) |; Rh
		CMPLT(Ry, Rh, Rcond)
		BF(Rcond, right_border)
	
	|; if isblack (x, y) then returns else setblack (x, y)
		PUSH(Rx) LD(BP, -12, Rx) |; Rx = x
		PUSH(Rbit) PUSH(Rcell) PUSH(Rpixel) PUSH(Rjump)

		LD(w, Rw) |; Rw ecrase Rh, qui n'est plus utilise dans la fonction
		
		BR(set_black_if_not, Rjump) |; Rpixel vaut != 0 si le pixel etait deja noir

		BT(Rpixel, is_already_black) 


|; Pseudo-procedure qui met le pixel (Rx, Ry) a 1 s'il ne l'est pas deja.
|; Evite ainsi de recalculer deux fois les coordonnees si les fonctions
|; isblack() et setblack() etait implementees separement.
|; Place une valeur non nulle dans Rpixel si le pixel etait a 1.
|; Modifie les valeurs de Rbit, Rindex et Rcell et jumpe sur Rjump.
set_black_if_not:
	|; Calcule l'index de la cellule de 32 bits
	|; Rindex = (Ry * Rw + Rx) / 32 :
		MUL(Ry, Rw, Rindex) ADD(Rindex, Rx, Rindex)
		DIVC(Rindex, 32, Rindex)

	|; Charge la cellule de 32 bits
	LD(Rindex, image, Rcell) |; Rcell = image[Rindex]

	|; Extrait le pixel de la cellule de 32 bits
	|; Rpixel = Rcell & (1 << (Rx % 32)) :
		MODC(Rx, 32, Rbit)  |; Rbit = Rx % 32
		ADD(R31, 1, Rpixel)   |; Rpixel = 1
		SHL(Rpixel, Rbit, Rbit) |; Rbit = 1 << (Rx % 32)
		AND(Rcell, Rbit, Rpixel)  |; Rpixel = Rcell & Rbit

	|; Rpixel != 0 => jump_set_black_if_not
	BT(Rpixel, jump_set_black_if_not)

	|; setblack (x, y)
		OR(Rcell, Rbit, Rcell) |; Rcell |= Rbit
		ST(Rcell, 0, Rindex)   |; image[Rindex] = Rcell

	|; Retourne a l'endroit ou la pseudo-procedure a ete appellee
	jump_set_black_if_not:	
		JMP(Rjump)

|; Branches de terminaison de la fonction floodfill
is_already_black:
	POP(Rjump) POP(Rpixel) POP(Rcell) POP(Rbit)
	POP(Rx)
	
right_border:
	POP(Rh) |; == POP(Rw)

left_border:
	POP(Rcond) |; == POP(Rindex)
	POP(Ry)
	POP(BP)	POP(LP)
	RTN()

LONG(0xdeadcafe)
stack:
	STORAGE(1024)