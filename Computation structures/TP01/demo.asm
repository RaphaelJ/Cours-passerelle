.include beta.uasm

CMOVE(stack, SP) 				|; Initalise SP
BR(main)         				|; Lance le programme

|; Aligne l'image sur 256 bits pour s'afficher correctement sur la 
|; visualisation de la VRAM.
.align 32 |; align <x> aligne sur un multiple de <x> bytes

mon_image:
	|; Quelques images avec des positions initiales pour le floodfill.
	|; IMW et IMH sont definis dans les fichiers .asm des images.
	
	.include images/pikachu.asm
	x: LONG(40) 			|; Colorie la queue
	y: LONG(19)	
	
	|; .include images/nyancat.asm
	|; x: LONG(80) 			|; Colorie la tete
	|; y : LONG(25)

	|; .include images/BIGBOOBS.asm
	|; x: LONG(20) 			|; Colorie le dernier L
	|; y: LONG(12)
 
	|; .include images/java.asm
	|; x: LONG(50) 			|; Colorie les dents
	|; y: LONG(50)

main:
	|; Pour verifier que les registres sont correctement reinitialises :
	CMOVE(0x1, R1)	CMOVE(0x6, R6)
	CMOVE(0x2, R2)	CMOVE(0x7, R7)
	CMOVE(0x3, R3)	CMOVE(0x8, R8)
	CMOVE(0x4, R4)	CMOVE(0x9, R9)
	CMOVE(0x5, R5)	CMOVE(0x10, R10)

	|; Initialise les variables globales image, IMW et IMH
	CMOVE(mon_image, R0) ST(R0, image)
	LD(w, R0) ST(R0, IMW)
	LD(h, R0) ST(R0, IMH)

	LD(y, R0) PUSH(R0)
	LD(x, R0) PUSH(R0)
	CALL(floodfill, 2)

	HALT()

.include floodfill.asm

LONG(0xdeadcafe)
stack:
	STORAGE(1024)