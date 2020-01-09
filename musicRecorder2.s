.equ PS2, 0xFF200100
.equ JTAG, 0xff201000
.equ TIMER, 0xFF202000
.equ AUDIO, 0xFF203040
.equ pixelBuffer, 0x08000000
.equ characterBuffer, 0x09000000
.equ time, 0x47868C0 #1.5 seconds

pianoImage:
.incbin "piano.bmp"

background:
.incbin "startimage.bmp"

#note frequencies
#number of highs/lows in each period
.equ noteC, 84
.equ noteD, 75
.equ noteE, 67
.equ noteF, 63
.equ noteG, 56
.equ noteA, 50
.equ noteB, 45
.equ noteC2, 42

#keyboard
.equ A, 0x1C
.equ S, 0x1B
.equ D, 0x23
.equ F, 0x2B
.equ G, 0x34
.equ H, 0x33
.equ J, 0x3B
.equ K, 0x42
.equ L, 0x4B
.equ P, 0x4D
.equ C, 0x21
.equ F0, 0xF0
.equ SPACE, 0x29


.section .data

noteArray: .word 0,0,0,0,0,0,0,0,0,0    #space for storing 5 notes
currentNote: .word 0            #ptr to current note in note array
numNotes: .word 0

gameState: .byte 0x00  #State of the game, 0 is start screen, 1 is game

subtitleArray: .byte 'P', 'r', 'e', 's', 's', ' ', 's', 'p', 'a', 'c', 'e', ' ', 't', 'o', ' ', 'c', 'o', 'n', 't', 'i', 'n', 'u', 'e', ' ', '.', '.', '.',  0
clearArray: .byte ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',  0

.section .exceptions, "ax"
.global .ISR
ISR:
    #save registers
    subi sp, sp, 56
    stw ra, 0(sp)
    stw r16, 4(sp)
    stw r17, 8(sp)
    stw r18, 12(sp)
    stw r19, 16(sp)
    stw r20, 20(sp)
    stw r4, 24(sp)
    stw ra, 28(sp)
    stw r2, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)
	stw r22, 44(sp)
	stw r23, 48(sp)
	stw r5, 52(sp)
    
    #make pointers
    movia r16, PS2      #ptr to ps2 keyboard
    movia r19, JTAG     #ptr to JTAG
    movia r20, AUDIO    #ptr to audio
    
    #check for keyboard interrupt
    rdctl et, ipending
    andi et, et, 0x80
    beq et, r0, exit #if IRQ7 (ps/2) is not pending, exit0   
    
keyboardInterrupt:

    ldwio r18, 0(r16) #load data from keyboard
    andi r17, r18, 0xFF #r17 = data bits (7-0)

;#get current location in note array
    movia et,currentNote
    ldw et,0(et)

check:
    ldwio r18, 4(r19)
    srli r18, r18, 16
    beq r18, r0, check ;#check if there is space available for writing

CompareKey:

	movia r18,gameState
   	ldbu r18,0(r18)
   	beq r18,r0, checkSpace

    movi r18, A
    beq r17, r18, A_KEY
    movi r18, S
    beq r17, r18, S_KEY
    movi r18, D
    beq r17, r18, D_KEY
    movi r18, F
    beq r17, r18, F_KEY
    movi r18, G
    beq r17, r18, G_KEY
    movi r18, H
    beq r17, r18, H_KEY
    movi r18, J
    beq r17, r18, J_KEY
    movi r18, K
    beq r17, r18, K_KEY
    movi r18, P
    beq r17, r18, P_KEY
    movi r18, C
    beq r17, r18, C_KEY
    movi r18, F0
    beq r17, r18, F0_KEY

checkSpace:
	movi r18, SPACE
    beq r17, r18, SPACE_KEY
    br exit

A_KEY:
 #write to JTAG terminal which key was pressed
    movi r17, 'C'
    stwio r17, 0(r19)
 #store note to noteArray
    movi r4, noteC
    stw r4,0(et)

	movi r3, 0
	movia r5, 0xf800
	call drawKeyTop1
	call drawKeyBottom
    br Play


S_KEY:
    movi r17, 'D'
    stwio r17, 0(r19)
    movi r4, noteD
    stw r4,0(et)

	movia r5, 0xfc00 
	movi r3, 40
	call drawKeyTop2
	movi r3, 33
	call drawKeyBottom
    br Play 

D_KEY:
    movi r17, 'E'
    stwio r17, 0(r19)
    movi r4, noteE
    stw r4,0(et)

	movia r5, 0xffe0
	movi r3, 73
	call drawKeyTop3
	movi r3, 65
	call drawKeyBottom
    br Play  

F_KEY:
    movi r17, 'F'
    stwio r17, 0(r19)
    movi r4, noteF
    stw r4,0(et)

	movia r5, 0x06a0
	movi r3, 97
	call drawKeyTop1
	call drawKeyBottom
    br Play 

G_KEY:
    movi r17, 'G'
    stwio r17, 0(r19)
    movi r4, noteG
    stw r4,0(et)

	movia r5, 0x041f
	movi r3, 137
	call drawKeyTop2
	movi r3, 129
	call drawKeyBottom
    br Play

H_KEY:
    movi r17, 'A'
    stwio r17, 0(r19)
    movi r4, noteA
    stw r4,0(et)

	movia r5, 0x99ff
	movi r3, 168
	call drawKeyTop2
	movi r3, 161
	call drawKeyBottom
    br Play

J_KEY:
    movi r17, 'B'
    stwio r17, 0(r19)
    movi r4, noteB
    stw r4,0(et)

	movia r5, 0xca37
	movi r3, 201
	call drawKeyTop3
	movi r3, 193
	call drawKeyBottom
    br Play

K_KEY:
    movi r17, 'C'
    stwio r17, 0(r19)
    movi r4, noteC2

	movia r5, 0xf800
	movi r3, 224
	call drawKeyTop1
	call drawKeyBottom
    stw r4,0(et)
    br Play

################ F0 Key ##################
F0_KEY:
    movi r17, 'X'
    stwio r17, 0(r19)
	movia r4, pianoImage
	call drawPicture

WaitForNextInterrupt:
    rdctl r17, ipending
    andi r17, r17, 0x80
    beq r17, r0, WaitForNextInterrupt 

ClearReadFIFO:
    ldwio r17,0(r16) #clearFIFO
    andi r17,r17,0xFF

    ;#if "P" or "C", go to exit, otherwise, go to done
    movi r21, P
    beq r17,r21, exit
    movi r21, C
    beq r17,r21, exit
	movi r21, SPACE
    beq r17,r21, exit
    br Record

############ playback key #################
P_KEY:
    movia et,numNotes
    ldw r21,0(et)       ;#r21:number of notes recorded
    movia r17,noteArray ;#r17: address of note to be played

PlayNext:
    ldw r4,0(r17)   ;#r4: note to be played
    ldw r18,4(r17)  ;#r18: number of cycles to be played

PlayNote:
    call PlayTone
    subi r18,r18,1
    bne r18,r0,PlayNote

    addi r17,r17,8
    subi r21,r21,1
    bne r21,r0,PlayNext

    br exit

############ Clear key #################
C_KEY:
    movia et,numNotes
    stw r0,0(et) ;#reset numNotes to 0

    movia et,currentNote
    movia r17,noteArray
    stw r17,0(et) ;#reset currentNote to the beginning of noteArray

    br exit

################ SPACE Key ##################
SPACE_KEY:
	movia r22,gameState
   	movi r23,1
    stb r23,0(r22)
	br exit
############################################

Play:  
    mov r18,r0

KeepPlaying:
    call PlayTone
    ; #check if key is released
    addi r18,r18,1 ;#count variable to keep track of length
    rdctl r17, ipending
    andi r17, r17, 0x80
    beq r17, r0, KeepPlaying ;#if no pending interrupt, keep playing

;#only reach this point if there is an interrupt
checkInterrupt:
    ldwio r17, 0(r16)   #read data from keyboard, clear one byte from FIFO
    andi r17, r17, 0xFF #r17 = which key
    movi r21, F0
    bne r17, r21, KeepPlaying    ;#if not F0, key is still pressed, keep playing the same note
    br F0_KEY

Record:
;#store length of note in noteArray
    stw r18,4(et) 

;#increment new note location by 8 bytes (2 words)
    addi et,et,8
    movia r18,currentNote
    stw et,0(r18)

;#increment number of notes
    movia r18,numNotes
    ldw et,0(r18)
    addi et,et,1
    stw et,0(r18)

;#print out current number of notes
    addi et,et,48
    stwio et, 0(r19)

exit:
    ldw ra, 0(sp)
    ldw r16, 4(sp)
    ldw r17, 8(sp)
    ldw r18, 12(sp)
    ldw r19, 16(sp)
    ldw r20, 20(sp)
    ldw r4, 24(sp)
    ldw ra, 28(sp)
    ldw r2, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	ldw r22, 44(sp)
	ldw r23, 48(sp)
	ldw r5, 52(sp)
    addi sp, sp, 56
    
    subi ea, ea, 4
    eret
    

##### SUBROUTINE TO PLAY TONE ######
#takes in argument r4: half period how many samples

PlayTone:
    addi sp, sp, -28
    stw r16, 0(sp)
    stw r17, 4(sp)
    stw r18, 8(sp)
    stw r19, 12(sp)
    stw r20, 16(sp)
    stw r21, 20(sp)
    stw et, 24(sp)
    
    movi r17, 1                 #play 1 periods
    movia r18, AUDIO            #ptr to audio core
    movia r19, 0xf0000000       #Audio sample value
    mov r20, r4                 #counter for number of samples
    
WaitForWriteSpace:
    ldwio r21, 4(r18)
    andhi r16, r21, 0xff00
    beq r16, r0, WaitForWriteSpace
    andhi r16, r21, 0xff
    beq r16, r0, WaitForWriteSpace
    
WriteTwoSamples:
    stwio r19, 8(r18)
    stwio r19, 12(r18)
    subi r20, r20, 1
    bne r20, r0, WaitForWriteSpace
    
HalfPeriodInvertWaveform:
    mov r20, r4
    sub r19, r0, r19                # 32-bit signed samples: Negate.
    subi r17, r17, 1
    bge r17, r0, WaitForWriteSpace


Stop:
    ldw r16, 0(sp)
    ldw r17, 4(sp)
    ldw r18, 8(sp)
    ldw r19, 12(sp)
    ldw r20, 16(sp)
    ldw r21, 20(sp)
    ldw et, 24(sp)
    addi sp, sp, 28
ret


############################################

.global _start
_start:
#interrupt initialization

    movia sp, 0x04000000 #iniatize stack ptr
    movia r9, PS2
    
    movi r8, 0x80 #bit7=1
    wrctl ctl3, r8 #enable IRQ7 for ps2
    
    movi r8, 0x1
    wrctl ctl0, r8 #set PIE=1
    
    stwio r8, 4(r9) #set read enable interrupt=1

 #array initialization
    movia r8, noteArray
    movia r9, currentNote
    stw r8,0(r9)

#draw background image
	movia r4, background
	call drawPicture


titleLOOP:
#write subtitle
	movia r4, subtitleArray 
	call drawSubtitle
	call timerSubroutine
	
	movia r4, clearArray 
	call drawSubtitle	
	call timerSubroutine

   	movia r7,gameState
   	ldbu r7,0(r7)
   	beq r7,r0, titleLOOP

	
	movia r4, clearArray
	call drawSubtitle

	movia r4, pianoImage
	call drawPicture
	 
loop: br loop

#######################################################################
#drawing subroutines

drawPicture:

	addi sp, sp, -48
	stw r7, 0(sp)
    stw r4, 4(sp)
    stw r5, 8(sp)
    stw r6, 12(sp)
    stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)
	stw r8, 44(sp)
    
   # movia r16, image
    addi r4, r4, 66
    
    movi r7, 0 #x counter
    movi r8, 0 #y counter
    movi r5, 0 #store colour of a pixel
    movia r6, pixelBuffer
    
    movi r17, 319 #x limit
    movi r18, 240 #y limit
    
#Subroutine to draw piano
imageLOOP:
    beq r8, r18, end #if reach y limit, done
    ldh r5, (r4) #load colour from image

	mov r19, r7 #x
	mov r20, r8 #y
	mov r21, r6 #buffer
	slli r19, r19, 1 # shift x value one to the left
	or r21, r21, r19 # or pixel buffer address with x value
	slli r20, r20, 10 # shift y value 10 to the left
	or r21, r21, r20 # or pixel buffer address with y value
	sthio r5, (r21) # write color into new pixel buffer address


    beq r7, r17, XDONE #if reach x limit, go to next row
    addi r7, r7, 1 #increment x counter by 1
    addi r4, r4, 2 #move to next pixel (halfword)
	br imageLOOP
        
XDONE: 
  	addi r8, r8, 1 #increase y counter by 1
    addi r4, r4, 2 #move to next pixel
    movi r7, 0 #reset x to 0
    br imageLOOP
        
end: 
	ldw r7, 0(sp)
    ldw r4, 4(sp)
    ldw r5, 8(sp)
    ldw r6, 12(sp)
    ldw r16, 16(sp)
    ldw r17, 20(sp)
    ldw r18, 24(sp)
    ldw r19, 28(sp)
    ldw r20, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	ldw r8, 44(sp)
	addi sp, sp, 48
    ret

#subroutine to draw key
drawKeyTop1:
	addi sp, sp, -44
	stw r7, 0(sp)
    stw r4, 4(sp)
    stw r5, 8(sp)
    stw r6, 12(sp)
	stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)

    movi r7, 0 #x counter
	add r7, r7, r3 #shift x starting value depending on argument

    movi r4, 0 #y counter
   # movia r5, 0xf800 #store colour of a pixel
    movia r6, pixelBuffer
    
    movi r17, 23 #x limit
	add r17, r17, r3 #shift x limit depending on argument

    movi r18, 137 #y limit
    
#Subroutine to draw piano
LOOP1:
    beq r4, r18, end1 #if reach y limit, done
	mov r19, r7 #x
	mov r20, r4 #y
	mov r21, r6 #buffer
	slli r19, r19, 1 # shift x value one to the left
	or r21, r21, r19 # or pixel buffer address with x value
	slli r20, r20, 10 # shift y value 10 to the left
	or r21, r21, r20 # or pixel buffer address with y value
	sthio r5, (r21) # write color into new pixel buffer address

    beq r7, r17, DONEX1 #if reach x limit, go to next row
    addi r7, r7, 1 #increment x counter by 1
	br LOOP1
        
DONEX1: 
  	addi r4, r4, 1 #increase y counter by 1
    movi r7, 0 #reset x to 0
	add r7, r7, r3
    br LOOP1
    
end1: 
	ldw r7, 0(sp)
    ldw r4, 4(sp)
    ldw r5, 8(sp)
    ldw r6, 12(sp)
    ldw r16, 16(sp)
    ldw r17, 20(sp)
    ldw r18, 24(sp)
    ldw r19, 28(sp)
    ldw r20, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	addi sp, sp, 44
    ret


drawKeyTop2:
	addi sp, sp, -44
	stw r7, 0(sp)
    stw r4, 4(sp)
    stw r5, 8(sp)
    stw r6, 12(sp)
	stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)

    movi r7, 0 #x counter
	add r7, r7, r3 #shift x starting value depending on argument

    movi r4, 0 #y counter
   # movia r5, 0xf800 #store colour of a pixel
    movia r6, pixelBuffer
    
    movi r17, 15 #x limit
	add r17, r17, r3 #shift x limit depending on argument

    movi r18, 137 #y limit
    
#Subroutine to draw piano
LOOP2:
    beq r4, r18, end2 #if reach y limit, done
	mov r19, r7 #x
	mov r20, r4 #y
	mov r21, r6 #buffer
	slli r19, r19, 1 # shift x value one to the left
	or r21, r21, r19 # or pixel buffer address with x value
	slli r20, r20, 10 # shift y value 10 to the left
	or r21, r21, r20 # or pixel buffer address with y value
	sthio r5, (r21) # write color into new pixel buffer address

    beq r7, r17, DONEX2 #if reach x limit, go to next row
    addi r7, r7, 1 #increment x counter by 1
	br LOOP2
        
DONEX2: 
  	addi r4, r4, 1 #increase y counter by 1
    movi r7, 0 #reset x to 0
	add r7, r7, r3
    br LOOP2
    
end2: 
	ldw r7, 0(sp)
    ldw r4, 4(sp)
    ldw r5, 8(sp)
    ldw r6, 12(sp)
    ldw r16, 16(sp)
    ldw r17, 20(sp)
    ldw r18, 24(sp)
    ldw r19, 28(sp)
    ldw r20, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	addi sp, sp, 44
    ret

#subroutine to draw key
drawKeyTop3:
	addi sp, sp, -44
	stw r7, 0(sp)
    stw r4, 4(sp)
    stw r5, 8(sp)
    stw r6, 12(sp)
	stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)

    movi r7, 0 #x counter
	add r7, r7, r3 #shift x starting value depending on argument

    movi r4, 0 #y counter
    #movia r5, 0xf800 #store colour of a pixel
    movia r6, pixelBuffer
    
    movi r17, 22 #x limit
	add r17, r17, r3 #shift x limit depending on argument

    movi r18, 137 #y limit
    
#Subroutine to draw piano
LOOP3:
    beq r4, r18, end3 #if reach y limit, done
	mov r19, r7 #x
	mov r20, r4 #y
	mov r21, r6 #buffer
	slli r19, r19, 1 # shift x value one to the left
	or r21, r21, r19 # or pixel buffer address with x value
	slli r20, r20, 10 # shift y value 10 to the left
	or r21, r21, r20 # or pixel buffer address with y value
	sthio r5, (r21) # write color into new pixel buffer address

    beq r7, r17, DONEX3 #if reach x limit, go to next row
    addi r7, r7, 1 #increment x counter by 1
	br LOOP3
        
DONEX3: 
  	addi r4, r4, 1 #increase y counter by 1
    movi r7, 0 #reset x to 0
	add r7, r7, r3
    br LOOP3
    
end3: 
	ldw r7, 0(sp)
    ldw r4, 4(sp)
    ldw r5, 8(sp)
    ldw r6, 12(sp)
    ldw r16, 16(sp)
    ldw r17, 20(sp)
    ldw r18, 24(sp)
    ldw r19, 28(sp)
    ldw r20, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	addi sp, sp, 44
    ret



#subroutine to draw bottom of key
drawKeyBottom:
	addi sp, sp, -44
	stw r7, 0(sp)
    stw r4, 4(sp)
    stw r5, 8(sp)
    stw r6, 12(sp)
	stw r16, 16(sp)
    stw r17, 20(sp)
    stw r18, 24(sp)
    stw r19, 28(sp)
    stw r20, 32(sp)
    stw r21, 36(sp)
	stw r3, 40(sp)

    movi r7, 0 #x counter
	add r7, r7, r3 #shift x starting value depending on argument

    movi r4, 137 #y counter
    #movia r5, 0xf800 #store colour of a pixel
    movia r6, pixelBuffer
    
    movi r17, 30 #x limit
	add r17, r17, r3 #shift x limit depending on argument

    movi r18, 239 #y limit
    
#Subroutine to draw piano
LOOP_BOTTOM:
    beq r4, r18, endBottom #if reach y limit, done
	mov r19, r7 #x
	mov r20, r4 #y
	mov r21, r6 #buffer
	slli r19, r19, 1 # shift x value one to the left
	or r21, r21, r19 # or pixel buffer address with x value
	slli r20, r20, 10 # shift y value 10 to the left
	or r21, r21, r20 # or pixel buffer address with y value
	sthio r5, (r21) # write color into new pixel buffer address

    beq r7, r17, DONE_BOTTOMX #if reach x limit, go to next row
    addi r7, r7, 1 #increment x counter by 1
	br LOOP_BOTTOM
        
DONE_BOTTOMX: 
  	addi r4, r4, 1 #increase y counter by 1
    movi r7, 0 #reset x to 0
	add r7, r7, r3
    br LOOP_BOTTOM
    
endBottom: 
	ldw r7, 0(sp)
    ldw r4, 4(sp)
    ldw r5, 8(sp)
    ldw r6, 12(sp)
    ldw r16, 16(sp)
    ldw r17, 20(sp)
    ldw r18, 24(sp)
    ldw r19, 28(sp)
    ldw r20, 32(sp)
    ldw r21, 36(sp)
	ldw r3, 40(sp)
	addi sp, sp, 44
    ret

##################################################
#Subroutine to write subtitle

drawSubtitle:
	#r4 is the ptr to character array
	addi sp, sp, -16
	stw r5, 0(sp)
	stw r6, 4(sp)
	stw r7, 8(sp)
	stw r4, 12(sp)

	movia r5, characterBuffer
	addi r5, r5, 1960

drawSubtitleLoop:
	ldb r7, 0(r4)
	beq r7, r0, endSubtitle
	stbio r7, 0(r5)
	addi r4, r4, 1
	addi r5, r5, 1
	br drawSubtitleLoop

endSubtitle:
	ldw r5, 0(sp)
	ldw r6, 4(sp)
	ldw r7, 8(sp)
	ldw r4, 12(sp)
	addi sp, sp, 16
	ret

##################################################
#1.5 sec timer subroutine
timerSubroutine:
	addi sp, sp, -8
	stw r8, 0(sp)
	stw r9, 4(sp)

	movia r8, TIMER
	movui r9, %lo(time)
	stwio r9, 8(r8)
	movui r9, %hi(time)
	stwio r9, 12(r8)
	stwio r0, 0(r8)
	movui r9, 0b0100
	stwio r9, 4(r8)
poll: 
	ldwio r9, 0(r8)
	andi r9, r9, 0b1
	beq r9, r0, poll

	ldw r8, 0(sp)
	ldw r9, 4(sp)
	addi sp, sp, 8
	ret
##################################################



