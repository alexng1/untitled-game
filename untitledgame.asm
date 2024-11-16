org 38000

; Setup the 128 entry vector table
di
              ld            hl, IM2Table
              ld            de, IM2Table+1
              ld            bc, 256
              ; Setup the I register (the high byte of the table)
              ld            a, h
              ld            i, a
              ; Set the first entries in the table to $FC
				; The interrupt routine address will be FCFC
              ld            a, $FC
              ld            (hl), a
              ; Copy to all the remaining 256 bytes
              ldir
              ; Setup IM2 mode
              im            2
ei

            ld hl,font-256 ; new font address = initial font data address minus 32*8.
            ld (23606),hl   ; POKE 23606 with new font address to set new font
startGame
			ld a,2 ; upper screen
			call 5633; open channel

	xor a               ; quick way to load accumulator with zero.
	call 8859           ; set permanent border colours

; put splash image on screen
	ld hl,screen
	ld de,16384
	ld bc,6912
	ldir

; reset dummy screen
	ld b,48
	ld de,24576
	xor a
l2	push bc
	ld b,128
l1	ld (de),a
	inc de
	djnz l1
	pop bc
	djnz l2
	

; put instructions on screen
ld de,instructions0
ld bc,eoinstructions0-instructions0
call 8252
ld de,instructions1
ld bc,eoinstructions1-instructions1
call 8252
ld de,instructions2
ld bc,eoinstructions2-instructions2
call 8252

; play intro music loop - ends on key press
ld a,1
ld (musicloop),a
ld (songid),a
call nanoinit

; set random number seed
ld a,(23672)		; current timer
ld (seed),a			; set first byte of random seed

; do screen fade effect
			ld d,0
scrfades	ld hl,16384
			ld b,128
scrfade0	push bc
scrfade		ld b,54
scrfade1	ld a,d
			cp 0
			jr nz,scrfade2 
			ld a,(hl)
			and 63		;00111111 ... shift out the picture gradually
			ld (hl),a
			jr scrfadex	
scrfade2	cp 1
			jr nz,scrfade3
			ld a,(hl)
			and 15		;00001111
			ld (hl),a
			jr scrfadex
scrfade3	cp 2
			jr nz,scrfade4
			ld a,(hl)
			and 3		;00000011
			ld (hl),a
			jr scrfadex
scrfade4	ld (hl),0
scrfadex	inc hl
			djnz scrfade1
			pop bc
			djnz scrfade0
			halt
			halt
			inc d
			ld a,d
			cp 4
			jr nz,scrfades

    ld a,pipecolour
    ld (23693),a ; set permanent colours
    call 3503 ; clear screen

; draw background image tiles near bottom of screen

		ld b,11 ; 11 rows, from row 11 to row 22
drawrow push bc
		ld a,b
		add a,11 ; add offset
		ld c,a
		ld b,30 ; each row has 30 columns, from column 1 to column 30
drawbg push bc
		ld a,b
		ld b,c
		ld c,a
		ld hl,bgimg
		call char
		pop bc
		djnz drawbg
		pop bc
		djnz drawrow

; set colour attributes for main play area
; The ball colour will be the same colour:
; colour attributes start at 22528 with 768 bytes
; 1st attr addr for main area = 22528 +32 +1 = 22561
; main play area has cyan ink, black paper (69)
		ld de,32;   bytes per row

		ld hl,22561
		ld b,22
colourrow	push bc
		ld b,30
		push hl
colourbg ld (hl),bgcolour
		inc hl
		djnz colourbg
		pop hl
		add hl,de    ; go to next row
		pop bc
		djnz colourrow

; bat movement area has red ink, black paper (66)
; 1st attr addr for this = 22528+ (32*23) + 1 = 23265
		ld hl,23265
		ld b,30
colourbg3 ld (hl),66
		inc hl
		djnz colourbg3


; draw pipe borders
        ld b,32
thpipe1  push bc   ; top horizontal pipe
        ld c,b
        ld b,0
        ld hl,hpipe
        call char
        pop bc
        djnz thpipe1

        ld b,23
lvpipe1  push bc    ; left vertical pipe
        ld c,0
        ld hl,vpipe
        call char
        pop bc
        push bc
rvpipe1  ld c,15      ; right vertical pipe
        ld hl,vpipe
        call char
        pop bc
        djnz lvpipe1

lcpiped1 ld bc,0     ; b=0, c=0 left corner pipe block
        ld hl,lcpipe
        call char
rcpiped1 ld bc,15   ; b=0, c=15  right corner pipe block
        ld hl,rcpipe
        call char

        ld b,23
lvpipe2  push bc
        ld c,16
        ld hl,vpipe
        call char
        pop bc
        push bc
rvpipe2  ld c,31
        ld hl,vpipe
        call char
        pop bc
        djnz lvpipe2

lcpiped2 ld bc,16     ; b=0, c=16
        ld hl,lcpipe
        call char
rcpiped2 ld bc,31   ; b=0, c=31
        ld hl,rcpipe
        call char

; set colour for central two columns
; 1st attr addr for this = 22528 + 32 + 15
		ld de,32
		ld hl,22575
		ld b,23
colourrow2 push bc
		ld b,2
		push hl
colourbg2 ld (hl),pipecolour
		inc hl
		djnz colourbg2
		pop hl
		add hl,de
		pop bc
		djnz colourrow2

; initialise game data
		ld hl,initdata
		ld bc,initdataend-initdata
		ld de,balldata
		ldir

		ld hl,initbatdata
		ld bc,initbatdataend-initbatdata
		ld de,batdata
		ldir

		xor a
		ld (gameover),a

; randomise ball starting position and direction
push iy   ; using iy has a side effect on ROM calls so preserve its value
	ld ix,initdata
	ld iy,balldata
	ld b,2
rndball	ld d,(ix+2)		; default horizontal coordinate
		call random
		and 31			; restrict random number between 0 to 31
		add a,d
		ld (iy+2),a
		and 1			; restrict random number between 0 and 1
		ld (iy),a		; set random direction
		ld de,initdataend-initd2	; fixed number of data bytes for each ball
		add ix,de
		add iy,de
		djnz rndball
pop iy

; draw the initial bat images on screen
	ld ix,batdata
	ld b,2
bat0	push bc
		call processBat
		pop bc
		ld de,initbatdataend-initbat2
		add ix,de
		djnz bat0

; play pre-game-start music
		call playSecondSong

; this loop uses the ix register so make sure all sub calls
; do not corrupt its value, e.g. the beep calls
mloop equ $

		ld ix,balldata  ; ball data table
		ld b,2    ; number of balls
ball	push bc
		ld a,(ix) ; should ball appear?
		inc a     ; 255 = not display, increments to zero
		call nz,processBall
		pop bc
		ld de,initdataend-initd2   ; fixed number of bytes of data for each ball
		add ix,de ; get next ball
		djnz ball

		ld ix,batdata
		ld b,2		; number of bats
bat		push bc
		call processBat
		pop bc
		ld de,initbatdataend-initbat2
		add ix,de
		djnz bat

		ld a,(gameover)
		cp 2
		jp z,endGame

		call checkKeyPresses

		ld a,0
		ld (dowait),a

		halt
		jp mloop

beep    ld hl,850
		ld de,3
		; beep will affect ix so save it on stack first
		; also changing iy has a side effect on ROM calls so preserve it, too
		push ix
		push iy
		call 949
		pop iy
		pop ix
		ret

playSecondSong xor a
				ld (musicloop),a
				ld a,2
				ld (songid),a
				call nanoinit
				ret

endGame ld b,8
		ld hl, 22528 ; turn all graphics white on screen
whiteInk1 push bc
		ld b,96
whiteInk ld (hl),7
		inc hl	
		djnz  whiteInk
		pop bc
		djnz whiteInk1

		call playSecondSong

		ld b,7
		ld d,128
		
fade3   push bc
		ld hl,16384
		ld a,d
		cpl
		ld e,a
		ld b,48
fade2	push bc
		ld b,128
fade1	ld a,(hl)
		and e
		ld (hl),a
		inc hl
		djnz fade1
		pop bc
		djnz fade2
		rr d
		halt
		halt
		pop bc
		djnz fade3

		jp startGame
		

checkKeyPresses ld bc,63486		; keyboard row 1-5 / joystick port 2
				in a,(c)
				rra
				push af
				call nc,mlbat1	; key 1: move bat 1 to the left
				pop af
				rra
				push af
				call nc,mrbat1	; key 2: move bat 1 to the right
				pop af
				rra
				push af
				call nc,mlbat2	; key 3: move bat 2 to the left
				pop af
				rra
				call nc,mrbat2	; key 4: move bat 2 to the right
				ret

mlbat1	ld ix,batdata
		call mlbat
		ret
mrbat1	ld ix,batdata
		ld a,(ix)
		cp 0
		call z, mrbat
		ret
mlbat2	ld ix,batdata+initbatdataend-initbat2
		call mlbat
		ret
mrbat2	ld ix,batdata+initbatdataend-initbat2
		ld a,(ix)
		cp 0
		call z, mrbat
		ret
mlbat	ld a,(ix+2)
		cp (ix+3)
		ret z
		ld (ix),1			; go left
		ret
mrbat	ld a,(ix+2)
		cp (ix+4)
		ret z
		ld (ix),2			; go right
		ret

processBat ld hl,batimg
			ld a,(ix)
			cp 1
			jr nz, batRightChk
			dec (ix+2)       ; moving left
			dec (ix+5)
			dec (ix+6)
			jr batChkEnd
batRightChk	cp 2
			jr nz, batChkEnd	
			inc (ix+2)       ; moving right
			inc (ix+6)
			inc (ix+5)
			; even if the bat is not moving, we want to do the work
			; of drawing the sprite so as not to speed up the game
batChkEnd	call batsprite
			ld (ix),0   ; reset move flag
			ret

; we'll dynamically shift the sprite 
batsprit7	xor 7	; complement last 3 bits
			inc a	; add one for luck
batsprit3	rl d	; rotate left...
			rl c	; ... into middle byte ...
			rl e	; ... and finally into left character cell.
			dec a	; count shifts we've done.
			jr nz,batsprit3

	; Line of sprite image is now in e+c+d, we need it in form c+d+e

			ld a,e
			ld e,d
			ld d,c
			ld c,a
			jr batsprit0

batsprite ld a,(ix+1)	; vertical pos
			ld (battmp1),a
			call scadd
			ld a,8   ; height of sprite in pixels.
batsprit1	ex af,af'	; store loop counter
			push de		; store screen address
			ld c,(hl)	; first sprite graphic
			inc hl		; next sprite byte
			ld d,(hl)
			inc hl		; next row of sprite graphic
			ld (battmp0),hl
			ld e,0		; blank right byte for now.
			ld a,(ix+2)	; horiz pos
			and 7		; are we straddling character cells?
			jr z,batsprit0	; we are not,don't bother shifting
			cp 5		; 5 or more shifts needed?
			jr nc,batsprit7	; yes shift from left as it's quicker
			and a			; clear carry flag
batsprit2	rr c	; rotate left byte right...
			rr d	; through middle byte...
			rr e	; into right byte.
			dec a
			jr nz,batsprit2
batsprit0	pop hl			; pop screen address from stack
			ld a,(ix)
			cp 0
			jr z, skipDraw	; if not moving, don't draw bat
			set 5,h			; jump to dummy screen
			ld a,(hl)		; what's there already.
			or c			; merge in image data
			res 5,h			; jump back to physical screen
			ld (hl),a		; place onto screen.
			inc l			; next character cell to the right
			set 5,h			; do the same for the other 2 blocks
			ld a,(hl)
			or d
			res 5,h
			ld (hl),a
			inc hl
			set 5,h
			ld a,(hl)
			or e
			res 5,h
			ld (hl),a
skipDraw	ld a,(battmp1)	; temporary vertical coord
			inc a			; next pixel line down.
			ld (battmp1),a
			dec hl			; go left 2 bytes
			dec l			; not straddling 256-byte boundary here.
			inc h			; next row of this character cell.
batsprit6	ex de,hl		; put screen address in de.
			ld hl,(battmp0)	; restore graphic address
			ex af,af'		; restore loop counter
			dec a
			jp nz,batsprit1
			ret
			

processBall ld a,(ix+2) ; horizontal position - see which preshifted frame we need to show
			and 7       ; returns 0 to 7
			rla         ; multiply by 32 to get offset to add to hl for the beginning of the correct preshifted frame
			rla
			rla
			rla
			rla
			ld c,a
			ld b,0
			ld hl,preshiftedBall
			add hl,bc
			call sprite

			; see which direction the ball is moving and update coordinates
			call chkBallDirection
			call moveBall

			ret

moveBall ld a,(ix)
		cp 0      ; DL (down left)
		jr nz,moveBall1
		inc (ix+1)
		dec (ix+2)
		ret
moveBall1 ld a,(ix)
			cp 1; DR (down right)
			jr nz,moveBall2
			inc (ix+1)
			inc (ix+2)
			ret
moveBall2 ld a,(ix)
			cp 2; UL (up left)
			jr nz,moveBall3
			dec (ix+1)
			dec (ix+2)
			ret
moveBall3 ld a,(ix)
			cp 3; UR (up right)
			ret nz
			dec (ix+1)
			inc (ix+2)
			ret

; see if the ball needs to change direction
chkBallDirection ld a,(ix)
		cp 0
		call z,chkDL
		ld a,(ix)
		cp 1
		call z,chkDR
		ld a,(ix)
		cp 2
		call z,chkUL
		ld a,(ix)
		cp 3
		call z,chkUR
		ret

checkSafe ld a,(ix+2) ; horizontal coord
        ld l,(ix+9)
        ld h,(ix+10)
        cp (hl) ; left safe reach
        jr nc, checkRightSafe ; horiz coord >= left safe reach
        ld a,1
        ld (gameover),a
        ret
checkRightSafe ld a,(ix+2)
				ld l,(ix+11)
				ld h,(ix+12)
                cp (hl) ; right safe reach
                ret c   ; horiz coord < right safe reach
                ld a,1
                ld (gameover),a
                ret

chkDL ld a,(ix+2)	; horizontal coordinate
		cp (ix+4)	; left limit
		jr nz, chkDL1   
		ld (ix),1   ; change direction to DR
		call beep
		call chkDR2
		ret
chkDL1	ld a,(ix+1)	; vertical coordinate
		cp 184		; bottom row
		jr nz,vchk1
		ld a,2
		ld (gameover),a
		ret
vchk1	cp 176      ; vertical bottom limit
		ret nz		

		call checkSafe
		ld a,(gameover)
		cp 1
		ret z
		ld (ix),2	; change direction to UL
		call moveCeiling
		ret

chkDR	ld a,(ix+2)
		cp (ix+5)	; right limit
		jr nz, chkDR2 	
		ld (ix),0	; change direction to DL
		call beep
		call chkDL1
		ret
chkDR2	ld a,(ix+1)
		cp 184   ; bottom row
		jr nz,vchk2
		ld a,2
		ld (gameover),a
		ret
vchk2	cp 176
		ret nz	

		call checkSafe
		ld a,(gameover)
		cp 1
		ret z
		
		ld (ix),3	; change direction to UR
		call moveCeiling
		ret

chkUL	ld a,(ix+2) ; horizontal
		cp (ix+4)   ;left limit
		jr nz,chkUL1	
		ld (ix),3	; change direction to UR
		call beep
		call chkUR1
		ret
chkUL1	ld a,(ix+1)
		cp (ix+3)	; vertical top limit
		ret nz
		ld (ix),0	; change direction to DL
		call beep
		ret

chkUR	ld a,(ix+2) ; horizontal
		cp (ix+5)   ;right limit
		jr nz,chkUR1	
		ld (ix),2	; change direction to UL
		call beep
		call chkUL1
		ret
chkUR1	ld a,(ix+1)
		cp (ix+3)
		ret nz	
		ld (ix),1	; change direction to DR
		call beep
		ret

; blank out existing ceiling.
; change ball top limit
; draw ceiling in new position.
; if moving up, draw vertical pipes in previous row.
; set colours
; we have not copied to the dummy mask screen...
moveCeiling call random	; 33% chance of ceiling move
			and 3
			cp 0
			ret z	; no ceiling move
			ld b,16    ; ceiling is 16 characters across
			ld c,(ix+7) ; starting horizontal position
blankCeiling push bc
			ld b,(ix+8)
			ld hl,space
			call char
			pop bc
			inc c
			djnz blankCeiling

			ld a,(ix+6) ; ceiling movement direction, 0=down, 1=up
			cp 0
			jr nz,ceilingGoUp
			ld a,(ix+3) ; ball top limit
			add a,8
			ld (ix+3),a
			inc (ix+8) ; increase vertical position
			jr newCeiling
ceilingGoUp	ld a,(ix+3) ;ball top limit
			sub 8
			ld (ix+3),a
			dec (ix+8) ; decrease vertical position
newCeiling ld c,(ix+7) ; starting horizontal position
			ld b,(ix+8) ; vertical position
			push bc
			ld hl,lcpipe
			call char
			pop bc
			inc c
			ld b,14   ; 14 horizontal pipe blocks
newCeil		push bc
			ld b,(ix+8)
			ld hl,hpipe
			call char
			pop bc
			inc c
			djnz newCeil
			ld b,(ix+8)
			ld hl,rcpipe
			call char

			ld a,(ix+6)
			cp 1       ; going up?
			jr nz, ceilColour
			ld c,(ix+7) ; starting horizontal pos
			ld b,(ix+8) ; vertical pos
			inc b
			push bc
			ld hl,vpipe
			call char
			pop bc
			ld a,c
			add a,15
			ld c,a
			ld hl,vpipe
			call char

ceilColour ld b,14 	; 14 blocks to colour
			ld c,(ix+7) ; horiz pos
			inc c		; no need to colour corner pipe again
ceilColour1 push bc
			ld b,(ix+8) ; vert pos
			call atadd
			ld a,pipecolour
			ld (de),a
			inc b
			call atadd
			ld a,bgcolour
			ld (de),a
			pop bc
			inc c
			djnz ceilColour1

copyToMaskScr ld a,(ix+8) 
				cp 0
				jr nz,chkCeilUp
				ld (ix+6),0    ; change ceiling direction to down if at top
				jr postCeilBeep
chkCeilUp		cp 10
				jr nz,postCeilBeep
			ld (ix+6),1			; change direction up if vert pos is 10
postCeilBeep		
			        xor a
        ld (musicloop),a
        ld a,3
        ld (songid),a
		push ix
        call nanoinit
		pop ix
		ret

		
		
						
; get screen address for coordinates
scadd  ld a,(ix+1)       ; fetch vertical coordinate.
       ld e,a              ; store that in e.

; Find line within cell.   

       and 7               ; line 0-7 within character square.
       add a,64            ; 64 * 256 = 16384 = start of screen display.
       ld d,a              ; line * 256.

; Find which third of the screen we're in.

       ld a,e              ; restore the vertical.
       and 192             ; segment 0, 1 or 2 multiplied by 64.
       rrca                ; divide this by 8.
       rrca
       rrca                ; segment 0-2 multiplied by 8.
       add a,d             ; add to d give segment start address.
       ld d,a

; Find character cell within segment.

       ld a,e              ; 8 character squares per segment.
       rlca                ; divide x by 8 and multiply by 32,
       rlca                ; net calculation: multiply by 4.
       and 224             ; mask off bits we don't want.
       ld e,a              ; vertical coordinate calculation done.

; Add the horizontal element.

       ld a,(ix+2)        ; horizontal coordinate.
       rrca                ; only need to divide by 8.
       rrca
       rrca
       and 31              ; squares 0 - 31 across screen.
       add a,e             ; add to total so far.
       ld e,a              ; de = address of screen.
       ret
; scadd end

; sprite display/undisplay start

sprite ld a,(ix+1)     ;draw preshifted sprite located at (hl)
		ld (tmp1),a    ; store vertical coordinate
		call scadd     ; calculate screen address which gets stored in de
		ld a,8			; height of sprite in pixels

sprit1 ex af,af'		; store loop countr
		push de			; store screen address
		ld b,(hl)		; first two bytes of sprite graphic is the mask. Put them into b and c
		inc hl
		ld c,(hl)
		inc hl
		ld d,(hl)		; next 2 bytes are the actual sprite. Put into d and e
		inc hl
		ld e,(hl)
		inc hl

		ld (tmp0),hl	; store next sprite row address in tmp0 for later

		pop hl		; get back screen address
		set 5,h		; address of dummy screen (which starts at 24576)
		ld a,(hl)	; what's already there in dummy screen
		and b		; mask away the content
		or d		; add in the actual sprite's first byte
		res 5,h		; go back to physical screen
		ld (hl),a	; show the processed graphic
		inc hl		; do the same for the next screen byte
		set 5,h
		ld a,(hl)
		and c
		or e
		res 5,h
		ld (hl),a
		ld a,(tmp1)	; get temporary vertical position
		inc a		; next line down
		ld (tmp1),a	; store new position
		and 63		; are we moving to the next third of screen?
		jr z,sprit4	; yes so find next segment.
		and 7		; moving into character cell below?
		jr z,sprit5	; yes, find next row.
		dec hl		; go back 1 character
		inc h		; next row of this character cell.

sprit6 	ex de,hl	; put screen address in de
		ld hl,(tmp0)	; next sprite row address
		ex af,af'		; restore loop counter
		dec a
		jp nz,sprit1	; not reached bottom of sprite yet so repeat.
		ret				; job done

sprit4	ld de,31		; next screen segment is 31 bytes on. The number of bytes here depends on the sprite width
		add hl,de		; add to screen address
		jp sprit6		; repeat

sprit5	ld de,63775		; minus 1761. (the number of bytes depends on sprite width)
		add hl,de		; subtract 1762 from physical screen address.
		jp sprit6		; rejoin loop


; Display character hl at screen address de.
; hl needs to point to 1st byte of graphic (8x8 pixels)
char   call chadd          ; find screen address for char.
       ld b,8              ; number of pixels high.
char0  ld a,(hl)           ; source graphic.
       ld (de),a           ; transfer to screen.
		set 5,d				; address of dummy screen 
		ld (de),a			; write to dummy screen
		res 5,d				; go back to physical screen
       inc hl              ; next piece of data.
       inc d               ; next pixel line.
       djnz char0          ; repeat
       ret

; Calculate address of attribute for character: b=vertical pos, c=horiz pos
; Address is returned in de
; Attribute value is returned in accumulator
atadd  ld a,b              ; vertical coordinate.
       rrca                ; multiply by 32.
       rrca                ; Shifting right with carry 3 times is
       rrca                ; quicker than shifting left 5 times.
       ld e,a
       and 3
       add a,88            ; 88x256=address of attributes.
       ld d,a              
       ld a,e
       and 224             
       ld e,a              
       ld a,c              ; horizontal position.
       add a,e
       ld e,a              ; de=address of attributes.
       ld a,(de)           ; return with attribute in accumulator.
       ret


; Return character cell address of block at (b, c) where:
; b = vertical position (0-23)
; c = horizontal position (0-31)
; Screen position address is returned in de

chadd  ld a,b              ; vertical position.
       and 24              ; which segment, 0, 1 or 2?
       add a,64            ; 64*256 = 16384, Spectrum's screen memory.
       ld d,a              ; this is our high byte.
       ld a,b              ; what was that vertical position again?
       and 7               ; which row within segment?
       rrca                ; multiply row by 32.
       rrca
       rrca
       ld e,a              ; low byte.
       ld a,c              ; add on y coordinate.
       add a,e             ; mix with low byte.
       ld e,a              ; address of screen position in de.
       ret

random ld hl,(seed)     ; Pointer
		ld a,h
		and 31			; keep it within first 8k of ROM
		ld h,a
		ld a,(hl)		; get 'random' number from location
		inc hl			; increment pointer
		ld (seed),hl
		ret

include nanobeep.asm

instructions0 defb 22,21,0,16,5,17,0,'1/3=L'
eoinstructions0 equ $
instructions1 defb 22,21,27,16,5,17,0,'2/4=R'
eoinstructions1 equ $
instructions2 defb 22,0,6,16,6,17,0,'PRESS SPACE TO START'
eoinstructions2 equ $


seed defw 0
vpipe   defb 165,189,129,189,189,129,189,165
hpipe   defb 255,0,219,90,90,219,0,255
lcpipe  defb 31,32,71,136,144,163,164,165
rcpipe  defb 248,4,226,17,9,197,37,165
bgimg   defb 0,64,32,0,0,4,2,0
space   defb 0,0,0,0,0,0,0,0
pipecolour equ 70 ; yellow ink (6) on black paper(0), bright(64)
bgcolour equ 69 ; cyan ink, black paper (69)

; Table of ball info
; Format: 
; byte 1: 255=not displayed, 0=DL, 1=DR, 2=UL, 3=UR
; byte 2: y (vertical) coordinate
; byte 3: x (horizontal) coordinate
; byte 4: top limit
; byte 5: left limit
; byte 6: right limit
; byte 7: ceiling movement direction, 0=down, 1=up
; byte 8: ceiling left-most position
; byte 9: ceiling vertical position
; bytes 10+11: address of bat left safe reach
; bytes 12+13: address of bat right safe reach
initdata defb 0,8,64, 8,  7,112,0, 0,0 ; ball 1
		defw batdata+5
		defw batdata+6
initd2	 defb 0,8,192,8,135,241,0,16,0 ; ball 2
		defw batdata+initbatdataend-initbat2+5
		defw batdata+initbatdataend-initbat2+6
initdataend equ $
balldata defs initdataend-initdata

; Table of bat info
; byte 1; 0=not move, 1=left, 2=right
; byte 2: vert coord
; byte 3: horizontal coord
; byte 4: left limit
; byte 5: right limit
; byte 6: left safe reach
; byte 7: right safe reach
initbatdata defb 1 ,184,56,    7, 105,   49, 71   ; bat 1
initbat2    defb 1 ,184,188, 135, 233, 181, 203   ; bat 2
initbatdataend equ $
batdata defs initbatdataend-initbatdata


; bat sprite is not preshifted (16 pixels wide, 8 pixels high)
batimg defb 126,126,127,254,123,222,113,142,103,230,107,214,78,114,76,50

; the mask (2 bytes) always comes before the sprite data (2 bytes) for each preshifted frame row
preshiftedBall defb 255,255, 0,0            ; frame 1
defb 195,255, 60,0
defb 129,255, 110,0
defb 129,255, 94,0
defb 129,255, 126,0
defb 129,255, 126,0
defb 195,255, 60,0
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 2
defb 225,255, 30,0
defb 192,255, 51,0
defb 192,255, 63,0
defb 192,255, 63,0
defb 192,255, 63,0
defb 225,255, 30,0
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 3
defb 240,255, 15,0
defb 224,127, 29,128
defb 224,127, 30,128
defb 224,127, 31,128
defb 224,127, 31,128
defb 240,255, 15,0
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 4
defb 248,127, 7,128
defb 240,63,  15,192
defb 240,63, 15,64
defb 240,63, 15,64
defb 240,63, 15,192
defb 248,127, 7,128
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 5
defb 252,63, 3,192
defb 248,31, 7,224
defb 248,31, 7,224
defb 248,31, 7,160
defb 248,31, 7,96
defb 252,63, 3,192
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 6
defb 254,31, 1,224
defb 252,15, 3,240
defb 252,15, 3,240
defb 252,15, 3,240
defb 252,15, 3,48
defb 254,31, 1,224
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 7
defb 255,15, 0,240
defb 254,7, 1,248
defb 254,7, 1,248
defb 254,7, 1,120
defb 254,7, 1,184
defb 255,15, 0,240
defb 255,255, 0,0
defb 255,255, 0,0           ; frame 8
defb 255,135, 0,120
defb 255,3, 0,252
defb 255,3, 0,188
defb 255,3, 0,188
defb 255,3, 0,252
defb 255,135, 0,120
defb 255,255, 0,0

tmp0 defw 0
tmp1 defw 0
battmp0 defw 0
battmp1 defw 0
dowait defb 0
gameover defb 0


screen incbin explosion.scr
			ORG $EA60
include font.asm
              ORG           $FCFC
IM2Routine:  
              push          af
              push          hl
              push          bc
              push          de
              push          ix
              push          iy
              exx
              ex            af, af'
              push          af
              push          hl
              push          bc
              push          de
				ld hl,dowait
				ld a,(hl)
				cp 0
				jr z,nowait
		wait	ld b,255
		waitlp	djnz waitlp
				dec (hl)
				jr nz,wait
		nowait  ld hl,23672	; frames counter
				inc (hl)	; move it along
              pop           de
              pop           bc
              pop           hl
              pop           af
              ex            af, af'
              exx
              pop           iy
              pop           ix
              pop           de
              pop           bc
              pop           hl
              pop           af
              ei
              reti

; Make sure this is on a 256 byte boundary
              ORG           $FE00
IM2Table:
              defs          257



end 38000
