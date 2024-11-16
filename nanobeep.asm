;******************************************************************
;Copyright (c) 2014-2019, utz/irrlicht project
;All rights reserved.
;
;Redistribution and use in source and binary forms, with or without
;modification, are permitted provided that the following conditions are met:
;
;* Redistributions of source code must retain the above copyright notice, this
;  list of conditions and the following disclaimer.
;
;* Neither the name of utz/irrlicht project nor the names of its
;  contributors may be used to endorse or promote products derived from
;  this software without specific prior written permission.
;
;THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;nanobeep
;77 byte beeper engine by utz 09'2015-04'2016
;******************************************************************
;
;ignores kempston
;only reads keys Space,A,Q,1 (can be fixed with 2 additional bytes)
;
;D - add counter ch1
;E - base freq ch1
;B - internal delay counter
;C - add counter ch2
;HL - data pointer
;IY - timer
;IX - pointer to where to read music sequence data

nanoinit
;			di
	push iy    ; changing iy seems to have a side effect so preserve it
			ld a,(songid)
			cp 0
			ret z			; no song to play
			cp 1
			jr nz,songchk2
			ld ix,song1+2	
			jr rdseq
songchk2	cp 2
			jr nz,songchk3
			ld ix,song2+2
			jr rdseq
songchk3	ld ix,song3+2	; must be this song

;******************************************************************
rdseq
	xor a
	ld l,(ix)
	inc ix
	ld h,(ix)
	inc ix
	or h
	jr nz,rdptn
	ex af,af'
	ld a,(musicloop) ; should we end the song now?
	cp 0
	jr z, exit

	ld a,(songid)	; check which loop to use
	cp 1
	jr nz,loopchk2
	ld ix,loop1
	jr loopend
loopchk2 cp 2
		jr nz,loopchk3
		ld ix,loop2
		jr loopend
loopchk3 ld ix,loop3
loopend	ex af,af'
		jr rdseq

drum
	ex de,hl
	ld h,a
	ld c,l
	ld b,h
	otir     ; reads from (hl) and writes to the (c) port. hl is incremented and b is decremented until b=0
	ex de,hl

;******************************************************************	
rdptn
	inc hl	
	ld a,(hl)		;base freq ch1		
	ld e,a
	inc a			;if A=#ff
	jr z,rdseq
	
	inc a
	jr z,drum

	inc hl			;point to base freq ch2	

	ex af,af'
	ld a,(songid)
	cp 1
	jr nz,speedchk2	
	ld iy,(song1)	;speed for song1
	jr speedend
speedchk2 cp 2
		jr nz,speedchk3
		ld iy,(song2)	; speed for song2
		jr speedend
speedchk3 ld iy,(song3)	; speed for song3
speedend ex af,af'

;******************************************************************
play 
	ld a,d
	add a,e
	ld d,a
	
	ld b,48
	
	sbc a,a
	and b
	out (#fe),a   ; write to port 254

	djnz $

	ld a,c
	add a,(hl)
	ld c,a
	
	ld b,48

	sbc a,a
	and b
	out (#fe),a
	
	djnz $

	dec iy
	ld a,iyh
	or b
	jr nz,play

	ld a,(musicloop)
	cp 0
	jr z,rdptn	    ; if not looping indefinitely, don't read keyboard

	in a,(#fe)		;read kbd
	rra
	jr c,rdptn		;only space,a,q,1 will exit
	;cpl			;comment out the 2 lines above and uncomment this for full keyboard scan
	;and #1f
	;jr nz,rdptn
	
;******************************************************************			
exit
;	ei
	pop iy
	ret
;******************************************************************

musicloop defb 1      ; whether to play the music once or infinite loop
songid defb 1         ; which song to play
song1
	include "music1.asm"
song2
	include "music2.asm"
song3
	include "music3.asm"
