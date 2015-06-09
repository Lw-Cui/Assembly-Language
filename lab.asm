
assume cs:code
code segment
start:
	mov ax,cs
	mov es,ax
	mov ah,1
	mov al,3
	mov cl,0

	mov bx,offset begin
	mov dx,0
	call write
	
	;mov bx,offset time
	;mov dx,1
	;call write

	mov ax,4c00H
	int 21h
;if ah==1 then from es:bx to dl of floppy (al*512byte) 
;if ah==0 then from dl of floppy to es:bx (al*512byte)
;dl from cl

;start---------07c00H----------------
begin:
	mov ax,cs
	mov es,ax
	mov bx,7c00h
	mov cl,0
	mov dx,0
	mov ah,0
	mov al,3
	call write
	
	call beg
	x:dw s1-x,s2-x,s3-x,s4-x,t1-x,t2-x,t3-x,top-x,t-x,0,0ffffh,0,0
 	s1:db '1) reset pc','$'
	s2:db '2) start system','$'
	s3:db '3) clock','$'
	s4:db '4) set clock','$'
	t1:db '**/**/** **:**:**','$'
	t2:db 9,8,7,4,2,0
	t3:db 0,3,6,9,12,15
	top:dw 0
	t:db 18 dup('$')
beg:
	pop bp;get ip
	mov cx,9
	mov si,0
set:
	add word ptr cs:[bp][si],bp
	add si,2
	loop set

	mov ax,0
	mov es,ax
	mov di,200H
	mov ax,cs
	mov ds,ax
	mov si,offset show_str-offset x
	add si,bp
	mov cx,offset show_end-offset show_str
	cld
	rep movsb

	push es:[7ch*4]
	pop cs:[bp].22
	push es:[7ch*4+2]
	pop cs:[bp].24

	mov word ptr es:[7ch*4],200H
	mov word ptr es:[7ch*4+2],0H

print:
	call clear
	mov dx,cs
	mov es,dx
	mov dh,10
	mov dl,25
	mov di,0
	mov cx,4
Sshowf:
	mov si,cs:[bp][di]
	push cx
	mov cl,00000100b
	;call show_str
	int 7ch
	pop cx
	inc dh
	add di,2
	loop Sshowf

	mov ah,0
	int 16h

	cmp al,'1'
	jne ch1
	jmp near ptr reset
ch1:
	cmp al,'2'
	jne ch2
	jmp near ptr startS
ch2:
	cmp al,'3'
	jne ch3
	call clear
	call time
ch3:
	cmp al,'4'
	jne ch4
	call clear
	call getstr
ch4:
	jmp short print

;row:dh  column:dl  from es:si -> 0  color:cl
show_str:
	push ax
	push bx
	push cx
	push si
	push di
	push ds

	mov ax,160
	mul dh
	mov bx,ax

	mov ax,2
	mul dl
	mov di,ax

	mov ax,0B800H
	mov ds,ax
S_show:
	mov al,es:[si]
	cmp al,'$'
	je OK_show
	mov [bx][di].0,al
	mov [bx][di].1,cl
	add di,2
	inc si
	jmp short S_show
OK_show:
	mov byte ptr [bx][di].0,' '
	pop ds
	pop di
	pop si
	pop cx
	pop bx
	pop ax
	iret

show_end:
	nop

clear:
	push cx
	push bx
	push es

	mov cx,0b800H
	mov es,cx
	mov cx,2000
	mov bx,0
Sc:
	mov byte ptr es:[bx].0,' '
	mov byte ptr es:[bx].1,00000111B
	add bx,2
	loop Sc
	
	pop es
	pop bx
	pop cx
	ret

;if ah==1 then from es:bx to dx of floppy (512byte) 
;if ah==0 then from dx of floppy to es:bx
;dl from cl
write:
	push si
	push dx
	push cx
	push ax

	mov ax,dx
	mov dx,0
	mov si,1440
	div si

	push dx
	mov dh,al
	mov dl,cl

	pop ax
	mov cl,18
	div cl

	mov ch,al
	mov cl,ah
	inc cl

	pop ax
	add ah,2

	int 13h
	pop cx
	pop dx
	pop si
	ret
K:
	;db 512-(K-begin) dup(0)

;cs:[bp][4*2]:db '**/**/** **:**:**',0
;cs:[bp][5*2]:db 9,8,7,4,2,0
;cs:[bp][6*2]:db 0,3,6,9,12,15
time:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push bp
	push es
	push ds

	mov cx,2
	mov ds,cx
timebeg:
	mov cx,6
	mov si,0
S:
	mov bx,cs:[bp].10
	mov al,cs:[bx][si]
	out 70h,al
	in al,71h
	
	mov ah,al
	push cx
	mov cl,4
	shr ah,cl
	pop cx
	and al,00001111b
	add ah,30h
	add al,30h

	mov bx,cs:[bp].12
	mov di,cs:[bx][si]
	and di,00001111b

	mov bx,cs:[bp].8
	mov cs:[bx][di].0,ah
	mov cs:[bx][di].1,al

	inc si
	loop S

	;mov ah,0
	;int 16h
	mov cx,ds

	in al,60H
	;cmp al,0bbh
	cmp al,3bh
	jne n
	inc cl
	cmp cl,0ffH
	jne n
	mov cl,0
n:
	mov ds,cx

	cmp al,1h
	je tend

	mov dx,cs
	mov es,dx
	mov dh,10
	mov dl,25
	mov si,cs:[bp].8
	;call show_str
	int 7ch
	jmp short timebeg
tend:
	pop ds
	pop es
	pop bp
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
	
;top:cs:[bp].14
getstr:
	push ax
	push bx
	push cx
	push dx
	push si
	push di

Sstr:
	mov ah,0
	int 16h
	cmp ah,0eh
	jne next1
	call Backspace
	jmp short showstr
next1:
	cmp ah,1ch
	jne next2
	jmp short Enter
next2:
	call charpush
	jmp short showstr
showstr:
	mov dx,cs
	mov es,dx
	mov si,cs:[bp].16
	mov dh,10
	mov dl,25
	mov cl,00000100B
	int 7ch
	jmp short Sstr

Enter:
	mov cx,6
	mov si,0
Sset:
	mov bx,cs:[bp].12
	mov di,cs:[bx][si]
	and di,00001111B
	mov bx,cs:[bp].16
	mov dx,cs:[bx][di]
	sub dl,30H
	sub dh,30H
	push cx
	mov cl,4
	shl dl,cl
	pop cx
	add dh,dl
	
	mov bx,cs:[bp].10
	mov al,cs:[bx][si]
	out 70h,al
	mov al,dh
	out 71h,al
	
	inc si
	loop Sset

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret

charpush:
	push bx
	push si
	push di

	mov di,cs:[bp].14
	mov si,cs:[di]
	mov bx,cs:[bp].16
	mov cs:[bx][si],al
	inc si
	mov cs:[di],si

	pop di
	pop si
	pop bx
	ret

Backspace:
	push ax
	push bx
	push si
	push di

	mov di,cs:[bp].14
	mov si,cs:[di]
	cmp si,0
	je popend
	dec si
	mov al,'$'
	mov bx,cs:[bp].16
	mov cs:[bx][si],al
	mov cs:[di],si
popend:
	pop di
	pop si
	pop bx
	pop ax
	ret

reset:
	jmp dword ptr cs:[bp].18
startS:       
	mov ax,0
	mov es,ax

	push cs:[bp].22
	pop es:[7ch*4]
	push cs:[bp].24
	pop es:[7ch*4+2]

	call clear
	mov bx,7c00H
	mov cl,80H
	mov dx,0
	mov ah,0
	mov al,1
	call write2

	mov ax,7c00H
	jmp ax

write2:
	push si
	push dx
	push cx
	push ax

	mov ax,dx
	mov dx,0
	mov si,1440
	div si

	push dx
	mov dh,al
	mov dl,cl

	pop ax
	mov cl,18
	div cl

	mov ch,al
	mov cl,ah
	inc cl

	pop ax
	add ah,2

	int 13h
	pop cx
	pop dx
	pop si
	ret
;if ah==1 then from es:bx to dl of floppy (al*512byte) 
;if ah==0 then from dl of floppy to es:bx (al*512byte)
;dl from cl

code ends
end start 
