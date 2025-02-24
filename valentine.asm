.model tiny

.data
;-----------------------------------------------------------
        videoseg        equ     0b800h
        fs_len          equ     9d
        exit_cmd        equ     4c00h
        cmd_args_addr   equ     82h
        screen_height   equ     15d
        screen_width    equ     80d
;-----------------------------------------------------------

.code
org 100h

Start:
        cld                             ; clear destination flag

        mov bx, videoseg                ;
        mov es, bx                      ; videoseg addr
        xor bx, bx                      ;

        call GetParam                   ; getting cmd parameters

        cmp dh, 0                       ; keeping si safe in case of user's frame style
        je UserFS                       ;

        push si                         ; saving text address
        mov si, offset FrameStyle       ;
        dec dh                          ;
        mov al, fs_len                  ; setting frame style
        mul dh                          ;
        add si, ax                      ;

        UserFS:

        call SetFramePtr                ; calculating the first frame symbol addr

        call DrawFrame                  ; drawing frame

        cmp dh, 0                       ;
        je DontTouchSi                  ;
        pop si                          ; restoring text address if it was changed
        DontTouchSi:                    ;

        call StrLen                     ; text len in cx
        call SetTextPtr                 ; calculating the text address in video memory
        call WriteText                  ; writing the text

        mov ax, exit_cmd                ; ending program
        int 21h                         ;

;-----------------------------------------------------------------
; Function GetParam gets the frame parameters from cmd
; Entry: none
; Exit:  width in bh, height in bl, color in dl, frame style in dh
; Destr: ax, cx
;-----------------------------------------------------------------
GetParam        proc

                mov si, cmd_args_addr   ; cmd beginning (first space skipped)

                call AtoI               ; getting width
                mov bh, cl              ;

                call AtoI               ; getting height
                mov bl, cl              ;

                call AtoI               ; getting color
                mov dl, cl              ;

                call AtoI               ; getting frame style
                mov dh, cl              ;

                ret
                endp

;-----------------------------------------------------------------
; Function StrLen counts the number of letters in the string
; Entry: addr in si
; Exit:  number of letters in cx
; Destr: al, cx
;-----------------------------------------------------------------
StrLen          proc

                push es                 ; saving es, di
                push di                 ;

                push ds                 ;
                pop es                  ; es:[di] = ds:[si]
                mov di, si              ;

                mov al, "$"             ; terminal character
                xor cx, cx              ;
                dec cx                  ; cx = -1

                repne scasb             ; cx = -1 - (n + 1)

                neg cx                  ;
                sub cx, 2h              ; cx = n

                pop di                  ; restoring es, di
                pop es                  ;

                ret
                endp


;-----------------------------------------------------------------
; Function AtoI gets a number from cmd arguments line
; Entry: addr in si
; Exit:  number in cl
; Destr: ax, cx
;-----------------------------------------------------------------
AtoI            proc

                xor cx, cx              ; ax, cx = 0
                xor ax, ax              ;

        continue:
                mov ch, ds:[si]         ;
                                        ;
                cmp ch, "0"             ;
                jb finish               ; Checking for a number
                                        ;
                cmp ch, "9"             ;
                ja finish               ;

                mov al, 10d             ;
                mul cl                  ;
                add al, ch              ; cl = cl*10 + ch
                sub al, "0"             ;
                mov cl, al              ;

                inc si                  ; next symb
                jmp continue            ;


        finish:
                call SkipSpaces         ; Spaces skipping
                ret
                endp

;-----------------------------------------------------------------
; Function SkipSpaces skips spaces in a string
; Entry: addr in si
; Exit:  none
; Destr: ch
;-----------------------------------------------------------------
SkipSpaces      proc

        skipping:
                mov ch, ds:[si]         ;
                cmp ch, " "             ; Checking for space
                jne not_a_space         ;

                inc si                  ; next symb
                jmp skipping            ;

        not_a_space:
                ret                     ; finish function
                endp                    ;

;-----------------------------------------------------------------
; Function SetFramePtr calculates the address of the first frame symbol
; Entry: frame width in bh, height in bl
; Exit:  frame offset in di
; Destr: ax, cl, di
;-----------------------------------------------------------------
SetFramePtr     proc

                mov al, screen_height           ;
                sub al, bl                      ;
                xor ah, ah                      ;
                                                ;
                mov cl, 2h                      ;
                div cl                          ; vertical offset ((15 - bl) // 2 * 160)
                                                ;
                mov cl, screen_width * 2h       ;
                mul cl                          ;

                mov di, ax                      ;
                mov al, screen_width            ;
                sub al, bh                      ;
                xor ah, ah                      ;
                                                ;
                mov cl, 2h                      ;
                div cl                          ; horizontal offset ((80 - bh) // 2 * 2)
                                                ;
                mul cl                          ;
                                                ;
                add di, ax                      ;

                ret
                endp

;---------------------------------------------------------------------------
; Function SetTextPtr calculates the address of the first symbol of the text
; Entry: frame width in bh, height in bl, text len in cx,
;        videoseg address under left down corner in di
; Exit:  text offset in di
; Destr: ax, di, cx
;---------------------------------------------------------------------------
SetTextPtr      proc

                push cx                         ; saving text len
                push cx                         ;

                mov al, bl                      ;
                xor ah, ah                      ;
                mov cl, 2h                      ;
                div cl                          ;
                inc al                          ; vertical offset
                                                ;
                mov cl, screen_width * 2h       ;
                mul cl                          ;
                                                ;
                sub di, ax                      ;

                pop cx                          ; restoring text len

                mov al, bh                      ;
                xor ah, ah                      ;
                sub ax, cx                      ;
                mov cl, 2h                      ;
                div cl                          ;
                                                ; horizontal offset
                xor ah, ah                      ;
                mul cl                          ;
                                                ;
                add di, ax                      ;

                pop cx                          ; restoring text len in cx

                ret
                endp

;-----------------------------------------------------------------
; Function DrawFrame draws a centred frame
; Entry: frame width in bh, height in bl, first elem addr in di,
;        bound elements addr in si, color in dl
; Exit:  none
; Destr: ax, cx, di, si
;-----------------------------------------------------------------
DrawFrame       proc

                call DrawLine           ; first line

                xor ch, ch               ;
                mov cl, bl              ;
                sub cx, 2h              ;
                                        ;
        centre_frame:                   ;
                push cx                 ;
                call DrawLine           ; centre part
                pop  cx                 ;
                sub  si, 3h             ;
        loop centre_frame               ;
                                        ;
                add si, 3h              ;

                call DrawLine           ; last line

                ret
                endp

;-----------------------------------------------------------------
; Function DrawLine draws line of the frame
; Entry: frame width in bh, curr addr in di, bound elem addr in si,
;        color in dl
; Exit:  none
; Destr: ax, cx, di, si
;-----------------------------------------------------------------
DrawLine        proc

                mov ah, dl                      ; color

                lodsb                           ;
                stosw                           ; left elem

                xor ch, ch                      ;
                mov cl, bh                      ;
                sub cx, 2h                      ; centre elements
                                                ;
                lodsb                           ;
                rep stosw                       ;

                lodsb                           ;
                stosw                           ; right elem

                push ax                         ; saving color

                add di, screen_width * 2h       ;
                mov al, bh                      ;
                mov cl, 2h                      ; next line
                mul cl                          ;
                sub di, ax                      ;

                pop ax                          ; restoring color

                ret
                endp

;---------------------------------------------------------------------
; Function WriteText writes the text in the centre of the frame
; Entry: text len in cx, curr addr in di, text addr in si, color in dl
; Exit:  none
; Destr: ax, di, si
;---------------------------------------------------------------------
WriteText       proc

                mov ah, dl              ; color

        write:
                lodsb                   ; es:[di] = ds:[si] cx times
                stosw                   ;
        loop write

                ret
                endp

FrameStyle db '*-*| |*-*', 201, 205, 187, 186, " ", 186, 200, 205, 188, 3, 3, 3, 3, " ", 3, 3, 3, 3, '123456789'

end Start
