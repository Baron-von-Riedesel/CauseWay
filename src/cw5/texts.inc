
if SELECTTEXT eq 1
ErrorM00        db 'CauseWay error '
ErrorM00n       db '00 : $'
ErrorM01        label byte
        if ENGLISH
        db 'Unable to re-size program memory block.',13,10,'$'
        elseif SPANISH
        db "Incapaz de redimensionar el bloque de memoria del programa.",13,10,"$"
        endif
ErrorM02        db "$"
ErrorM03        db "$"
ErrorM04        db "$"
ErrorM05        label byte
        if ENGLISH
        db 'Not enough memory for CauseWay.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para CauseWay.",13,10,"$"
        endif
ErrorM06        db "$"
ErrorM07        db "$"
ErrorM08        db "$"
ErrorM09        label byte
        if ENGLISH
        db 'Unrecoverable exception. Program terminated.',13,10,'$'
        elseif SPANISH
        db "Excepcion irrecuperable. Programa terminado.",13,10,"$"
        endif
ErrorM10        label byte
        if ENGLISH
        db 'Unable to find '
ErrorM10_T      db 'application to load.',13,10,'$',32 dup (0)
        elseif SPANISH
        db "Incapaz de encontrar '
ErrorM10_T      db 'aplicacion a cargar.",13,10,"$",32 dup (0)
        endif
ErrorM11        label byte
        if ENGLISH
        db 'DOS reported an error or corrupt file found.'
        elseif SPANISH
        db "DOS informo de un error o de un fichero corrupto."
        endif
ErrorM11_0      db 13,10,'$'
        db "No:"
ErrorM11_1      db "00000000",13,10,"$"
ErrorM12        label byte
        if ENGLISH
        db 'Not enough memory to load application.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para cargar la aplicacion.",13,10,"$"
        endif
ErrorM13        db "$"
ErrorM14        label byte
        if ENGLISH
        db 'Memory structures destroyed. Program terminated.',13,10,'$'
        elseif SPANISH
        db "Estructuras de memoria destruidas. Programa terminado.",13,10,"$"
        endif
ErrorM15        label byte
        if ENGLISH
        db 'DOS reported an error while accessing swap file. Program terminated.',13,10,'$'
        elseif SPANISH
        db "DOS informa de un error mientras se accede al fichero de swap. Programa terminado.",13,10,"$"
        endif
ErrorM16        label byte
        if ENGLISH
        db "Unsupported DOS function call, program terminated.",13,10,"$"
        elseif SPANISH
        db "Llamada a funcion no soportada del DOS, programa terminado.",13,10,"$"
        endif
endif
;
if SELECTTEXT eq 2
IErrorM00       db 'CauseWay error '
IErrorM00n      db '00 : $'
IErrorM01       label byte
        if ENGLISH
        db 'Unable to re-size program memory block.',13,10,'$'
        elseif SPANISH
        db "Incapaz de redimensionar el bloque de memoria del programa",13,10,"$"
        endif
IErrorM02       label byte
        if ENGLISH
        db '386 or better required.',13,10,'$'
        elseif SPANISH
        db "Se requiere un 386 o superior.",13,10,"$"
        endif
IErrorM03       label byte
        if ENGLISH
        db 'Non-standard protected mode program already active.',13,10,'$'
        elseif SPANISH
        db "Programa en modo protegido no estandar activado.",13,10,"$"
        endif
IErrorM04       label byte
        if ENGLISH
        db 'DOS 3.1 or better required.',13,10,'$'
        elseif SPANISH
        db "DOS 3.1 o superior requerido.",13,10,"$"
        endif
IErrorM05       label byte
        if ENGLISH
        db 'Not enough memory for CauseWay.',13,10,'$'
        elseif SPANISH
        db "Memoria insuficiente para CauseWay.",13,10,"$"
        endif
IErrorM06       label byte
        if ENGLISH
        db 'VCPI failed to switch into protected mode.',13,10,'$'
        elseif SPANISH
        db "VCPI fallo al cambiar a modo protegido.",13,10,"$"
        endif
IErrorM07       label byte
        if ENGLISH
        db 'Unable to control A20.',13,10,'$'
        elseif SPANISH
        db "Capaz de controlar A20.",13,10,"$"
        endif
IErrorM08       label byte
        if ENGLISH
        db 'Selector allocation error.',13,10,'$'
        elseif SPANISH
        db "Error de asignacion del selector.",13,10,"$"
        endif
IErrorM09       label byte
        if ENGLISH
        db 'DPMI failed to switch to protected mode.',13,10,'$'
        elseif SPANISH
        db "DPMI fallo al cambiar a modo protegido.",13,10,"$"
        endif
endif
