" Vim syntax file
" Language:	SAS LOG files
" Mantainer: J. Manuel Picaza <jmpicaza@gmail.com>
"  For version 5.x: Clear all syntax items
"  For version 6.x: Quit when a syntax file was already loaded
if version < 600
   syntax clear
elseif exists("b:current_syntax")
   finish
endif

" Poner la letra mas pequeña
set gfn=courier_new:h9:cANSI

" señalar columna y fila en la que estoy
setlocal cursorcolumn
setlocal cursorline
hi cursorColumn guibg=white
hi cursorLine guibg=white



syn case ignore

syn match nmacro   /(\k\+):/ contained
syn match nnote    /^\(NOTE\|NOTA\|INFO\)\>/ contained
syn match nwarning    /^\(WARNING\|AVISO\)\>/ contained
syn match nerror    /^ERROR\>/ contained

syn region mensajes	start=/^\S/  end=/$/
syn region mensajes	start=/^\s*\d\{8,}/  end=/$/
syn region mprint	start=/^MPRINT(/  end=/;/ contains=nmacro
syn region mprint	start=/^MACROGEN(/  end=/$/ contains=nmacro
syn region note		start=/^\(NOTE\|NOTA\|INFO\):/  end=/$/ contains=nnote
syn region note		start=/^      \S/  end=/$/
"syn region warning	start=/^WARNING\s*\d*-*\d*:/  end=/^$/ contains=nwarning
syn region warning	start=/^\(WARNING\|AVISO\)\s*\d*-*\d*:/  end=/$/ contains=nwarning
syn region warning	start=/^\(\s\{9}\|\s\{7}\)\S/  end=/$/
syn region error	start=/^ERROR\s*\d*-*\d*:/  end=/$/ contains=nerror
syn region error	start=/^\s\{10,}\S/  end=/$/
syn region codigo	start=/^\d\{1,6}!\= \++\@!/  end=/$/
"syn region Mcodigo	start=/^\d\+!\=\s*+/  end=/$/
syn region Mcodigo	start=/^\d\+\s*!\=+/  end=/$/

" TODO cambiarlo a busquedas de tipo: /^ERROR\( \d\+-\d\+\)*:.*\n\(\s\{5,}\S.*$\n\)*/

"  Define the default highlighting.

   " Default sas enhanced editor color syntax
	hi mcodigo	term=NONE cterm=NONE ctermfg=Green ctermbg=Black gui=italic guifg=SeaGreen  guibg=NONE
	hi note		term=none cterm=NONE ctermfg=Blue  ctermbg=Black gui=none   guifg=Blue      guibg=NONE
	hi nnote	term=none cterm=NONE ctermfg=Blue  ctermbg=Black gui=bold   guifg=Blue      guibg=NONE
	hi warning	term=bold cterm=NONE ctermfg=Green ctermbg=Black gui=bold   guifg=DarkGreen guibg=NONE
	hi nwarning	term=bold cterm=NONE ctermfg=Green ctermbg=Black gui=bold,underline   guifg=DarkGreen guibg=NONE
	hi error	term=NONE cterm=NONE ctermfg=Red   ctermbg=Black gui=bold   guifg=Red       guibg=NONE
	hi nerror	term=NONE cterm=NONE ctermfg=Red   ctermbg=Black gui=bold,underline   guifg=Red       guibg=NONE
	hi codigo	term=NONE cterm=NONE ctermfg=Black ctermbg=Black gui=none   guifg=Black     guibg=NONE
	hi mprint   term=NONE cterm=NONE ctermfg=Black ctermbg=Black gui=italic guifg=Black     guibg=NONE
	hi nmacro    term=NONE cterm=NONE ctermfg=Black ctermbg=Black gui=bold  guifg=Black     guibg=NONE
	hi codigo	term=NONE cterm=NONE ctermfg=Black ctermbg=Black gui=NONE   guifg=Black     guibg=NONE
	hi mensajes	term=NONE cterm=NONE ctermfg=Black ctermbg=Black gui=bold   guifg=Black     guibg=NONE

" Syncronize from beginning to keep large blocks from losing
" syntax coloring while moving through code.
syn sync fromstart

let b:current_syntax = "sasLog"

" vim:ts=4:nobackup
