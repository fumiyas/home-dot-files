" http://vim.sf.net/
" http://pcmania.jp/~moraz/basics.html
" http://www.kawaz.jp/pukiwiki/?vim

set nocompatible
"set viminfo+=!

" Command mode
" ======================================================================

" ga	Show character's attribute

" q <r>	Record command to register <r>
" q <R>	Add command to register <R>
" q	End of command recording
" @ <r> Run command in register <r>
" @@	Run command in latest recoded command

" :sp		Split window
" :vsp		Split window vertically
" C-w C-w	Move to next window
" C-w +		Increase size of window
" C-w -		Decrease size of window
" :only		Close other windows

" :tabnew
" gt
" gT

" zfXX		Create a fold
" zE		Eliminate all folds
" zo		Open fold
" zc		Close fold

" C-a	Increment the number
" C-x	Decrement the number
" [I	??
" *	Search locations for same words on cursor

" !G command	Filter contents via command

" q: q/
" :help object-select
" dib diB cib ciB yib yiB >ib >iB ...
" di> di]

" :verbose set optionname
" :help runtimepath

" :help quickfix
" :mak
" :cl
" :cn

" C-o		Back to place before mark jump
"
" :%!sed -n 's/foo/bar/p'

" ''		Back to the previous place

" !ctags -R
" !gtags
" :TlistToggle	Tag list
" C-]		Tag jump
" C-w]		Open new window and tag jump
" C-t		Tag reverse jump
" :tnext (:tn)	Tag jump to next entry if any
" :tprev (:tp)	Tag jump to prev entry if any
" :tselect (:ts)Select tags
" :tjump (:tj)	Select tags
" :select tagname
" :tag /string
" FIXME		Tag search

" *		do a search on the currently selected word

" C-p, c-n

" Insert mode
" ======================================================================

"       C-w     remove word before cursor

" ======================================================================

"if $TERM == "console"
" source ~/.vimrc.con
"elseif $TERM == "xterm"
" source ~/.vimrc.X
"elseif $TERM =~ "vt[12]*"
" source ~/.vimrc.dec
"endif

" ======================================================================

" Reset "vimrc" autocmd group
augroup vimrc
  autocmd! vimrc
augroup END

if has("autocmd")
" Enabled file type detection
" Use the default filetype settings. If you also want to load indent files
" to automatically do language-dependent indenting add 'indent' as well.
  filetype plugin on
  augroup vimrc
    autocmd BufReadPost /tmp/cvs* set fileencoding=EUC-JP
  augroup END
endif " has ("autocmd")

"syn region myFold start="{" end="}" transparent fold
"set foldnestmax=1
"set foldmethod=syntax

" Coloring
" ======================================================================

" For test:
" :e $VIMRUNTIME/syntax/colortest.vim
" :so %

if &t_Co == '' && &term =~ '.*term$'
  if has("terminfo")
    set t_Co=8
    set t_Sf=^[[3%p1%dm
    set t_Sb=^[[4%p1%dm
  else
    set t_Co=8
    set t_Sf=^[[3%dm
    set t_Sb=^[[4%dm
  endif
endif

" Syntax highlighting
" ======================================================================

if version >= 700
  set cursorline
endif

if has("syntax") && (&t_Co > 2 || has("gui_running"))
  syntax on

  "set cursorcolumn
  "highlight CursorLine ctermbg=DarkBlue

  highlight default link TagName Search

  function! ActivateInvisibleCharIndicator()
    syntax match InvisibleJISX0208Space "　" display containedin=ALL
    highlight InvisibleJISX0208Space ctermbg=DarkBlue guibg=DarkBlue
    syntax match InvisibleTrailingSpace "[ \t]\+$" display containedin=ALL
    highlight InvisibleTrailingSpace ctermbg=Red guibg=Red
  endf
  augroup vimrc
    autocmd BufNewFile,BufRead * call ActivateInvisibleCharIndicator()
  augroup END

  " Status line
  highlight StatusLine   term=NONE cterm=NONE ctermfg=yellow ctermbg=red
  highlight StatusLineNC term=NONE cterm=NONE ctermfg=black  ctermbg=white

  highlight Search term=NONE cterm=NONE ctermfg=black  ctermbg=grey
  highlight IncSearch term=NONE cterm=NONE ctermfg=black  ctermbg=yellow
endif

" ======================================================================

set shortmess+=I

set nolist
"set listchars+=tab:>-
"set listchars+=eol:$
"set listchars+=trail:-
"set listchars+=extends:~
"set listchars+=precedes:~

set ruler
set showmode showmatch showcmd
set scrolloff=5
set sidescroll=1
set sidescrolloff=8

set incsearch hlsearch ignorecase smartcase nowrapscan

set autoindent
set shiftwidth=2
set shiftround
set tabstop=8
set noexpandtab
set smarttab
set backspace=indent,eol,start
set nrformats-=octal
set nobackup
set noendofline
set cinoptions=:0
set comments=
set history=99999

set encoding=UTF-8
set termencoding=UTF-8
set fileencodings=UCS-BOM,UTF-8
if $LANG =~ "euc"
  set termencoding=EUC-JP
endif

set ambiwidth=double

"set fileencodings=iso-2022-jp,cp932,shift-jis,euc-jp-ms,euc-jp,utf-8,utf-16,ucs-2-internal,ucs-2
"set fileencodings=iso-2022-jp,utf-8,ucs-2le,ucs-2,euc-jp,cp932

if has('iconv')
  let s:enc_eucjp = 'EUC-JP'
  let s:enc_jis = 'ISO-2022-JP'
  " Check if iconv has EUC-JP-MS support
  if iconv("\x82\xA0", 'Shift_JIS', 'EUC-JP-MS') ==# "\xA4\xA2"
    let s:enc_eucjp = 'EUC-JP-MS'
  endif
  " Check if iconv has JISX0213 support
  if iconv("\x87\x64\x87\x6a", 'CP932', 'EUC-JISX0213') ==# "\xad\xc5\xad\xcb"
    let s:enc_eucjp = s:enc_eucjp . ',EUC-JISX0213'
    let s:enc_jis = 'ISO-2022-JP-3'
  endif

  " Build fileencodings
  let &fileencodings = s:enc_jis . ',' . &fileencodings
  "let &fileencodings = &fileencodings . ',' . s:enc_jis
  let &fileencodings = &fileencodings . ',' . s:enc_eucjp
"  if &encoding =~# '^EUC-\%(JP\|JISX0213\)$'
"     let &encoding = s:enc_eucjp
"  else
"     let &fileencodings = &fileencodings . ',' . s:enc_eucjp
"  endif
  set fileencodings+=CP932,Shift_JIS,LATIN1,UCS-2LE,UCS-2

  " Discard temporary variables
  unlet s:enc_eucjp
  unlet s:enc_jis
endif

set fileformats=

set formatoptions-=o
set formatoptions-=r

set textwidth=0
if exists('&colorcolumn')
  set colorcolumn=+1
  autocmd FileType sh,cpp,perl,vim,ruby,python,haskell,scheme setlocal textwidth=80
endif
	"
" Status-line
" ======================================================================
	  
function! GetStatusEx()
  let str = &fileformat . '|'
  if has('multi_byte') && &fileencoding != ''
    let str = str . &fileencoding . '|'
  endif
  "let str = str . cfi#format("%s()", "-") . '|'
  return str
endfunction
"let &statusline='%n %f %Y|%{&fileformat}|%{&fileencoding}|%04B|%R|%M%=%c%V,%l/%L %P'
let &statusline='%n %f %Y|%{GetStatusEx()}%04B|%R|%M%=%c%V,%l/%L %P'
"set statusline=%n\ %f\ %y%{GetStatusEx()}[%04B]%m%h%r%=%c%V,%l/%L\ %P
"set statusline=[%n]\ %t\ %y%{GetStatusEx()}\ %m%h%r=%l/%L,%c%V\ %P
"set statusline=%<%f\ %m%r%h%w%{GetStatusEx()}%=%l,%c%V%8P
set laststatus=2

":set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P

" Key binding
" ======================================================================

map - $
"map t8 :set tabstop=8
"map t4 :set tabstop=4
"map gg :s/.*/\/\*&\*\//            " コメントで囲む
"map hh :s/^\/\*\(.*\)\*\/$/\1/     " コメントを外す

"" Global / Gtags
map  :GtagsCursor<CR>
"map <C-> :GtagsCursor<CR>
map <C-\> :Gtags -r <CR>
map <C-n> :cn<CR>
map <C-p> :cp<CR>
"map <C-x> :! sort -n
"map <C-C> :write !xclipx<CR><CR>
"map <C-V> :r! xclip -out<CR><CR>
if version >= 700
  nmap sy <Plug>(fakeclip-y)
  nmap syy <Plug>(fakeclip-Y)
  vmap sy <Plug>(fakeclip-y)
  nmap sp <Plug>(fakeclip-p)
endif

function! ClipboardPaste() range
  let text = system("xclip -out")
  let pos = getpos(".")
  " FIXME: ???
  "if visualmode() == 'v'
  "  execute ":delete"
  "endif
  set noautoindent
  execute ":normal i" . text
  set autoindent
  call setpos(".", pos)
endfunction
command! ClipboardPaste :call ClipboardPaste()

"function! ClipboardCopy() range
"  :write !xclipx
"  :let tmp = @@
"  :silent normal gvy
"  :let selected = @@
"  :let @@ = tmp
"  :echo selected
"endfunction
"command! -range ClipboardCopy :silent call ClipboardCopy()

"command! -range ClipboardCopy :<line1>,<line2>call ClipboardCopy()
map <Esc>c :write !xclip<CR><CR>
map <Esc>v :ClipboardPaste<CR>
"map <Esc>v :read !xclip -out<CR><CR>

" FIXME
"function! ClipboardCopy()
"  " Save unnamed register
"  let s:regd = @@
"
"  let exe a:firstline . "," . a:lastline . "y"
"
"  " Restore unnamed register
"  let @@ = s:regd
"endfun

"> 貼り付けをCtrl+Vに(も)割り当てる設定
":map <C-V> "+gP
":cmap <C-V> <C-R>+
"
"> Ctr+Sで保存できるような設定
":inoremap <C-s> <C-o>:w<CR>

" 文字列を選択して :call HtmlEscape()
function! HtmlEscape()
  silent s/&/\&amp;/eg
  silent s/>/\&gt;/eg
  silent s/</\&lt;/eg
endfunction

nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz

iab Yruler 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

" Auto command
" ======================================================================

augroup vimrc
  autocmd BufNewFile,BufRead *.[ch] set formatoptions=croql cindent comments=sr:/*,bm:*,el:*/,://
  autocmd BufNewFile *.pm 0r ~/lib/vim/template/package.pm
  autocmd BufNewFile *.pl 0r ~/lib/vim/template/perl.pl
  "autocmd BufNewFile *.pl execute "normal i#!/usr/bin/env perl\<CR>" | echo "New File"

  autocmd BufNewFile *.html 0r ~/lib/vim/template/strict.html
  autocmd BufNewFile *.html set fileencoding=UTF-8

  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt if getline(1) =~ "Last modification: "
  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt normal msgg/Last modification: /e+1"_D"=strftime("%Y %b %d")p`s
  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt endif

augroup END

"augroup BinaryXXD
"  autocmd!
"  autocmd BufReadPre  *.bin let &binary =1
"  autocmd BufReadPost * if &binary | silent %!xxd -g 1
"  autocmd BufReadPost * set ft=xxd | endif
"  autocmd BufWritePre * if &binary | %!xxd -r | endif
"  autocmd BufWritePost * if &binary | silent %!xxd -g 1
"  autocmd BufWritePost * set nomod | endif
"augroup END

" ======================================================================

runtime! ftplugin/man.vim
nnoremap K :Man <cword><CR>

if filereadable($VIMRUNTIME . "/macros/matchit.vim")
  source $VIMRUNTIME/macros/matchit.vim
endif

if filereadable($HOME . "/lib/vim/plugin/yankring.vim") && version >= 700
  let g:yankring_persist = 0
  source $HOME/lib/vim/plugin/yankring.vim
endif

if filereadable($HOME . "/lib/vim/plugin/fakeclip.vim") && version >= 700
  source $HOME/lib/vim/plugin/fakeclip.vim
endif

if filereadable($HOME . "/lib/vim/plugin/blockdiff.vim")
  source $HOME/lib/vim/plugin/blockdiff.vim
endif

if filereadable($HOME . "/lib/vim/plugin/verifyenc.vim")
  source $HOME/lib/vim/plugin/verifyenc.vim
  let verifyenc_enable=1
endif

if filereadable($HOME . "/lib/vim/plugin/grep.vim")
  source $HOME/lib/vim/plugin/grep.vim
endif

if filereadable("/usr/bin/ctags") || filereadable("/usr/local/bin/ctags")
  if filereadable($HOME . "/lib/vim/plugin/taglist.vim")
    source $HOME/lib/vim/plugin/taglist.vim
  endif
  set updatetime=1000
  set tags=./tags,./TAGS,tags,TAGS;/
endif

"let migemo=1
if filereadable($HOME . "/lib/vim/plugin/migemo.vim")
  source $HOME/lib/vim/plugin/migemo.vim
endif

if filereadable($HOME . "/lib/vim/plugin/closetag.vim")
  " C-_ in insert mode
  let g:closetag_html_style=1
  au Filetype html,xml,xsl,ant source $HOME/lib/vim/plugin/closetag.vim
endif

