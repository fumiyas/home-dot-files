scriptencoding UTF-8
" http://vim.sf.net/
" http://pcmania.jp/~moraz/basics.html
" http://www.kawaz.jp/pukiwiki/?vim

set nocompatible
"set viminfo+=!

if !exists(expand('$HOME/.vim/private'))
  silent !umask 077; mkdir -p "$HOME/.vim/private"
endif

" Plugins
" ======================================================================

" NeoBundle
" ----------------------------------------------------------------------

filetype off

if has('vim_starting')
  if isdirectory(expand('~/.vim/bundle/neobundle.vim')) && version >= 702
    set runtimepath+=~/.vim/bundle/neobundle.vim
    call neobundle#begin(expand('~/.vim/bundle/'))
    NeoBundleFetch 'Shougo/neobundle.vim'
    NeoBundle 'https://github.com/koron/verifyenc-vim.git'
    NeoBundle 'https://github.com/itchyny/lightline.vim.git'
    NeoBundle 'BlockDiff'
    NeoBundle 'vim-creole'
    NeoBundle 'vim-scripts/diffchar.vim'
    NeoBundle 'nathanaelkane/vim-indent-guides'
    NeoBundle 'kana/vim-fakeclip'
    NeoBundle 'YankRing.vim'
    NeoBundle 'closetag.vim'
    NeoBundle 'MultipleSearch'
    NeoBundle 'L9'
    NeoBundle 'FuzzyFinder'
    NeoBundle 'Tagbar'
    NeoBundle 'tpope/vim-fugitive'
    NeoBundle 'scrooloose/syntastic.git'
    NeoBundle 'lambdalisue/vim-unified-diff'
    "NeoBundle 'https://github.com/dhruvasagar/vim-table-mode/'
    "NeoBundle 'https://github.com/AndrewRadev/inline_edit.vim'

    NeoBundle 'godlygeek/tabular'
    NeoBundle 'plasticboy/vim-markdown'
    let g:vim_markdown_folding_disabled=1

    call neobundle#end()
    function! NeoBundleIsInstalled(name)
      return neobundle#is_installed(a:name)
    endfunction
  else
    function! NeoBundleIsInstalled(name)
      return ''
    endfunction
  endif
endif

filetype plugin on
filetype indent on

" misc
" ----------------------------------------------------------------------

runtime! ftplugin/man.vim
nnoremap K :Man <cword><CR>

if filereadable($VIMRUNTIME . "/macros/matchit.vim")
  source $VIMRUNTIME/macros/matchit.vim
endif

if filereadable($VIM . "/addons/plugin/migemo.vim")
  source $VIM/addons/plugin/migemo.vim
  noremap // :<C-u>Migemo<CR>
endif

" ----------------------------------------------------------------------

let g:indent_guides_auto_colors=0
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size=1

if NeoBundleIsInstalled('vim-indent-guides')
  autocmd VimEnter,Colorscheme * :highlight IndentGuidesOdd  ctermbg=233
  autocmd VimEnter,Colorscheme * :highlight IndentGuidesEven ctermbg=236
endif

" ----------------------------------------------------------------------

map sy <Plug>(fakeclip-y)
map sd <Plug>(fakeclip-d)
nmap syy <Plug>(fakeclip-Y)
nmap sdd <Plug>(fakeclip-dd)
nmap sp <Plug>(fakeclip-p)
nmap sP <Plug>(fakeclip-P)

" ----------------------------------------------------------------------

let g:yankring_history_dir = expand('$HOME/.vim/private')
let g:yankring_persist = 1

" ----------------------------------------------------------------------

let g:closetag_html_style=1

inoremap <C-]> <C-R>=GetCloseTag()<CR><ESC>F<i
map <C-]> a<C-]><ESC>

" ----------------------------------------------------------------------

" :Search keyword
" :SearchReset
" :SearchBuffers keyword
" :SearchBuffersReset

let g:MultipleSearchMaxColors = 10

" ----------------------------------------------------------------------

"let g:fuf_useMigemo = 1

let g:fuf_patternSeparator = ' '
let g:fuf_modesDisable = ['mrucmd']
let g:fuf_file_exclude = '\v\.DS_Store|\.git|\.svn|\.swp'
let g:fuf_mrufile_exclude = '\v\.DS_Store|\.git|\.svn|\.swp'
let g:fuf_enumeratingLimit = 20

nnoremap <silent> <C-f><C-f> :FufRenewCache<CR>:FufFileWithCurrentBufferDir!<CR>

" ----------------------------------------------------------------------

" :TagbarToggle

set updatetime=1000
set tags=./tags,./TAGS,tags,TAGS;/

nmap <F8> :TagbarToggle<CR>

" Command mode
" ======================================================================

" FIXME
" :vert diffsplit と ]c [c dp do
" :.,$s/\([^ -~、。]\)\([!#-~]\)\|\([!-~]\)\([^ -~、。]\)/\1\3 \2\4/gc
" :.,$s/\( [^ -~、。]\)\([A-Za-z]\)/\1 \2/gc
" :.,$s/\( [A-Za-z]\)\([^ -~、。]\)/\1 \2/gc

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
" C-o	Move to old cursor place
" C-i	Move to new cursor place
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
"       C-n     Completion
"       C-p     Completion (move to backward)

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

colorscheme default

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

if exists('&cursorline')
  set cursorline
endif
if exists('&cursorcolumn')
  set cursorcolumn
  autocmd VimEnter,Colorscheme * :highlight CursorColumn cterm=bold ctermbg=none
endif

set textwidth=0
if exists('&colorcolumn')
  set textwidth=80
  set colorcolumn=+1
  autocmd VimEnter,Colorscheme * :highlight ColorColumn ctermbg=darkgrey guibg=darkgrey
endif

let g:markdown_fenced_languages = [
\  'c',
\  'css',
\  'erb=eruby',
\  'go',
\  'javascript',
\  'js=javascript',
\  'json=javascript',
\  'perl',
\  'python',
\  'ruby',
\  'sass',
\  'sh',
\  'xml',
\]

if has("syntax") && (&t_Co > 2 || has("gui_running"))
  syntax on

  autocmd VimEnter,Colorscheme * :highlight default link TagName Search

  function! ActivateInvisibleCharIndicator()
    syntax match InvisibleJISX0208Space "　" display containedin=ALL
    autocmd VimEnter,Colorscheme * :highlight InvisibleJISX0208Space ctermbg=DarkBlue guibg=DarkBlue
    syntax match InvisibleTrailingSpace "[ \t]\+$" display containedin=ALL
    autocmd VimEnter,Colorscheme * :highlight InvisibleTrailingSpace ctermbg=Red guibg=Red
  endf
  augroup vimrc
    autocmd BufNewFile,BufRead * call ActivateInvisibleCharIndicator()
  augroup END

  " Status line
  highlight StatusLine   term=NONE cterm=NONE ctermfg=yellow ctermbg=red
  highlight StatusLineNC term=NONE cterm=NONE ctermfg=black  ctermbg=white

  highlight Search term=NONE cterm=NONE ctermfg=black  ctermbg=grey
  highlight IncSearch term=NONE cterm=NONE ctermfg=black  ctermbg=yellow

  autocmd FileType go :highlight goErr cterm=bold ctermfg=214
  autocmd FileType go :match goErr /\<err\>/
endif

" ======================================================================

set directory^=~/var/vim
if version >= 703
  set undodir=~/var/vim/undodir
  set undofile
endif

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
set formatoptions=
set nrformats-=octal
set nobackup
set noendofline
set cinoptions=:0
set comments=
set history=10000
set wildmenu
set wildmode=longest:full

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

" Key binding
" ======================================================================

map - $
"map t8 :set tabstop=8
"map t4 :set tabstop=4
"map gg :s/.*/\/\*&\*\//            " コメントで囲む
"map hh :s/^\/\*\(.*\)\*\/$/\1/     " コメントを外す

nmap <Esc><Esc> :nohlsearch<CR><Esc>

" タグジャンプ時にタグが複数あったらリスト表示
" カーソルがウィンドウの中心行になるようにジャンプ
nnoremap <C-]> g<C-]>zz
" Global / Gtags
map  :GtagsCursor<CR>
"map <C-> :GtagsCursor<CR>
map <C-\> :Gtags -r <CR>
map <C-n> :cn<CR>
map <C-p> :cp<CR>
"map <C-x> :! sort -n
"map <C-C> :write !xclipx<CR><CR>
"map <C-V> :r! xclip -out<CR><CR>

function! ClipboardPaste() range
  let text = system("xclip -out")
  let pos = getpos(".")
  " FIXME: ???
  "if visualmode() == 'v'
  "  execute ":delete"
  "endif
  set paste
  execute ":normal i" . text
  set nopaste
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

" Misc auto commands
" ======================================================================

augroup vimrc
"  autocmd BufReadPre * setlocal noswapfile
"  autocmd BufEnter ~/Dropbox/* set directory^=~/var/vim
"  autocmd BufEnter * nested setlocal swapfile
"  autocmd BufLeave ~/Dropbox/* set directory-=~/var/vim
"
  autocmd FileType spec setlocal path=.,./../SOURCES,,

  autocmd BufNewFile,BufRead *.go setlocal noexpandtab shiftwidth=8
  autocmd BufNewFile,BufRead *.[ch] setlocal formatoptions=croql cindent comments=sr:/*,bm:*,el:*/,://
  autocmd BufNewFile *.rb 0r ~/.vim/template/ruby.rb
  autocmd BufNewFile *.py 0r ~/.vim/template/python.py
  autocmd BufNewFile *.pl 0r ~/.vim/template/perl.pl
  autocmd BufNewFile *.pm 0r ~/.vim/template/package.pm
  "autocmd BufNewFile *.pl execute "normal i#!/usr/bin/env perl\<CR>" | echo "New File"

  autocmd BufNewFile *.html 0r ~/.vim/template/strict.html
  autocmd BufNewFile *.html setlocal fileencoding=UTF-8
  autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
  autocmd BufRead /tmp/ldapvi-*/data setlocal filetype=ldif

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

" Status-line
" ======================================================================

set laststatus=2

if NeoBundleIsInstalled('lightline.vim')
  let g:lightline = {
    \ 'colorscheme': 'jellybeans',
    \ 'separator': { 'left': "", 'right': "" },
    \ 'subseparator': { 'left': "", 'right': "" },
    \ 'active': {
    \   'left': [ [ 'filename', 'paste' ] ],
    \   'right': [ [ 'lineinfo' ], [ 'fileformat', 'fileencoding', 'filetype' ] ]
    \ },
    \ 'inactive': {
    \   'left': [ [ 'filename', 'paste' ] ],
    \   'right': [ [ 'lineinfo' ], [ 'fileformat', 'fileencoding', 'filetype' ] ]
    \ },
    \ 'component_function': {
    \   'filename': 'LightLineFilename',
    \   'filetype': 'LightLineFiletype',
    \ },
    \ 'component_expand': {
    \   'lineinfo': 'LightLineLineinfo',
    \
    \ },
  \ }
  function! LightLineModified()
    return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
  endfunction
  function! LightLineReadonly()
    return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
  endfunction
  function! LightLineFilename()
    return ('' != LightLineReadonly() ? LightLineReadonly() . ' ' : '') .
      \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
      \  &ft == 'unite' ? unite#get_status_string() :
      \  &ft == 'vimshell' ? vimshell#get_status_string() :
      \ '' != expand('%') ? expand('%') : '[No Name]') .
      \ ('' != LightLineModified() ? ' ' . LightLineModified() : '')
  endfunction
  function! LightLineFiletype()
     return strlen(&filetype) ? &filetype : '?'
  endfunction
  function! LightLineLineinfo()
    return '%v,%l/%L'
  endfunction
else
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
  "set statusline=%<%f\ %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).']['.&ff.']'}%=%l,%c%V%8P
endif

" vimdiff
" ======================================================================

command! DiffOrig vert new | set bt=nofile | r ++edit # | 0d_| diffthis | wincmd p | diffthis

if NeoBundleIsInstalled('diffchar.vim')
  if &diff
    augroup vimrc
      autocmd!
      autocmd VimEnter * execute "%SDChar"
    augroup END
  endif
endif

highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=22
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=52
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17
highlight DiffText   cterm=bold ctermfg=10 ctermbg=21

if NeoBundleIsInstalled('vim-unified-diff')
  set diffexpr=unified_diff#diffexpr()
else
  " https://github.com/fumiyas/home-commands/blob/master/git-diff-normal

  let g:git_diff_normal="git-diff-normal"
  let g:git_diff_normal_opts=["--diff-algorithm=histogram"]

  function! GitDiffNormal()
    let args=[g:git_diff_normal]
    if &diffopt =~ "iwhite"
      call add(args, "--ignore-all-space")
    endif
    call extend(args, g:git_diff_normal_opts)
    call extend(args, [v:fname_in, v:fname_new])
    let cmd="!" . join(args, " ") . ">" . v:fname_out
    silent execute cmd
    redraw!
  endfunction

  if executable(g:git_diff_normal)
    call system(g:git_diff_normal)
    if v:shell_error == 0
      set diffexpr=GitDiffNormal()
    endif
  endif
endif

augroup DiffAutocommands
  autocmd!
  " Turn off diff mode automatically
  autocmd WinEnter * if (winnr('$') == 1) && (getbufvar(winbufnr(0), '&diff')) == 1 | diffoff | endif
augroup END

" ----------------------------------------------------------------------

if NeoBundleIsInstalled('syntastic')
  let g:syntastic_always_populate_loc_list=0
  let g:syntastic_auto_loc_list=1
  let g:syntastic_check_on_open=0
  let g:syntastic_check_on_wq=0
  let g:syntastic_loc_list_height=5
  let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
  let g:syntastic_spec_rpmlint_args = "-o 'NetworkEnabled False'"
  " SC1007: Remove space after = if trying to assign a value
  let g:syntastic_sh_shellcheck_args = "-e SC1007"
endif

