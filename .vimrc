" .vimrc
" ======================================================================

" Normal mode
"	Open previous file	Ctrl+^
"	Move to previous place	Ctrl+o
"	Move to next place	Ctrl+i

" Insert mode
"	Add indent		Ctrl+t
"	Delete indent		Ctrl+d
"	Complete word		Ctrl+n
"	Complete word		Ctrl+p

" ======================================================================

augroup vimrc
  autocmd!
augroup END

scriptencoding UTF-8
" http://vim.sf.net/
" http://pcmania.jp/~moraz/basics.html
" http://www.kawaz.jp/pukiwiki/?vim

"set viminfo+=!

if !exists(expand('$HOME/.vim/private'))
  silent !umask 077; mkdir -p "$HOME/.vim/private"
endif

" ======================================================================

" vim からの制御シーケンスの使用例
" https://ttssh2.osdn.jp/manual/ja/usage/tips/vim.html

" Cursor mode in insert mode: box, blink
let &t_SI .= "\e[1 q"
" Cursor mode in command mode: box, no blink
let &t_EI .= "\e[2 q"

if &term =~ '^\(x\|ml\)term'
    " クリップボードからの貼り付け時に自動インデントを無効
    let &t_SI .= "\e[?2004h"
    let &t_EI .= "\e[?2004l"
    function! XTermPasteBegin(ret)
        set pastetoggle=<Esc>[201~
        set paste
        return a:ret
    endfunction
    inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

" Plugins
" ======================================================================

" Dein.vim
" ----------------------------------------------------------------------

" Update packages	:call dein#update()
" Show update log	:echo dein#get_updates_log()

if v:version >= 800
  let dein_dir=expand('~/git/vim/dein.vim')
  let dein_cache_dir=expand('~/.cache/dein.vim')
else
  let dein_dir=expand('~/git/vim/dein.vim-1.5')
  let dein_cache_dir=expand('~/.cache/dein.vim-1.5')
endif

if isdirectory(dein_dir)
  let &runtimepath.=','.dein_dir

  call dein#begin(dein_cache_dir)

  call dein#add('Shougo/dein.vim')
  call dein#add('Shougo/vimproc.vim', {'build': 'make'})

  call dein#add('Shougo/unite.vim')
  call dein#add('Shougo/neocomplete.vim')
  call dein#add('Shougo/neomru.vim')
  "call dein#add('Shougo/neosnippet')

  call dein#add('itchyny/lightline.vim.git')
  call dein#add('koron/verifyenc-vim.git')
  "call dein#add('lambdalisue/vim-unified-diff')
  call dein#add('rickhowe/diffchar.vim')
  call dein#add('nathanaelkane/vim-indent-guides')
  call dein#add('takubo/BlockDiff')
  call dein#add('vim-scripts/MultipleSearch')
  call dein#add('kana/vim-fakeclip')
  call dein#add('AndrewRadev/inline_edit.vim')
  call dein#add('tpope/vim-fugitive')
  call dein#add('scrooloose/syntastic.git')

  call dein#add('godlygeek/tabular')
  call dein#add('pearofducks/ansible-vim')
  call dein#add('joker1007/vim-markdown-quote-syntax')
  call dein#add('rcmdnk/vim-markdown')

  call dein#end()

  function! s:dein_tap(name)
    return dein#tap(a:name)
  endfunction
else
  function! s:dein_tap(name)
    return 0
  endfunction
endif

"    NeoBundle 'YankRing.vim'
"    NeoBundle 'closetag.vim'
"    NeoBundle 'L9'
"    NeoBundle 'Shougo/neoyank.vim'
"    NeoBundle 'Tagbar'
"    NeoBundle 'andviro/flake8-vim'
"    "NeoBundle 'https://github.com/dhruvasagar/vim-table-mode/'

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

if s:dein_tap('vim-indent-guides')
  let g:indent_guides_auto_colors=0
  let g:indent_guides_enable_on_vim_startup=1
  let g:indent_guides_guide_size=1
  augroup vimrc
    autocmd Colorscheme * highlight IndentGuidesOdd  ctermbg=235
    autocmd Colorscheme * highlight IndentGuidesEven ctermbg=237
  augroup END
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

" Vimでunite.vimプラグインを使い始めて一週間 - アインシュタインの電話番号
" http://blog.ruedap.com/2011/01/17/vim-unite-plugin-1-week

" VimのUniteプラグインでファイル、バッファ、ブックマーク管理 | karakaram-blog
" http://www.karakaram.com/unite

if s:dein_tap('unite.vim')
  nnoremap	[unite]		<Nop>
  nmap		<Space>f	[unite]

  call unite#filters#matcher_default#use(['matcher_fuzzy'])

  " 起動時にインサートモードで開始
  let g:unite_enable_start_insert = 1

  " 現在開いているファイルのディレクトリ下のファイル一覧
  " 開いていない場合はカレントディレクトリ
  nnoremap <silent> [unite]f :<C-u>UniteWithBufferDir -buffer-name=files file file/new<CR>
  " バッファ一覧
  nnoremap <silent> [unite]b :<C-u>Unite buffer<CR>
  " レジスタ一覧
  nnoremap <silent> [unite]r :<C-u>Unite -buffer-name=register register<CR>
  " 最近使用したファイル一覧
  nnoremap <silent> [unite]m :<C-u>Unite file_mru<CR>
  " ブックマーク一覧
  nnoremap <silent> [unite]k :<C-u>Unite bookmark<CR>
  " ブックマークに追加
  nnoremap <silent> [unite]a :<C-u>UniteBookmarkAdd<CR>

  " unite を開いている間のキーマッピング
  augroup vimrc
    autocmd FileType unite call s:unite_my_settings()
  augroup END
  function! s:unite_my_settings()"{{{
    " ESCでuniteを終了
    nmap <buffer> <ESC> <Plug>(unite_exit)
    " 入力モードのとき Ctrl+w でバックスラッシュも削除
    " 空のときは親ディレクトリへ移動
    imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  endfunction"}}}
endif

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
" :Ctrl-f
" /Ctrl-f

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

" Command-line window
" ======================================================================

augroup vimrc
  " Remove bogus history in the command-line window
  autocmd CmdwinEnter : g/^qa\?!\?$/d
  autocmd CmdwinEnter : g/^wq\?a\?!\?$/d
  autocmd CmdwinEnter : g/^\(n\|rew\|sp\)$/d
augroup END

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

" Enabled file type detection
" Use the default filetype settings. If you also want to load indent files
" to automatically do language-dependent indenting add 'indent' as well.
filetype plugin on

"syn region myFold start="{" end="}" transparent fold
"set foldnestmax=1
"set foldmethod=syntax

" Highlighting
" ======================================================================

if &diff
  syntax off
  " vimdiff is very slow with cursorline and/or cursorcolumn
  if exists('&cursorline')
    set nocursorline
  endif
  if exists('&cursorcolumn')
    set nocursorcolumn
  endif
else
  syntax enable
  if exists('&cursorline')
    set cursorline
    augroup vimrc
      autocmd Colorscheme * highlight CursorLine cterm=bold ctermbg=235
    augroup END
  endif
  if exists('&cursorcolumn')
    set cursorcolumn
    augroup vimrc
      autocmd Colorscheme * highlight CursorColumn cterm=bold ctermbg=235
    augroup END
  endif
endif

set textwidth=0
if exists('&colorcolumn')
  set textwidth=80
  set colorcolumn=+1
  augroup vimrc
    autocmd Colorscheme * highlight ColorColumn ctermbg=235
  augroup END
endif

augroup vimrc
  autocmd Colorscheme * highlight default link TagName Search
  autocmd Colorscheme * highlight StatusLine   term=NONE cterm=NONE ctermfg=yellow ctermbg=red
  autocmd Colorscheme * highlight StatusLineNC term=NONE cterm=NONE ctermfg=black  ctermbg=white
  autocmd Colorscheme * highlight Search ctermfg=lightgreen ctermbg=darkyellow cterm=bold
  autocmd Colorscheme * highlight IncSearch term=NONE cterm=NONE ctermfg=black  ctermbg=yellow

  autocmd FileType go match goErr /\<err\>/
  autocmd FileType go highlight goErr cterm=bold ctermfg=214

  autocmd VimEnter * syntax match JISX0208Space "　" containedin=ALL display
  autocmd Colorscheme * highlight JISX0208Space ctermbg=DarkBlue guibg=DarkBlue
  autocmd VimEnter * syntax match TrailingSpace "[ \t]\+$" containedin=ALL display
  autocmd Colorscheme * highlight TrailingSpace ctermbg=DarkRed guibg=DarkRed
augroup END

let g:vim_markdown_folding_disabled=1
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
\  'yaml',
\  'json',
\]

if s:dein_tap('vim-markdown')
  let g:markdown_quote_syntax_filetypes = {
	  \ "go" : {
	  \   "start" : "go",
	  \},
	  \ "yaml" : {
	  \   "start" : "yaml",
	  \},
	  \ "json" : {
	  \   "start" : "json",
	  \},
	  \ "vb" : {
	  \   "start" : "vb",
	  \},
  \}
endif

" ======================================================================

set directory^=~/var/vim//
set undodir=~/var/vim/undodir
set undofile

set shortmess+=I

set modeline

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
set noexpandtab
set tabstop=8
set softtabstop=2
set shiftwidth=2
set shiftround
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

set cryptmethod=blowfish2

set encoding=UTF-8
set termencoding=UTF-8
set fileencodings=UCS-BOM,UTF-8

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

let &fileformats=$VIM_FILEFORMATS

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

" Search selected string in Visual mode
vnoremap * "zy:let @/ = @z<CR>n

" Misc auto commands
" ======================================================================

augroup vimrc
"  autocmd BufReadPre * setlocal noswapfile
"  autocmd BufEnter ~/Dropbox/* set directory^=~/var/vim
"  autocmd BufEnter * nested setlocal swapfile
"  autocmd BufLeave ~/Dropbox/* set directory-=~/var/vim
"
  autocmd FileType spec setlocal path=.,./../SOURCES,,
  autocmd FileType yaml setlocal indentexpr=

  autocmd BufNewFile *.md 0r ~/.vim/template/template.md
  autocmd BufNewFile,BufRead *.go setlocal noexpandtab shiftwidth=8
  autocmd BufNewFile,BufRead *.[ch] setlocal formatoptions=croql cindent comments=sr:/*,bm:*,el:*/,://
  autocmd BufNewFile *.bash 0r ~/.vim/template/template.bash
  autocmd BufNewFile *.rb 0r ~/.vim/template/template.rb
  autocmd BufNewFile *.py 0r ~/.vim/template/template.py
  autocmd BufNewFile,BufRead *.py setlocal expandtab
  autocmd BufNewFile *.pl 0r ~/.vim/template/template.pl
  autocmd BufNewFile *.pm 0r ~/.vim/template/template.pm
  "autocmd BufNewFile *.pl execute "normal i#!/usr/bin/env perl\<CR>" | echo "New File"

  autocmd BufNewFile *.html 0r ~/.vim/template/template.html
  autocmd BufNewFile *.html setlocal fileencoding=UTF-8
  autocmd BufNewFile,BufRead *.md setlocal filetype=markdown expandtab
  if s:dein_tap('ansible-vim')
    autocmd BufNewFile,BufRead *.yml setlocal filetype=yaml.ansible
    autocmd BufNewFile,BufRead *.yaml setlocal filetype=yaml.ansible
  endif
  autocmd BufRead /tmp/ldapvi-*/data setlocal filetype=ldif

  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt if getline(1) =~ "Last modification: "
  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt normal msgg/Last modification: /e+1"_D"=strftime("%Y %b %d")p`s
  "autocmd BufWritePre,FileWritePre *vim/vim/runtime/doc/*.txt endif
augroup END

"augroup vimrc
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

if s:dein_tap('lightline.vim')
  let g:lightline = {
    \ 'colorscheme': 'PaperColor',
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

if s:dein_tap('diffchar.vim')
  let g:DiffUnit = "Word3"
  if &diff
    augroup vimrc
      autocmd VimEnter * execute "%SDChar"
    augroup END
  endif
endif

highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=22
highlight DiffDelete cterm=bold ctermfg=10 ctermbg=52
highlight DiffChange cterm=bold ctermfg=10 ctermbg=17
highlight DiffText   cterm=bold ctermfg=10 ctermbg=21

if match(split(&diffopt, ','), 'internal') != -1
  set diffopt+=algorithm:minimal,indent-heuristic
elseif s:dein_tap('vim-unified-diff')
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

augroup vimrc
  " Turn off diff mode automatically
  autocmd WinEnter * if (winnr('$') == 1) && (getbufvar(winbufnr(0), '&diff')) == 1 | diffoff | endif
augroup END

" ----------------------------------------------------------------------

if s:dein_tap('syntastic')
  let g:syntastic_always_populate_loc_list=0
  let g:syntastic_auto_loc_list=1
  let g:syntastic_check_on_open=0
  let g:syntastic_check_on_wq=0
  let g:syntastic_loc_list_height=5

  let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
  let g:syntastic_spec_rpmlint_args = "-o 'NetworkEnabled False'"

  " SC1007: Remove space after = if trying to assign a value
  " SC2039: In POSIX sh, 'type' is not supported
  let g:syntastic_sh_shellcheck_args = "-e SC1007,SC2039"

  " 206: Variables should have spaces before and after: {{ var_name }}
  let g:syntastic_ansible_ansible_lint_args = "-x 206"

  if executable('python3')
    let g:syntastic_python_checkers = ['python3', 'flake8']
  endif
  " E266: too many leading '#' for block comment
  " E501: line too long (82 > 79 characters)
  let g:syntastic_python_flake8_args = "--ignore=E266,E501"
endif
highlight Search ctermfg=lightgreen ctermbg=darkyellow cterm=bold

" ======================================================================

colorscheme default

" For test:
" :e $VIMRUNTIME/syntax/colortest.vim
" :so %

"if &t_Co == '' && &term =~ '.*term$'
"  if has("terminfo")
"    set t_Co=8
"    set t_Sf=^[[3%p1%dm
"    set t_Sb=^[[4%p1%dm
"  else
"    set t_Co=8
"    set t_Sf=^[[3%dm
"    set t_Sb=^[[4%dm
"  endif
"endif
