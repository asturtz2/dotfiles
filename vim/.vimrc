" Startup {{{

" Filetype templates
augroup NewFile
    autocmd!
    autocmd BufNewFile *.tex 0r ~/.vim/templates/template.tex
augroup end

"Strip all trailing whitespace from file on write
augroup WriteBuffer
    autocmd!
    autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
augroup end

augroup Quickfix
    autocmd!
    autocmd FileType qf :nnoremap <buffer> <CR> <CR><C-w>o
augroup END

"Create file marks for the last file viewed of a given type
augroup LeaveBuffer
    autocmd!
    autocmd BufLeave *.vim* normal! mV
    autocmd BufLeave *.hs normal! mH
    autocmd BufLeave *.c normal! mC
    autocmd BufLeave *.cpp normal! mC
    autocmd BufLeave *.js normal! mJ
    autocmd BufLeave *.rb normal! mR
    autocmd BufLeave *.css normal! mS
    autocmd BufLeave *.tex normal! mT
    autocmd BufLeave *.md normal! mM
augroup end

" }}}

" Plugins {{{

map <Space>pi :PlugInstall<CR>
map <Space>pu :PlugUpdate<CR>
map <Space>ps :PlugStatus<CR>
map <Space>pc :PlugClean<CR>

" Installation{{{
call plug#begin()

Plug 'tpope/vim-abolish'
Plug 'tpope/vim-apathy'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-obsession'
Plug 'sheerun/vim-polyglot'
Plug 'tommcdo/vim-lion'
Plug 'rstacruz/vim-closer'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'wellle/targets.vim'
Plug 'romainl/vim-cool'
Plug 'romainl/vim-qf'
Plug 'romainl/vim-tinyMRU'
Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' }
Plug 'ervandew/supertab'
" Plug 'SirVer/ultisnips'
" Plug 'honza/vim-snippets'
Plug 'junegunn/goyo.vim', { 'on' : 'Goyo' }
Plug 'unblevable/quick-scope'
Plug 'christoomey/vim-tmux-navigator'
" Plug 'Shougo/vimproc.vim', { 'do' : 'make' }

"Haskell
Plug 'itchyny/vim-haskell-indent', { 'for' : 'haskell' }
Plug 'neovimhaskell/haskell-vim', { 'for' : 'haskell' }
" Plug 'eagletmt/ghcmod-vim', { 'for' : 'haskell' }

" Latex
Plug 'lervag/vimtex', { 'for' : ['tex', 'plaintex', 'latex'] }

" Wal
if executable('wal')
    Plug 'dylanaraps/wal'
endif

call plug#end()
" }}}

" Options {{{

" Quick-scope
let g:qs_highlight_on_keys = ['f','F','t','T']

" Incsearch.vim
set hlsearch

" nnoremap <Space>c :CtrlPDir<CR>
" let g:ctrlp_show_hidden = 1 "Show hidden files in control p
" let g:ctrlp_user_command = 'ag %s -l -g "" -f --nocolor --hidden --path-to-ignore ~/.ignore'
" let g:ctrlp_working_path_mode = 0

" Supertab
let g:SuperTabDefaultCompletionType = "context"

" Gundo
map <Space>u :GundoToggle<CR>

"Vimtex
let g:vimtex_complete_close_braces = 1
let g:vimtex_view_method = 'zathura'
let g:vimtex_indent_enabled = 0

"Haskell-vim
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskell_indent_disable = 0

" Ale
let g:lint_on_save = 1
let g:lint_on_text_changed = 'never'
let g:lint_on_filetype_changed = 0
" Wal
if executable('wal')
    colorscheme wal
endif
" }}}
"}}}

" Changes {{{
nnoremap Y y$

"Augment vim-surround with a 'space' text object
nnoremap ds<space> F<space>xf<space>x

"Swap the word under the cursor with the next occurring word to the left (h)
"or to the right (l), ignoring non-alphanumeric characters.
nnoremap <Space>wh "_yiw?\v\w+\_W+%#<CR>:s/\v(%#\w+)(\_W+)(\w+)/\3\2\1/<CR><C-o><C-l>:nohl<CR>
nnoremap <Space>wl "_yiw:s/\v(%#\w+)(\_W+)(\w+)/\3\2\1/<CR><C-o>/\v\w+\_W+<CR><C-l>:nohl<CR>
" }}}

" Movement {{{

" Go to beginning of last visual selection
nnoremap gV `[v`

nnoremap <Home> ^
nnoremap <End> $

nnoremap [[ :cprevious<CR>
nnoremap ]] :cnext<CR>

nnoremap <Space>' :marks<CR>:normal!<Space>'

" "in line" (entire line sans white-space; cursor at beginning--ie, ^)
xnoremap <silent> il :<c-u>normal! g_v^<cr>
onoremap <silent> il :<c-u>normal! g_v^<cr>

" "around line" (entire line sans trailing newline; cursor at beginning--ie, 0)
xnoremap <silent> al :<c-u>normal! $v0<cr>
onoremap <silent> al :<c-u>normal! $v0<cr>

" "in number" (next number after cursor on current line)
xnoremap <silent> in :<c-u>call <sid>inNumber()<cr>
onoremap <silent> in :<c-u>call <sid>inNumber()<cr>

" "around number" (next number on line and possible surrounding white-space)
xnoremap <silent> an :<c-u>call <sid>aroundNumber()<cr>
onoremap <silent> an :<c-u>call <sid>aroundNumber()<cr>

" "in indentation" (indentation level sans any surrounding empty lines)
xnoremap <silent> ii :<c-u>call <sid>inIndentation()<cr>
onoremap <silent> ii :<c-u>call <sid>inIndentation()<cr>

" "around indentation" (indentation level and any surrounding empty lines)
xnoremap <silent> ai :<c-u>call <sid>aroundIndentation()<cr>
onoremap <silent> ai :<c-u>call <sid>aroundIndentation()<cr>

xnoremap $ $h
" }}}
"
" Search/Replace {{{

"Displays all lines containing the word in the file. Mnemonic: Global
"Occurrences or Go to Occurrences. The word will be remembered as the last
"search pattern used, so n/N will work, for example.
nnoremap go "gyiw:g/<C-R>g/z#.2<CR>

"Replace every occurrence of the word under the cursor in the paragraph.
nnoremap <Space>sp :'{,'}s/\<<C-r>=expand('<cword>')<CR>\>//g<left><left>
nnoremap <Space>sf :%s/\<<C-r>=expand('<cword>')<CR>\>//g<left><left>
nnoremap <Space>sl :s/\<<C-r>=expand('<cword>')<CR>\>//g<left><left>


if has('nvim')
    set inccommand=nosplit "Show substitution matches/replacements dynamically
endif
set incsearch "Search as characters are entered
set hlsearch "Highlight search matches
set ignorecase
set smartcase
"}}}

" Buffers {{{
nnoremap <Space>a <C-^>
nnoremap <Space>d :ls<CR>:bd<Space>
nnoremap <Space>b :ls<CR>:b **<Left>
nnoremap <Space>r :ME<Space>
nnoremap <PageUp> :bnext<CR>
nnoremap <PageDown> :bprev<CR>

" }}}

"{{{ Splits
set splitright
"}}}

" Files {{{
nnoremap <Space>f :find <C-R>=expand(getcwd()).'/**/*'<CR>
nnoremap <Space>F :find *
nnoremap <Space>e :edit <C-R>=fnameescape(expand('%:p:h')).'/'<CR>
nnoremap <Space>cf :cd **/*
nnoremap <Space>ch :cd<CR>:pwd<CR>
nnoremap <Space>cc :cd %:p:h<CR>:pwd<CR>
command! -nargs=+ -complete=file_in_path -bar Grep  silent! grep! <args> <bar> redraw! <bar> cwindow
nnoremap <silent> <Space>gw :grep <C-r><C-w><CR>
nnoremap <Space>gg :Grep<Space>

set path&
let &path .= "**"

set wildignore+=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*,bundle.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/min/*,*/vendor/*
set wildignore+=*/node_modules/*,*/bower_components/*
set wildignore+=*/java/*,*/target/*,*/out/*
set wildignore+=tags,cscope.*
set wildignore+=*.tar.*
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.aux,*.log,*.fdb*,*.fls
set wildignorecase
set wildmode=full
set wildmenu

" if executable("ag")
"     set grepprg=ag\ --nogroup\ --nocolor\ --ignore-case\ --vimgrep
"     set grepformat^=%f:%l:%c:%m
" endif
if executable("rg")
    set grepprg=rg\ --vimgrep
    set grepformat^=%f:%l:%c:%m
endif

let g:tex_flavor = "latex"
" }}}
"
" Terminals {{{
if has('nvim')
    tnoremap <Esc> <C-\><C-N>
endif
" }}}

" Insert mode {{{
" inoremap (; (<CR>);<C-c>O<Tab>
" inoremap (, (<CR>),<C-c>O<Tab>
" inoremap {; {<CR>};<C-c>O<Tab>
" inoremap {, {<CR>},<C-c>O<Tab>
" inoremap [; [<CR>];<C-c>O<Tab>
" inoremap [, [<CR>],<C-c>O<Tab>
"}}}

"{{{ Command mode
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
"}}}

" Statusline {{{
" set statusline=%<\ %f\ %m%r%y%w%=%l\/%-6L\ %3c\
hi User1 ctermbg=7 ctermfg=0 cterm=bold
hi User2 ctermbg=7 ctermfg=0
set statusline=
set statusline+=%1*
"set statusline+=%<\ %.20f "Filename
set statusline+=\ %{expand('%:~:.')} "Filename
set statusline+=\ %m "Modified flag
set statusline+=%= "Switch to right-hand side
set statusline+=%2*
set statusline+=%{strlen(&ft)?&ft:'none'}\ \|
set statusline+=\ %{&fileformat}\ \  "File format
set statusline+=%1*
set statusline+=\ (%l,\ %c):\%P\ \  "Line number
set laststatus=2
" }}}

" Folding {{{
set foldenable "Enable folding
set foldopen=all " Open folds on enter
set foldclose=all " Close folds on exit
set foldlevel=5 "Autofold everything by default
set foldnestmax=5 "Only fold toplevel
set foldmethod=indent
" }}}

" {{{ Tags
set tags=./tags,tags,~/.vim/tags/*;
" }}}

" Completion {{{
set completeopt=longest,menuone
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <C-p> pumvisible() ? '<C-p>' :
  \ '<C-p><C-r>=pumvisible() ? "\<lt>Up>" : ""<CR>'
" inoremap  
" inoremap  

" let s:python_version = 3
" }}}

" Undo {{{
set undofile
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000
" }}}

" {{{ Git
nnoremap <Space>vs :Gstatus<CR>
nnoremap <Space>vb :!git branch<CR>
nnoremap <Space>vc :!git commit -am ""<Left>
" }}}
" {{{ UI
set showmatch "Show matching characters (parentheses, brackets, etc.)
" }}}

" Misc {{{
map <Space>~ :so ~/.vimrc<CR>
map <Space>z :!zathura * &<Left><Left>
nnoremap <Space>m :make<CR>

filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set showmatch
set textwidth=80
set columns=80
set ttimeout
set ttimeoutlen=20
let loaded_netrwPlugin=1 "Do not load netrw
set tabstop=8
set shiftwidth=4
set softtabstop=4
set smarttab
set expandtab
"}}}


" Custom functions {{{

" Strips trailing whitespace at the end of files. this
" is called on buffer write in the autogroup above.
function! <SID>StripTrailingWhitespaces()
    " save last search & cursor position
    let _s=@/
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    let @/=_s
    call cursor(l, c)
endfunction

" regular expressions that match numbers (order matters .. keep '\d' last!)
" note: \+ will be appended to the end of each
let s:regNums = [ '0b[01]', '0x\x', '\d' ]

function! s:inNumber()
	" select the next number on the line
	" this can handle the following three formats (so long as s:regNums is
	" defined as it should be above this function):
	"   1. binary  (eg: "0b1010", "0b0000", etc)
	"   2. hex     (eg: "0xffff", "0x0000", "0x10af", etc)
	"   3. decimal (eg: "0", "0000", "10", "01", etc)
	" NOTE: if there is no number on the rest of the line starting at the
	"       current cursor position, then visual selection mode is ended (if
	"       called via an omap) or nothing is selected (if called via xmap)

	" need magic for this to work properly
	let l:magic = &magic
	set magic

	let l:lineNr = line('.')

	" create regex pattern matching any binary, hex, decimal number
	let l:pat = join(s:regNums, '\+\|') . '\+'

	" move cursor to end of number
	if (!search(l:pat, 'ce', l:lineNr))
		" if it fails, there was not match on the line, so return prematurely
		return
	endif

	" start visually selecting from end of number
	normal! v

	" move cursor to beginning of number
	call search(l:pat, 'cb', l:lineNr)

	" restore magic
	let &magic = l:magic
endfunction

function! s:aroundNumber()
	" select the next number on the line and any surrounding white-space;
	" this can handle the following three formats (so long as s:regNums is
	" defined as it should be above these functions):
	"   1. binary  (eg: "0b1010", "0b0000", etc)
	"   2. hex     (eg: "0xffff", "0x0000", "0x10af", etc)
	"   3. decimal (eg: "0", "0000", "10", "01", etc)
	" NOTE: if there is no number on the rest of the line starting at the
	"       current cursor position, then visual selection mode is ended (if
	"       called via an omap) or nothing is selected (if called via xmap);
	"       this is true even if on the space following a number

	" need magic for this to work properly
	let l:magic = &magic
	set magic

	let l:lineNr = line('.')

	" create regex pattern matching any binary, hex, decimal number
	let l:pat = join(s:regNums, '\+\|') . '\+'

	" move cursor to end of number
	if (!search(l:pat, 'ce', l:lineNr))
		" if it fails, there was not match on the line, so return prematurely
		return
	endif

	" move cursor to end of any trailing white-space (if there is any)
	call search('\%'.(virtcol('.')+1).'v\s*', 'ce', l:lineNr)

	" start visually selecting from end of number + potential trailing whitspace
	normal! v

	" move cursor to beginning of number
	call search(l:pat, 'cb', l:lineNr)

	" move cursor to beginning of any white-space preceding number (if any)
	call search('\s*\%'.virtcol('.').'v', 'b', l:lineNr)

	" restore magic
	let &magic = l:magic
endfunction

function! s:inIndentation()
	" select all text in current indentation level excluding any empty lines
	" that precede or follow the current indentationt level;
	"
	" the current implementation is pretty fast, even for many lines since it
	" uses "search()" with "\%v" to find the unindented levels
	"
	" NOTE: if the current level of indentation is 1 (ie in virtual column 1),
	"       then the entire buffer will be selected
	"
	" WARNING: python devs have been known to become addicted to this

	" magic is needed for this
	let l:magic = &magic
	set magic

	" move to beginning of line and get virtcol (current indentation level)
	" BRAM: there is no searchpairvirtpos() ;)
	normal! ^
	let l:vCol = virtcol(getline('.') =~# '^\s*$' ? '$' : '.')

	" pattern matching anything except empty lines and lines with recorded
	" indentation level
	let l:pat = '^\(\s*\%'.l:vCol.'v\|^$\)\@!'

	" find first match (backwards & don't wrap or move cursor)
	let l:start = search(l:pat, 'bWn') + 1

	" next, find first match (forwards & don't wrap or move cursor)
	let l:end = search(l:pat, 'Wn')

	if (l:end !=# 0)
		" if search succeeded, it went too far, so subtract 1
		let l:end -= 1
	endif

	" go to start (this includes empty lines) and--importantly--column 0
	execute 'normal! '.l:start.'G0'

	" skip empty lines (unless already on one .. need to be in column 0)
	call search('^[^\n\r]', 'Wc')

	" go to end (this includes empty lines)
	execute 'normal! Vo'.l:end.'G'

	" skip backwards to last selected non-empty line
	call search('^[^\n\r]', 'bWc')

	" go to end-of-line 'cause why not
	normal! $o

	" restore magic
	let &magic = l:magic
endfunction

function! s:aroundIndentation()
	" select all text in the current indentation level including any emtpy
	" lines that precede or follow the current indentation level;
	"
	" the current implementation is pretty fast, even for many lines since it
	" uses "search()" with "\%v" to find the unindented levels
	"
	" NOTE: if the current level of indentation is 1 (ie in virtual column 1),
	"       then the entire buffer will be selected
	"
	" WARNING: python devs have been known to become addicted to this

	" magic is needed for this (/\v doesn't seem work)
	let l:magic = &magic
	set magic

	" move to beginning of line and get virtcol (current indentation level)
	" BRAM: there is no searchpairvirtpos() ;)
	normal! ^
	let l:vCol = virtcol(getline('.') =~# '^\s*$' ? '$' : '.')

	" pattern matching anything except empty lines and lines with recorded
	" indentation level
	let l:pat = '^\(\s*\%'.l:vCol.'v\|^$\)\@!'

	" find first match (backwards & don't wrap or move cursor)
	let l:start = search(l:pat, 'bWn') + 1

	" NOTE: if l:start is 0, then search() failed; otherwise search() succeeded
	"       and l:start does not equal line('.')
	" FORMER: l:start is 0; so, if we add 1 to l:start, then it will match
	"         everything from beginning of the buffer (if you don't like
	"         this, then you can modify the code) since this will be the
	"         equivalent of "norm! 1G" below
	" LATTER: l:start is not 0 but is also not equal to line('.'); therefore,
	"         we want to add one to l:start since it will always match one
	"         line too high if search() succeeds

	" next, find first match (forwards & don't wrap or move cursor)
	let l:end = search(l:pat, 'Wn')

	" NOTE: if l:end is 0, then search() failed; otherwise, if l:end is not
	"       equal to line('.'), then the search succeeded.
	" FORMER: l:end is 0; we want this to match until the end-of-buffer if it
	"         fails to find a match for same reason as mentioned above;
	"         again, modify code if you do not like this); therefore, keep
	"         0--see "NOTE:" below inside the if block comment
	" LATTER: l:end is not 0, so the search() must have succeeded, which means
	"         that l:end will match a different line than line('.')

	if (l:end !=# 0)
		" if l:end is 0, then the search() failed; if we subtract 1, then it
		" will effectively do "norm! -1G" which is definitely not what is
		" desired for probably every circumstance; therefore, only subtract one
		" if the search() succeeded since this means that it will match at least
		" one line too far down
		" NOTE: exec "norm! 0G" still goes to end-of-buffer just like "norm! G",
		"       so it's ok if l:end is kept as 0. As mentioned above, this means
		"       that it will match until end of buffer, but that is what I want
		"       anyway (change code if you don't want)
		let l:end -= 1
	endif

	" finally, select from l:start to l:end
	execute 'normal! '.l:start.'G0V'.l:end.'G$o'

	" restore magic
	let &magic = l:magic
endfunction
"Custom commands {{{
"A better oldfiles
command! Bro :enew | setl buftype=nofile |  setl nobuflisted | 0put =v:oldfiles
  \| nnoremap <buffer> <CR> gf | 1
"}}}

"}}}

" vim:foldmethod=marker:foldlevel=0
