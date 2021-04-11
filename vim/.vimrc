augroup Compiling
    autocmd!
    autocmd BufWritePost *.ts,*.tsx silent TsuAsyncGeterr
augroup end

augroup Templates
    autocmd!
    autocmd BufNewFile *.* silent! execute 'keepalt 0r ~/.vim/templates/template.'.expand("<afile>:e")
augroup end

"Strip all trailing whitespace from file on write
augroup WriteBuffer
    autocmd!
    autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
augroup end

set hlsearch

" Matchup
let g:matchup_matchparen_enabled = 0
let g:matchup_surround_enabled = 1

syntax on
" Wal
" if executable('wal')
"     packadd wal
"     colorscheme wal
" endif
colorscheme apprentice
set cursorline

nnoremap Y y$

"Augment vim-surround with a 'space' text object
nnoremap ds<space> F<space>xf<space>x

"Swap the word under the cursor with the next occurring word to the left (h)
"or to the right (l), ignoring non-alphanumeric characters.
nnoremap <Space>wh "_yiw?\v\w+\_W+%#<CR>:s/\v(%#\w+)(\_W+)(\w+)/\3\2\1/<CR><C-o><C-l>:nohl<CR>
nnoremap <Space>wl "_yiw:s/\v(%#\w+)(\_W+)(\w+)/\3\2\1/<CR><C-o>/\v\w+\_W+<CR><C-l>:nohl<CR>
nmap <silent> gs m':set opfunc=Substitute<CR>g@

" Text objects {{{
" 24 simple text-objects
" ----------------------
" i_ i. i: i, i; i| i/ i\ i* i+ i- i#
" a_ a. a: a, a; a| a/ a\ a* a+ a- a#
for char in [ '_', '.', ':', ',', ';', '<bar>', '/', '<bslash>', '*', '+', '-', '#' ]
    execute 'xnoremap i' . char . ' :<C-u>normal! T' . char . 'vt' . char . '<CR>'
    execute 'onoremap i' . char . ' :normal vi' . char . '<CR>'
    execute 'xnoremap a' . char . ' :<C-u>normal! F' . char . 'vf' . char . '<CR>'
    execute 'onoremap a' . char . ' :normal va' . char . '<CR>'
endfor

" buffer text-objects
" -------------------
" i% a%
xnoremap i% :<C-u>let z = @/\|1;/^./kz<CR>G??<CR>:let @/ = z<CR>V'z
onoremap i% :normal vi%<CR>
xnoremap a% GoggV
onoremap a% :normal va%<CR>

" square brackets text-objects
" ----------------------------
" ir ar
xnoremap ir i[
xnoremap ar a[
onoremap ir :normal vi[<CR>
onoremap ar :normal va[<CR>
" }}}

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

" Movement {{{

" Go to beginning of last visual selection
nnoremap gV `[v`

nnoremap <Home> ^
nnoremap <End> $

nnoremap [[ :cprevious<CR>
nnoremap ]] :cnext<CR>

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

set incsearch "Search as characters are entered
set hlsearch "Highlight search matches
set ignorecase
set smartcase
"}}}

" Buffers {{{
nnoremap <Space>a <C-^>
nnoremap <Space>d :ls<CR>:bd<Space>
nnoremap <Space>b :b<Space><C-R><C-W><CR>
nnoremap <Space>r :ME<Space>
nnoremap <PageUp> :bnext<CR>
nnoremap <PageDown> :bprev<CR>

" }}}

"{{{ Splits
set splitright
nnoremap <C-W><C-D> :vert<Space>dsp<Space><C-R>=expand('<cword>')<CR><CR>
"}}}

" Files {{{
nnoremap [a :prev <bar> args<CR>
nnoremap ]a :set nomore <bar> :next <bar> args<CR>
nnoremap <Space>f :find *<C-z><S-Tab>
nnoremap <Space>F :find <C-R>=expand(getcwd()).'/**/*'<CR><C-z><S-Tab>
nnoremap <Space>e :edit <C-R>=fnameescape(expand('%:p:h')).'/'<CR><C-z><S-Tab>
nnoremap <Space>cf :cd **/*
nnoremap <Space>ch :cd<CR>:pwd<CR>
nnoremap <Space>cc :cd %:p:h<CR>:pwd<CR>
" command! -nargs=+ -complete=file_in_path -bar Grep silent grep <args>
function! Grep(args)
    " return system(join([&grepprg, join(a:000)], ' '))
    let args = split(a:args, ' ')
    return system(join([&grepprg, args[0], len(args) > 1 ? join(args[1:-1], ' ') : ''], ' '))
endfunction
command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr Grep(<q-args>)
command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr Grep(<q-args>)
nnoremap <silent> <Space>gw :Grep<Space><C-R>=expand('<cword>' . ' ' . &path)<CR><CR>
nnoremap <Space>gg :Grep<Space>

set path=.,**

set wildignore=
set wildignore+=*.swp,*.bak
set wildignore+=*.pyc,*.class,*.sln,*.Master,*.csproj,*.csproj.user,*.cache,*.dll,*.pdb,*.min.*,bundle.*
set wildignore+=*/.git/**/*,*/.hg/**/*,*/.svn/**/*
set wildignore+=*/min/*,*/vendor/*
set wildignore+=*/node_modules/*,*/bower_components/*
set wildignore+=*/java/*,*/target/*,*/out/*
set wildignore+=*/dist/*
set wildignore+=tags,cscope.*
set wildignore+=*.tar.*
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.aux,*.log,*.fdb*,*.fls
set wildignorecase
set wildmode=full
set wildmenu
set wildcharm=<C-z>

if executable("rg")
    set grepprg=rg\ --vimgrep
    set grepformat^=%f:%l:%c:%m
elseif executable("ag")
    set grepprg=ag\ --nogroup\ --nocolor\ --ignore-case\ --vimgrep
    set grepformat^=%f:%l:%c:%m
endif

let g:tex_flavor = "latex"
" }}}
"

cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

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

" Folding {{{
set foldenable
set foldopen=all
set foldclose=all
set foldlevel=5
set foldnestmax=5
set foldmethod=indent
" }}}

" {{{ Tags
set tags=./tags,tags,~/.vim/tags/*;
nnoremap <C-]> g<C-]>
" }}}

" Completion {{{
set completeopt=longest,menuone
set complete=.,w,b,u
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>C-n>" : ""<CR>'
inoremap <expr> <C-p> pumvisible() ? '<C-p>' :
  \ '<C-p><C-r>=pumvisible() ? "\<lt>C-p>" : ""<CR>'
" }}}

" Undo {{{
set undofile
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000
" }}}

" Misc {{{
command! -nargs=* -bar Make w<bar>silent!<Space>make<Space><args><bar>silent<Space>redraw!
nnoremap <Space>m :Make<CR>
nnoremap <Space>M :Make
let g:tsuquyomi_disable_default_mappings = 1
let g:tsuquyomi_disable_quickfix = 1

filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set showmatch
set showcmd
set textwidth=80
set columns=80
set ttimeout
set ttimeoutlen=20
set tabstop=8
set shiftwidth=4
set softtabstop=4
set smarttab
set expandtab
set history=1000
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

" make list-like commands more intuitive
function! CCR()
    let cmdline = getcmdline()
    command! -bar Z silent set more|delcommand Z
    if cmdline =~ '\v\C^(ls|files|buffers)'
        " like :ls but prompts for a buffer command
        return "\<CR>:b"
    elseif cmdline =~ '\v\C/(#|nu|num|numb|numbe|number)$'
        " like :g//# but prompts for a command
        return "\<CR>:"
    elseif cmdline =~ '\v\C^(dli|il)'
        " like :dlist or :ilist but prompts for a count for :djump or :ijump
        return "\<CR>:" . cmdline[0] . "j  " . split(cmdline, " ")[1] . "\<S-Left>\<Left>"
    elseif cmdline =~ '\v\C^(cli|lli)'
        " like :clist or :llist but prompts for an error/location number
        return "\<CR>:sil " . repeat(cmdline[0], 2) . "\<Space>"
    elseif cmdline =~ '\C^old'
        " like :oldfiles but prompts for an old file to edit
        set nomore
        return "\<CR>:Z|e #<"
    elseif cmdline =~ '\C^changes'
        " like :changes but prompts for a change to jump to
        set nomore
        return "\<CR>:Z|norm! g;\<S-Left>"
    elseif cmdline =~ '\C^ju'
        " like :jumps but prompts for a position to jump to
        set nomore
        return "\<CR>:Z|norm! \<C-o>\<S-Left>"
    elseif cmdline =~ '\C^marks'
        " like :marks but prompts for a mark to jump to
        return "\<CR>:norm! `"
    elseif cmdline =~ '\C^undol'
        " like :undolist but prompts for a change to undo
        return "\<CR>:u "
    else
        return "\<CR>"
    endif
endfunction
cnoremap <expr> <CR> CCR()

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

function! Substitute(type, ...)
    let cur = getpos("''")
    call cursor(cur[1], cur[2])
    let cword = expand('<cword>')
    execute "'[,']s/" . cword . "/" . input(cword . '/')
    call cursor(cur[1], cur[2])
endfunction

"Custom commands {{{

"Lightweight git blame
command! -range GB echo join(systemlist("git -C " . shellescape(expand('%:p:h')) . " blame -L <line1>,<line2> " . expand('%:t')), "\n")
"Rename current file
command! -nargs=1 -bar -complete=file Rename file<space><args><bar>call<space>delete(expand('#'))<bar>w

"}}}

"Vimtex {{{
let g:vimtex_view_method = 'zathura'
"}}}

"Vim-slime {{{
let g:slime_target = 'x11'
"}}}

"vim-ghcid-quickfix {{{
let g:ghcid_quickfix = #{ showing: 'quickfix_on_error' }
"}}}
"}}}
