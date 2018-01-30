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

augroup NumberToggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
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
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'xolox/vim-easytags'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-fugitive'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
" Plug 'sheerun/vim-polyglot'
Plug 'tommcdo/vim-lion'
Plug 'tommcdo/vim-exchange'
Plug 'rstacruz/vim-closer'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'wellle/targets.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'romainl/vim-tinyMRU'
Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' }
Plug 'ervandew/supertab'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'junegunn/goyo.vim', { 'on' : 'Goyo' }
Plug 'unblevable/quick-scope'
Plug 'w0rp/ale'
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
let g:incsearch#auto_nohlsearch = 1
map / <Plug>(incsearch-forward)
map ? <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)
map n  <Plug>(incsearch-nohl-n)
map N  <Plug>(incsearch-nohl-N)
map *  <Plug>(incsearch-nohl-*)
map #  <Plug>(incsearch-nohl-#)
map g* <Plug>(incsearch-nohl-g*)
map g# <Plug>(incsearch-nohl-g#)


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

" Wal
if executable('wal')
    colorscheme wal
endif
" }}}
"}}}

" Changes {{{
nnoremap Y y$

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
" }}}
"
" Search/Replace {{{

"Displays all lines containing the word in the file. Mnemonic: Global
"Occurrences or Go to Occurrences. The word will be remembered as the last
"search pattern used, so n/N will work, for example.
nnoremap go "gyiw:g/<C-R>g/z#.2<CR>

"Replace every occurrence of the word under the cursor in the paragraph.
nnoremap <Space>sp :'{,'}s/\<<C-r>=expand('<cword>')<CR>\>/
nnoremap <Space>sf :%s/\<<C-r>=expand('<cword>')<CR>\>/


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

" Files {{{
nnoremap <Space>f :find <C-R>=expand(getcwd()).'/**/*'<CR>
nnoremap <Space>F :find *
nnoremap <Space>e :edit <C-R>=fnameescape(expand('%:p:h')).'/'<CR>
nnoremap <Space>cf :cd **/*
nnoremap <Space>ch :cd<CR>:pwd<CR>
nnoremap <Space>cc :cd %:p:h<CR>:pwd<CR>
command! -nargs=+ -complete=file_in_path -bar Grep  silent! grep! <args> | redraw! | cwindow
nnoremap <silent> <Space>gw :Grep <C-r><C-w><CR>
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
let g:easytags_file = '~/.vim/global-tags'
set tags=./tags,tags,~/.vim/tags/*;
let g:easytags_by_filetype = '~/.vim/tagfiles'
" let g:easytags_languages = {
"     'haskell': {
"         'cmd': 'haskdogs',
" }}}

" Completion {{{
set omnifunc=syntaxcomplete#Complete
" }}}

" Undo {{{
set undofile
set undodir=$HOME/.vim/undo
set undolevels=1000
set undoreload=10000
" }}}

" {{{ UI
set number
set relativenumber "Enable line numbers
set showmatch "Show matching characters (parentheses, brackets, etc.)
" }}}

" Misc {{{
map <Space>v :so ~/.vimrc<CR>
map <Space>z :!zathura * &<Left><Left>
nnoremap <Space>m :make<CR>

filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set completeopt-="preview"
set showmatch
set textwidth=80
set columns=80
set ttimeout
set ttimeoutlen=20
let loaded_netrwPlugin=1 "Do not load netrw
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

"Custom commands {{{
"A better oldfiles
" command! Bro :enew | setl buftype=nofile |  setl nobuflisted | 0put =v:oldfiles
"   \| nnoremap <buffer> <CR> gf | 1
"}}}

"}}}

" vim:foldmethod=marker:foldlevel=0
