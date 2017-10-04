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

"Create file marks for the last file viewed of a given type
augroup LeaveBuffer
    autocmd!
    autocmd BufLeave *.css normal! mC
    autocmd BufLeave *.tex normal! mT
    autocmd BufLeave *.js normal! mJ
    autocmd BufLeave *.rb normal! mR
    autocmd BufLeave *.md normal! mM
    autocmd BufLeave *.hs normal! mH
augroup end

augroup NumberToggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
    autocmd BufLeave,FocusLost,InsertEnter * set norelativenumber
augroup end
" }}}

" Core Mappings {{{

" Basic mappings {{{
let mapleader = "\<Space>"
let maplocalleader = "\\"
"}}}

" Movement {{{
nnoremap Y y$

" Go to beginning of last visual selection
nnoremap gV `[v`
nnoremap <Home> ^
nnoremap <End> $
nnoremap [[ :cprevious<CR>
nnoremap ]] :cnext<CR>
" }}}

" Buffers {{{
nnoremap <Leader>s <C-^>
nnoremap <Leader>d :ls<CR>:bd<Space>
nnoremap <PageUp> :bnext<CR>
nnoremap <PageDown> :bprev<CR>
" }}}

" Marks {{{
nnoremap <Leader>m :marks<CR>:normal!<Space>'
" }}}

" Vim plug {{{
map <Leader>pi :PlugInstall<CR>
map <Leader>pu :PlugUpdate<CR>
map <Leader>ps :PlugStatus<CR>
map <Leader>pc :PlugClean<CR>
"}}}

" Terminals {{{
if has('nvim')
    tnoremap <Esc> <C-\><C-N>
endif
" }}}

" Misc {{{
map <Leader>v :so ~/.config/nvim/init.vim<CR>
nnoremap <Leader>z :make<CR>
"}}}

"}}}

"{{{ Core Options

" Files {{{
set path=.,**
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.aux,*.log,*.fdb*,*.fls
set wildignorecase
set wildmode=longest:full,list,full
set wildmenu

let g:tex_flavor = "latex"
" }}}

" Search {{{
if has('nvim')
    set inccommand=nosplit "Show substitution matches/replacements dynamically
endif
set incsearch "Search as characters are entered
set hlsearch "Highlight search matches
set ignorecase
set smartcase
" }}}

" Folding {{{
set foldenable "Enable folding
set foldlevelstart=10 "Start new buffers with folds of level 10 or greater as folded
set foldnestmax=10 "Don't allow folding greater than level 10
set foldmethod=indent "Fold based on an indent level
" }}}

" {{{ Tags
let g:easytags_file = '~/.vim/global-tags'
set tags=./tags,tags,~/.vim/tags/*;
let g:easytags_by_filetype = '~/.vim/tagfiles'
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

" Misc {{{
filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set completeopt-="preview"
set showmatch
set textwidth=76
set columns=76
set ttimeout
set ttimeoutlen=20
let loaded_netrwPlugin=1 "Do not load netrw
" }}}

"}}}

" Core UI {{{
set number
set relativenumber "Enable line numbers
set showmatch "Show matching characters (parentheses, brackets, etc.)
" }}}

" Plugins {{{

" Installation{{{
call plug#begin()

" Core {{{
Plug 'ctrlpvim/ctrlp.vim', { 'on' : ['CtrlP', 'CtrlPBuffer', 'CtrlPCurFile', 'CtrlPMRU', 'CtrlPBookmarkDir'] }
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'xolox/vim-easytags'
Plug 'tpope/vim-endwise'
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
Plug 'Houl/repmo-vim'
Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ervandew/supertab'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'junegunn/goyo.vim', { 'on' : 'Goyo' }
" }}}

" Haskell {{{
Plug 'itchyny/vim-haskell-indent', { 'for' : 'haskell' }
Plug 'neovimhaskell/haskell-vim', { 'for' : 'haskell' }
" Plug 'eagletmt/ghcmod-vim', { 'for' : 'haskell' }
" }}}

" Latex {{{
Plug 'lervag/vimtex', { 'for' : ['tex', 'plaintex', 'latex'] }
" }}}

" Wal {{{
if executable('wal')
    Plug 'dylanaraps/wal'
endif
" }}}

call plug#end()
" }}}

" Plugin options {{{

" Lion {{{
" let g:lion_squeeze_spaces=1
" }}}
"
" Repmo {{{

" Testing plugin mappings out
map <expr> ; repmo#LastKey(';') | sunmap ;
map <expr> , repmo#LastRevKey(',') | sunmap ,

noremap <expr> j repmo#SelfKey('j', 'k') | sunmap j
noremap <expr> k repmo#SelfKey('k', 'j') | sunmap k
noremap <expr> h repmo#SelfKey('h', 'l') | sunmap h
noremap <expr> l repmo#SelfKey('l', 'h') | sunmap l

noremap <expr> w repmo#SelfKey('w', 'b') | sunmap w
noremap <expr> b repmo#SelfKey('b', 'w') | sunmap b
noremap <expr> W repmo#SelfKey('W', 'B') | sunmap W
noremap <expr> B repmo#SelfKey('B', 'W') | sunmap B
noremap <expr> e repmo#SelfKey('e', 'ge') | sunmap e
noremap <expr> ge repmo#SelfKey('ge', 'e') | sunmap ge
noremap <expr> E repmo#SelfKey('E', 'gE') | sunmap E
noremap <expr> gE repmo#SelfKey('gE', 'E') | sunmap gE

" These mappings apparently don't work, need to fix
" noremap <expr> <C-D> repmo#SelfKey('<C-D>', '<C-U>') | sunmap <C-D>
" noremap <expr> <C-U> repmo#SelfKey('<C-U>', '<C-D>') | sunmap <C-U>
" noremap <expr> <C-F> repmo#SelfKey('<C-F>', '<C-B>') | sunmap <C-F>
" noremap <expr> <C-B> repmo#SelfKey('<C-B>', '<C-F>') | sunmap <C-B>
" noremap <expr> <C-I> repmo#SelfKey('<C-I>', '<C-O>') | sunmap <C-I>
" noremap <expr> <C-O> repmo#SelfKey('<C-O>', '<C-I>') | sunmap <C-O>
" noremap <expr> <C-[> repmo#SelfKey('<C-[>', '<C-]>') | sunmap <C-[>
" noremap <expr> <C-]> repmo#SelfKey('<C-]>', '<C-[>') | sunmap <C-]>

" noremap <expr> <C-E> repmo#SelfKey('<C-E>', '<C-Y>') | sunmap <C-E>
" noremap <expr> <C-Y> repmo#SelfKey('<C-Y>', '<C-E>') | sunmap <C-Y>


noremap <expr> f repmo#ZapKey('f')|sunmap f
noremap <expr> F repmo#ZapKey('F')|sunmap F
noremap <expr> t repmo#ZapKey('t')|sunmap t
noremap <expr> T repmo#ZapKey('T')|sunmap T

" }}}


" Incsearch.vim {{{
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
" }}}

" CtrlP {{{
nnoremap <Leader>f :CtrlP<CR>
nnoremap <Leader>F :CtrlPCurFile<CR>
nnoremap <Leader>l :CtrlPBuffer<CR>
nnoremap <Leader>r :CtrlPMRU<CR>

nnoremap <Leader>c :CtrlPDir<CR>
nnoremap <Leader>b :CtrlPBookmarkDir<CR>
nnoremap <Leader>ab :CtrlPBookmarkDirAdd<CR>
let g:ctrlp_show_hidden = 1 "Show hidden files in control p
let g:ctrlp_user_command = 'ag %s -l -g "" -f --nocolor --hidden --path-to-ignore ~/.ignore'
let g:ctrlp_working_path_mode = 0
" }}}

" Syntastic {{{
let g:syntastic_aggregate_errors = 1
" set statusline+=%#warningmsg#
" set statusline+=%{SyntasticStatuslineFlag()}
" set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_wq = 1
let g:syntastic_cs_checkers=['syntax', 'semantic', 'issues']
" }}}

" Airline {{{
let g:airline_theme='term'
let g:airline_powerline_fonts=1
let g:airline_skip_empty_sections = 1
" }}}

" Supertab {{{
let g:SuperTabDefaultCompletionType = "context"
" }}}

" Gundo {{{
map <Leader>u :GundoToggle<CR>
" }}}

"Vimtex {{{
let g:vimtex_complete_close_braces = 1
let g:vimtex_view_method = 'zathura'
let g:vimtex_indent_enabled = 0
"}}}

"{{{ Haskell-vim
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
let g:haskell_indent_disable = 0
"}}}

" Wal {{{
if executable('wal')
    colorscheme wal
endif
" }}}

" }}}

" }}}

" Custom Functions {{{

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

"FIXME: Quick and dirty latex mapping
inoremap <F1> $\mathbf{}$<ESC>F}i

"}}}

" vim:foldmethod=marker:foldlevel=0
