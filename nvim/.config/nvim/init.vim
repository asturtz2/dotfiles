" Startup {{{

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
nnoremap <Leader>b :ls<CR>:b<Space>
nnoremap <Leader>a <C-^>
nnoremap <Leader>d :ls<CR>:bd<Space>
nnoremap <Leader>vb :ls<CR>:vert<Space>:sb<Space>
nnoremap <Leader>hb :ls<CR>:sb<Space>
nnoremap <PageUp> :bnext<CR>
nnoremap <PageDown> :bprev<CR>
" }}}

" Search {{{
nnoremap <silent> <Leader>c :nohlsearch<CR>
nnoremap <Leader>e :edit **/*
nnoremap <Leader>f :find *
nnoremap <Leader>F :find <C-R>=expand('%:h').'/*'<CR>
nnoremap <Leader>vf :vert :sfind *
nnoremap <Leader>vF :vert :sfind <C-R>=expand('%:h').'/*'<CR>
nnoremap <Leader>hf :sfind *
nnoremap <Leader>hF :sfind <C-R>=expand('%:h').'/*'<CR>
nnoremap <Leader>tf :tabfind *
nnoremap <Leader>tF :tabfind <C-R>=expand('%:h').'/*'<CR>
" }}}

" Marks {{{
nnoremap <Leader>m :marks<CR>:normal!<Space>'
" }}}

" Vim plug {{{
map <Leader>pi :PlugInstall<CR>
map <Leader>pu :PlugUpdate<CR>
map <Leader>pc :PlugClean<CR>
"}}}
" Misc {{{
map <Leader>rv :so ~/.config/nvim/init.vim<CR>
"}}}

"}}}

"{{{ Core Options

" Search {{{
set incsearch "Search as characters are entered
set hlsearch "Highlight search matches
set path=.,**/
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.aux,*.log,*.fdb*,*.fls
set wildignorecase
set wildmode=list:full
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

" Misc {{{
filetype plugin indent on
set modelines=1
set hidden "Enable hidden buffers
set completeopt-="preview"
set showmatch
set textwidth=80
let loaded_netrwPlugin=1 "Do not load netrw
" }}}

"}}}

" Core UI {{{
set relativenumber "Enable line numbers
set number "Show current line number
set showmatch "Show matching characters (parentheses, brackets, etc.)
" }}}

" Plugins {{{

" Installation{{{
call plug#begin()

" Core {{{
Plug 'ctrlpvim/ctrlp.vim', { 'on' : ['CtrlP', 'CtrlPBuffer', 'CtrlPMRU'] }
Plug 'tpope/vim-fugitive'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-sleuth'
Plug 'francoiscabrol/ranger.vim' , {'on' : 'Ranger'}
Plug 'sjl/gundo.vim', { 'on' : 'GundoToggle' }
Plug 'dylanaraps/wal'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'ervandew/supertab'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'airblade/vim-gitgutter'
" }}}

" Haskell {{{
Plug 'itchyny/vim-haskell-indent'
" }}}

" C# {{{
Plug 'OmniSharp/omnisharp-vim', { 'for' : 'cs' }
" }}}

" Latex {{{
Plug 'lervag/vimtex', { 'for' : ['tex', 'plaintex', 'latex'] }
" }}}

call plug#end()
" }}}

" Plugin options {{{

" Ranger {{{
let g:ranger_map_keys = 0
map <Leader>t :Ranger<CR>
" }}}

" CtrlP {{{
" map <Leader>fo :CtrlP<CR>
" map <Leader>fl :CtrlPBuffer<CR>
" map <Leader>fu :CtrlPMRU<CR>
let g:ctrlp_show_hidden = 1 "Show hidden files in control p
let g:ctrlp_user_command = 'ag %s -l -g "" -f --nocolor --hidden'
let g:ctrlp_use_caching = 0
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

" Supertab {{{
let g:SuperTabDefaultCompletionType = "context"
" }}}

" Fugitive {{{
map <Leader>gs :Gstatus<CR>
map <Leader>gc :Gcommit -m
map <Leader>gm :Gmerge
map <Leader>gpl :Gpull
map <Leader>gps :Gpush
map <Leader>gf :Gfetch
map <Leader>gg :Ggrep
map <Leader>gl :Glog
map <Leader>gd :Gdiff
map <Leader>gb :Gblame
" }}}

" Gundo {{{
map <Leader>u :GundoToggle<CR>
" }}}

" Omnisharp {{{

" Options{{{
let g:OmniSharp_selector_ui='ctrlp'
let g:OmniSharp_want_snippet=1
" }}}

" }}}

"Vimtex {{{
let g:vimtex_complete_close_braces = 1
"}}}

" Airline {{{
let g:airline_theme='term'
let g:airline_powerline_fonts=1
" let g:airline#extensions#syntastic#enabled = 1
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#buffer_idx_mode = 1
" let g:airline#extensions#tabline#tab_nr_type = 1 " tab number
" let g:airline#extensions#tabline#show_tab_nr = 1
" let g:airline#extensions#tabline#fnamemod = ':t:.'
" let g:airline#extensions#tabline#formatter = 'unique_tail_improved'
" let g:airline#extensions#whitespace#symbol = '|'
" let g:airline#extensions#whitespace#checks = []
let g:airline_skip_empty_sections = 1
let g:airline#parts#ffenc#skip_expected_string='utf-8[unix]'
" }}}

" Bufferline {{{
let g:bufferline_echo = 0
" }}}

" Wal {{{
colorscheme wal
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
"}}}

" vim:foldmethod=marker:foldlevel=0
