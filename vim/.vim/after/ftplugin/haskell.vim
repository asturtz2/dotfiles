set softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

setlocal include=import
setlocal includeexpr=substitute(v:fname,'\\.','/','g')
setlocal suffixesadd=.hs
let &l:define='\(^data\s\|^class\s\|^\ze\i\+\s*::\)'
setlocal path=src/**

setlocal wildignore+=*/build/**/*
let b:replcmd = filereadable("package.yaml") ? 'stack ghci' : 'ghci'
nnoremap <buffer> \t :ReplSend :t <C-R><C-W><bar>ReplOpen %<CR>
nnoremap <buffer> \i :ReplSend :i <C-R><C-W><bar>ReplOpen %<CR>
nnoremap <buffer> \c :call term_sendkeys(b:replcmd, '<C-V><C-L>')<bar>ReplOpen %<CR>
nnoremap <buffer> \l :ReplSend <C-R>=':l '.expand('%')<CR><CR>
command! -nargs=* Repl if bufname(b:replcmd) == "" | execute 'vert' 'term' b:replcmd <q-args> | wincmd w | elseif bufwinid(b:replcmd) == -1 | execute 'vert' 'sbuffer' b:replcmd | wincmd w | else | execute bufwinnr(b:replcmd).'wincmd c' | endif
command! -nargs=* ReplOpen if bufname(b:replcmd) == "" || bufwinid(b:replcmd) == -1 | execute 'Repl' <q-args> | endif
command! -nargs=? -range -complete=tag -bar ReplSend call haskell#ReplSend(expand('<args>'), <range>)
nnoremap <buffer> \r :Repl %<CR>

function! haskell#ReplSend(content, range) range
    if a:content != "" && a:range == 0
        call term_sendkeys(b:replcmd, a:content.'')
    else
        normal! gv"vy
        call term_sendkeys(b:replcmd, ':{'.getreg("v").':}')
    endif
endfunction

augroup ReloadGhci
    autocmd!
    autocmd BufWritePost *.hs call term_sendkeys('ghci', ':r')
augroup END


packadd vim-ghcid-quickfix
compiler stack
let g:qf_auto_open_quickfix = 0
GhcidQuickfixStart

" packadd vim-lsp

" if executable('haskell-language-server-wrapper')
"     au User lsp_setup call lsp#register_server({
"         \ 'name': 'haskell-language-server',
"         \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
"         \ 'allowlist': ['haskell'],
"         \ })
" endif

" function! s:on_lsp_buffer_enabled() abort
"     setlocal omnifunc=lsp#complete
"     setlocal signcolumn=yes
"     " if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
"     nmap <buffer> \d <plug>(lsp-definition)
"     nmap <buffer> \i <plug>(lsp-implementation)
"     nmap <buffer> \t <plug>(lsp-type-definition)
"     nmap <buffer> [g <plug>(lsp-previous-diagnostic)
"     nmap <buffer> ]g <plug>(lsp-next-diagnostic)
"     nmap <buffer> K <plug>(lsp-hover)
"     inoremap <buffer> <expr><c-f> lsp#scroll(+4)
"     inoremap <buffer> <expr><c-d> lsp#scroll(-4)

"     " refer to doc to add more commands
" endfunction

" augroup lsp_install
"     au!
"     " call s:on_lsp_buffer_enabled only for languages that has the server registered.
"     autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
" augroup END
