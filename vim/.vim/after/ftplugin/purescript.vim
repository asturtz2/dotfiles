" if exists("b:did_ftplugin")
"     finish
" endif
" let b:did_ftplugin = 1

augroup Build
    autocmd!
    autocmd BufWritePost <buffer> make | silent redraw!
augroup end

let purescript_indent_let = 2
let purescript_indent_do = 2
let purescript_indent_where = 2
let purescript_indent_case = 2
let purescript_indent_in = 0

compiler pulp

let &path='src/**'
let &define='^\s*\(data\|type\|newtype\|\(foreign import \)\?\ze\i\+\s*::\)'

setlocal shiftwidth=2
setlocal softtabstop=4
setlocal expandtab

" What command to use
function! s:Cmd() abort
    " Linux/BSD
    if executable("xdg-open")
        return "xdg-open"
    endif
    " MacOS
    if executable("open")
        return "open"
    endif
    " Windows
    return "explorer"
endfunction

let s:stub = "$BROWSER 'https://pursuit.purescript.org/search?q="


" Build the full URL
function! s:Pursuit(args, ...) abort
    let query = s:stub . substitute(a:args, '\\\?\s\+', '%20', 'g') . "'" . " &"
    return query
endfunction

" Build the command
command! -nargs=* SearchPursuit call system(<SID>Pursuit(<q-args>))

setlocal keywordprg=:SearchPursuit

