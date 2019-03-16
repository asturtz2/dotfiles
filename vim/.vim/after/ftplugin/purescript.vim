let purescript_indent_let = 2
let purescript_indent_do = 2
let purescript_indent_where = 2
let purescript_indent_case = 2
let purescript_indent_in = 0

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
    let query = s:stub . substitute(a:args, '\\\?\s\+', '%20', 'g') . "'"
    echo expand(query)
    return query
endfunction

" Build the command
command! -nargs=* SearchPursuit call system(<SID>Pursuit(<q-args>))

setlocal keywordprg=:SearchPursuit

