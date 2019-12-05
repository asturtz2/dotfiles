setlocal shiftwidth=2
setlocal softtabstop=2
setlocal tabstop=2

nnoremap <buffer> <Space>gu :Grep '<<C-R>=expand('%:t:r')<CR>'<CR>

setlocal isfname+=@-@
setlocal suffixesadd+=.vue,.js,.ts,.tsx
setlocal include=^\\s*[^\/]\\+\\(from\\\|require(\\)\\s*['\"\.]
setlocal includeexpr=RESOLVE(v:fname)

function! RESOLVE(module) abort
    let built_ins = [
    \ "assert",
    \ "async_hooks",
    \ "child_process",
    \ "cluster",
    \ "crypto",
    \ "dns",
    \ "domain",
    \ "events",
    \ "fs",
    \ "http",
    \ "http2",
    \ "https",
    \ "inspector",
    \ "net",
    \ "os",
    \ "path",
    \ "perf_hooks",
    \ "punycode",
    \ "querystring",
    \ "readline",
    \ "stream",
    \ "string_decoder",
    \ "tls",
    \ "tty",
    \ "dgram",
    \ "url",
    \ "util",
    \ "v8",
    \ "vm",
    \ "zlib" ]

    " There's no source file for built-in node modules.
    " Example: var http = require('http');
    if index(built_ins, a:module) != -1
        return 0
    endif

    " Handling absolute and relative paths.
    " Example: var foo = require('./foo.js');
    if a:module =~ '^\/' || a:module =~ '^\.\{1,2}\/' || a:module =~ '^@\/'
        let root = fnamemodify(substitute(finddir("node_modules", ".;"), '/node_modules', '', ''), ':p:h')

        if a:module =~ '^@\/'
            let module = substitute(a:module, '^@\/', '', '')
        endif

        if a:module =~ '^\.\{1,2}\/'
            let module = substitute(a:module, '^\.\{1,2}\/', '', '')
        endif

        let filename = 0
        try
            if isdirectory(module)
                let filename = findfile(module . "/index.js")
            else
                let filename = findfile(module)
            endif
        catch
            let filename = a:module
        endtry

        return filename
    endif

    " Handling vendored-in modules.
    " Example: var _ = require('lodash');
    for dir in NODE_MODULES_PATHS()
        " require('module') => node_modules/module/package.json#main => node_modules/module/**/...
        if filereadable(dir . "/" . a:module . "/package.json")
            let package = json_decode(join(readfile(dir . "/" . a:module . "/package.json")))
            return dir . "/" . a:module . "/" . substitute(get(package, "main", "index.js"), '^\.\{1,2}\/', '', '')
        endif

        " require('module') => node_modules/module/index.js
        if filereadable(dir . "/" . a:module . "/index.js")
            return dir . "/" . a:module . "/index.js"
        endif

        " require('module') => node_modules/module.js
        if filereadable(dir . "/" . a:module . ".js")
            return dir . "/" . a:module . ".js"
        endif
    endfor

    " well, well, well
    return 0
endfunction

function! NODE_MODULES_PATHS() abort
    " node_modules/
    let node_modules = finddir("node_modules", ".;")

    " node_modules/foo/node_modules/
    let basic_dirs = extend([node_modules], globpath(node_modules, "*/node_modules", 1, 1))

    " node_modules/@foo/bar/node_modules/
    let all_dirs = extend(basic_dirs, globpath(node_modules, "@*/*/node_modules", 1, 1))

    return all_dirs
endfunction

let &define = '^\%(' . join([
  \ '\s*\(var\|let\|const\)\s\+\ze\k\+\s*=\s*function\>',
  \ '\s*\(var\|let\|const\)\s\+\ze\k\+\s*=\s*(.*)\s+=>',
  \ '\s*\ze\k\+\s*:\s*function\>',
  \ '\s*\ze\k\+\s*:\s*(.*)\s*{',
  \ '\s*\ze\k\+\s*(.*)\s*{',
  \ '\s*function\s\+\ze\k\+'
  \ ], '\|') . '\)'
