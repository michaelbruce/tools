" vimrc
" Author:       Michael Bruce <http://michaelbruce.online/>
"
" This is your survival vim setup
" vim:set ts=2 sts=2 sw=2 expandtab:

autocmd!

silent! call plug#begin('~/.config/nvim/plugged')

Plug 'michaelbruce/vim-sane'
Plug 'michaelbruce/vim-alternator', { 'on' : ['Alternate', 'Test'] }
Plug 'michaelbruce/vim-notes',   { 'for' : 'notes' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'junegunn/rainbow_parentheses.vim', { 'on' : 'RainbowParentheses' }
Plug 'guns/xterm-color-table.vim', { 'on' : 'XtermColorTable' }
Plug 'junegunn/fzf', { 'dir': '~/.config/fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Languages
Plug 'sentient-lang/vim-sentient', { 'for' : 'sentient' }
Plug 'michaelbruce/vim-chruby', { 'for' : 'ruby' }
Plug 'neovim/node-host'
Plug 'michaelbruce/replay.nvim', { 'for' : 'clojure' }
" Plug 'clojure-vim/clj-refactor.nvim'
Plug 'michaelbruce/vim-parengage', { 'for' : ['clojure', 'lisp'] }
Plug 'guns/vim-clojure-static', { 'for' : 'clojure' }
Plug 'guns/vim-clojure-highlight', { 'for' : 'clojure' }
Plug 'rust-lang/rust.vim',  { 'for': 'rust' }
Plug 'cespare/vim-toml',  { 'for': 'toml' }
Plug 'junegunn/vader.vim',  { 'on': 'Vader', 'for': 'vader' }

call plug#end()

syn on

augroup filetypeCustomisations
  autocmd!
  au FileType python setl sw=2 sts=2 et
  au FileType vim setl sw=2 sts=2 et
  au FileType r setl sw=2 sts=2 et
  au FileType gitcommit set textwidth=72
  autocmd FileType c set complete-=i
  autocmd BufNewFile,BufReadPost *.notes set filetype=notes
  autocmd BufNewFile,BufReadPost *.cls set filetype=java
  autocmd BufNewFile,BufReadPost *.trigger set filetype=java
  autocmd BufNewFile,BufReadPost *.component set filetype=htmlm4
  autocmd BufNewFile,BufReadPost *.page set filetype=htmlm4
  autocmd BufNewFile,BufReadPost *.html set filetype=htmlm4
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  autocmd BufNewFile,BufReadPost *.boot set filetype=clojure
  autocmd BufNewFile,BufReadPost *.pxi set filetype=clojure
  autocmd BufNewFile,BufReadPost *.{clj,cljs} silent! JackIn
augroup END

set background=light
color white

if has("gui_running")
    set guifont=Inconsolata:h16
    set guioptions-=r
    set lines=50 columns=120
endif

" Disables BCE, Background color erase - allows non-text background to be
" filled under tmux
set t_ut=

set cursorline
set iskeyword+=- " Autocomplete words that include hyphen
set re=1 " vim-ruby still runs slowly on the newer regex engine...
set ttyfast

let mapleader= ' '
map gy :Alternate<CR>
nnoremap <CR> :Test<CR>
map <Leader>e :e <C-R>=expand('%:p:h')<CR>/

" Treat - as a word seperator when using C-w e.g (defn stream-file)
set iskeyword-=-

" TODO duplicate from alternator - fireplace needs autocmd to allow cqq CR
" ------------------------------------------------------------------------
function! MapCR()
  nnoremap <cr> :Test<cr>
endfunction
call MapCR()

" Leave the return key alone when in command line windows, since it's used
" to run commands there.
autocmd! CmdwinEnter * silent! unmap <cr>
autocmd! CmdwinLeave * silent! call MapCR()
" ------------------------------------------------------------------------

inoremap <C-d>        <C-o>x
inoremap <C-f>        <C-o>l
inoremap <C-b>        <C-o>h

command! JackIn :Connect nrepl://127.0.0.1:9999 %:h

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Selecta Mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Swallow the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction

function! SelectaFile(path)
    call SelectaCommand(
                \"find " . a:path . "/* -path ./target -prune " .
                \"-o -type f ! -regex '.*\\(pyc\\|gz\\)' -print", "", ":e")
endfunction

nnoremap <leader>f :FZF<cr>

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=7
  highlight fzf2 ctermfg=23 ctermbg=7
  highlight fzf3 ctermfg=237 ctermbg=7
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fzf\ -
endfunction

autocmd! User FzfStatusLine call <SID>fzf_statusline()

" nnoremap <leader>f :call SelectaFile(".")<cr>
nnoremap <leader>w :call SelectaCommand(
            \"find " .
            \"$(find ~/code/* -maxdepth 0 -type d \| selecta)" .
            \"/* -type f ! -name '*.gz'", "", ":view")<cr>

function! DeploySalesforce()
    let l:filepath = 'src' . split(expand('%:p'), 'src')[-1]
    execute "!apex_deploy.sh " . l:filepath . "
        \ ~/dotfiles/tooling-force.com-0.3.4.0.jar
        \ ~/notes/SingletrackDev.properties
        \ ~/code/Singletrack-Core/SingletrackDev/"
endfunction

command! DeploySalesforce :call DeploySalesforce()

function! Tags()
    exec "!ctags -R $(git rev-parse --show-toplevel || echo \".\")"
endfunction

command! Tags :call Tags()

" does this even work?
if expand('%:t') == 'build.boot'
  let b:fireplace_ns = 'boot.user'
endif

let g:clojure_syntax_keywords = {
    \ 'clojureMacro': ["deftask", "defproject", "defcustom"],
    \ 'clojureFunc': ["string/join", "string/replace"]
    \ }

if has('nvim')
    map <M-o> <C-\><C-n><C-w><C-w>
    tmap <M-o> <C-\><C-n><C-w><C-w>
endif

function! Scratch()
    normal 
    enew
    set ft=clojure
endfunction

command! Scratch :call Scratch()

" use lisp ft for scheme shell scripts
if did_filetype()
    finish
endif
if getline(1) =~# '^#!.*/usr/bin/env\s\+scsh.*\>'
    setfiletype lisp
endif

nnoremap <leader>y :belowright split \| term tmux attach -t repl<CR>
