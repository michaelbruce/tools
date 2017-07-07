" VI configuration
" Author: Michael Bruce <mike@bearmetalcoding.com>
" vim:set ts=2 sts=2 sw=2 expandtab:

autocmd!
silent! call plug#begin('~/.config/nvim/plugged')

Plug 'michaelbruce/vim-sane'
Plug 'michaelbruce/vim-alternator', { 'on' : ['Alternate', 'Test'] }
Plug 'michaelbruce/vim-notes',   { 'for' : 'notes' }
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'michaelbruce/vim-chruby', { 'for' : ['ruby', 'clojure'] }
Plug 'michaelbruce/ice.nvim', { 'for' : ['clojure'] }
Plug 'michaelbruce/vim-parengage', { 'for' : ['clojure', 'lisp'] }

call plug#end()

syntax on
set background=dark
color night

augroup filetypeCustomisations
  autocmd!
  au FileType python setl sw=2 sts=2 et
  au FileType vim setl sw=2 sts=2 et
  au FileType r setl sw=2 sts=2 et
  au FileType gitcommit set textwidth=72
  au FileType markdown set textwidth=79
  autocmd FileType c set complete-=i
  autocmd BufNewFile,BufReadPost *.notes set filetype=notes
  autocmd BufNewFile,BufReadPost *.html set filetype=htmlm4
  autocmd BufNewFile,BufReadPost *.md set filetype=markdown
  autocmd BufNewFile,BufReadPost *.boot set filetype=clojure
  autocmd BufNewFile,BufReadPost .Rprofile set filetype=r
augroup END

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

function! Tags()
    exec "!ctags -R $(git rev-parse --show-toplevel || echo \".\")"
endfunction

command! Tags :call Tags()

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

function! SyntaxStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

command! SyntaxStack :call SyntaxStack()

function! Selecta() abort
  let g:selecta_original_window = win_getid()
  belowright split
  enew
  let options = { 'buf': bufnr('') }

  function! options.on_stdout(job_id, data, event)
    let g:selecta_output = substitute(join(a:data), "", "", "g")
  endfunction

  function! options.on_exit(id, code, _event)
    execute 'bd!' self.buf
    if g:selecta_output !~ "Interrupt" && g:selecta_output != " "
      call win_gotoid(g:selecta_original_window)
      execute 'e ' . g:selecta_output
    endif
  endfunction

  call termopen("echo -ne $(find ./* -path ./target -prune -o -type f -print | selecta)", options)
  startinsert
endfunction

command! Selecta call Selecta()

nnoremap <leader>f :Selecta<CR>

" set vim-r-plugin to
let r_indent_align_args = 0

" Set vim-r-plugin to mimics ess :
let r_indent_ess_comments = 0
let r_indent_ess_compatible = 0

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

function! AgSearch() abort
  let s:word_under_cursor = expand("<cword>")
  belowright vsplit
  execute 'term ag "' . s:word_under_cursor . '"'
endfunction

nnoremap K :call AgSearch()<CR>
