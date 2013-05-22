"Set up Vundle
set nocompatible        " Use Vim defaults instead of 100% vi compatibility
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

"Bundles!
Bundle 'gmarik/vundle'
Bundle 'tpope/vim-sensible'
Bundle 'scrooloose/nerdtree'
Bundle 'kien/ctrlp.vim'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'scrooloose/syntastic'
" ^^^ This requires syntax checkers to be installed (php, jshint)
Bundle 'majutsushi/tagbar'
" Bundle 'kchmck/vim-coffee-script'
Bundle 'CITguy/vim-coffee-script'
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-powerline'
" ^^^ This requires patched fonts
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/JSON.vim'
Bundle 'tpope/vim-eunuch'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-vividchalk'
Bundle 'sjl/gundo.vim'
Bundle 'vim-scripts/paredit.vim'
Bundle 'groenewege/vim-less'
Bundle 'wavded/vim-stylus'
Bundle 'uarun/vim-protobuf'
Bundle 'vim-scripts/SmartCase'
Bundle 'derekwyatt/vim-scala'
Bundle 'mileszs/ack.vim'
Bundle 'vim-scripts/trailing-whitespace'
Bundle 'vim-scripts/Align'
Bundle 'Valloric/YouCompleteMe'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-classpath'
Bundle 'guns/vim-clojure-static'
Bundle 'kien/rainbow_parentheses.vim'
Bundle 'airblade/vim-gitgutter'
Bundle 'airblade/vim-rooter'

"make thee comma the leader, and swap the it with ctrl-e
nnoremap <C-e> ,
let mapleader = ","

"make it easier to go in command mode
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

"search for the current word
noremap <F4> <ESC>:AckFromSearch!<CR>

" Search for selected text, forwards or backwards.
vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gV:call setreg('"', old_reg, old_regtype)<CR>

"build ctags
command! -nargs=0 Ctags :
	\ exe "!ctags -R
            \ --exclude=.git
            \ --exclude=\\*.js
            \ --tag-relative=yes
            \ --PHP-kinds=+cf-v
            \ --regex-PHP='/abstract\s+class\s+([^ ]+)/\1/c/'
            \ --regex-PHP='/interface\s+([^ ]+)/\1/c/'
            \ --regex-PHP='/(public\s+|static\s+|abstract\s+|protected\s+|private\s+)function\s+\&?\s*([^ (]+)/\2/f/'"

"make the vim search regex more like perl
nnoremap / /\v
vnoremap / /\v

"jj breaks out of insert mode
imap jj <Esc>l

"shift insert pastes the x11 clipboard
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>

""abbrevation: %% -> current dir of file
cabbr <expr> %% expand('%:p:h')

"json.vim syntax
au! BufRead,BufNewFile *.json set filetype=json
augroup json_autocmd
	autocmd FileType json set foldmethod=syntax
augroup END

" have Vim jump to the last position when
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

filetype plugin indent on

if has("gui_running")
	colorscheme vividchalk
	highlight StatusLine guibg=darkred
	highlight StatusLineNC guifg=purple
	highlight Comment guifg=salmon
	set guifont=Inconsolata-dz\ for\ Powerline\ 9
	" Remove toolbar
	set guioptions-=T
	"remove menu
	set guioptions-=m
	" remove scrollbar
	set guioptions-=R
	set guioptions-=r
	set guioptions-=L
	set guioptions-=l
endif

set background=dark
set ignorecase		" Do case insensitive matching
set hidden             " Hide buffers when they are abandoned
set number		"line numbers
set hlsearch		"Highlight search
set fileencodings=utf-8
set encoding=utf-8
set wildmode=list:longest
:set cpoptions+=$	"show a dollar sign when doing a change command
set nowrap		"dont word wrap
set encoding=utf-8
set smartindent
set cursorline		"show where the cursor is
set ttyfast
set gdefault		"all s// commands are global
set expandtab
set tabstop=2
set shiftwidth=2
set completeopt =menu,menuone,longest "get rid of scratch pad
set wildignore+=*.git,*.jpg,tags,*.pyc,*/node_modules/*
set magic	"make regexs more perl like
set nomodeline
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc,.pyc " Suffixes that get lower priority when doing tab completion for filenames.
set nowb
set colorcolumn=81

"ctrlp
let g:ctrlp_working_path_mode = 0
let g:ctrlp_use_caching = 0
let g:ctrlp_max_height = 30
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard']
let g:ctrlp_dont_split = ''
let g:ctrlp_arg_map = 0
let g:ctrlp_open_multi = '2'
let g:ctrlp_extensions = ['tag']

"taqbar
let g:tagbar_left = 1
let g:tagbar_type_php  = {
	\ 'ctagstype' : 'php',
	\ 'kinds'     : [
		\ 'i:interfaces',
		\ 'c:classes',
		\ 'd:constant definitions',
		\ 'f:functions',
	\ ]
\ }

let g:tagbar_type_coffee = {
      \ 'ctagstype' : 'coffee',
      \ 'kinds' : [
      \   'c:classes',
      \   'f:functions',
      \   'v:variables'
      \ ],
\ }

"show the tag list
nnoremap <silent> <F8> :TagbarOpenAutoClose<CR>

"syntastic options
"This requires jsonlint and jshint
let s:php_executable = "/usr/bin/php"
let g:syntastic_phpcs_disable = 1
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1
let g:syntastic_check_on_open=1

"minibuf options
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplCheckDupeBufs = 0
let g:miniBufExplVSplit = 40   " column width in chars
let g:miniBufExplSplitBelow=1  " Put new window on right
hi MBENormal guifg=gray ctermfg=gray

"powerline
let g:Powerline_symbols='fancy'

"nerdtree
noremap <silent> <F2> :NERDTreeToggle<CR>
au VimEnter * highlight clear SignColumn
