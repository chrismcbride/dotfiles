set runtimepath=~/.vim,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim72,/usr/share/vim

set nocompatible        " Use Vim defaults instead of 100% vi compatibility
syntax on

"Set up Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

"Bundles!
Bundle 'scrooloose/nerdtree'
Bundle 'kien/ctrlp.vim'
Bundle 'fholgado/minibufexpl.vim'
Bundle 'shawncplus/phpcomplete.vim'
Bundle 'StanAngeloff/php.vim'
Bundle 'scrooloose/syntastic'
Bundle 'majutsushi/tagbar'
Bundle 'vim-scripts/VimClojure'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tpope/vim-commentary'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'tpope/vim-fugitive'
Bundle 'tsaleh/vim-matchit'
Bundle 'Lokaltog/vim-powerline'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/darkspectrum'


"make thee comma the leader, and swap the it with ctrl-e
nnoremap <C-e> ,
let mapleader = ","

"make it easier to go in command mode
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

"search for the current word
noremap <F4> <ESC>:call VimGrepper(expand("<cword>"))<CR> 

"search for a word
command! -nargs=1 Vg call VimGrepper( '<args>' )

function! VimGrepper(arg)
	:execute "noautocmd vimgrep /" . a:arg . "/j **" 
	:execute "botright cwindow"
endfunction

"show the tag list
nnoremap <silent> <F8> :TagbarToggle<CR>

"show the nerdtree
noremap <silent> <F2> :NERDTreeToggle<CR>

"make the vim search regex more like perl
nnoremap / /\v
vnoremap / /\v

"jj breaks out of insert mode
imap jj <Esc>l

"shift insert pastes the x11 clipboard
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>

"ctrl-tab omnicompletes
inoremap <C-tab> <C-x><C-o>

""abbrevation: %% -> current dir of file
cabbr <expr> %% expand('%:p:h')

" If using a dark background within the editing area and syntax highlighting
set background=dark

"compile coffee script when file is written
au BufWritePost *.coffee silent CoffeeMake!
"make *.tpl highlight as html (needed for matchit to work)
au BufRead,BufNewFile *.tpl setlocal ft=html

autocmd FileType php call PhpSettings()

"i hate typing ->
function! PhpSettings()
	inoremap <buffer> - ->
endfunction

" have Vim jump to the last position when
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

filetype plugin indent on

if has("gui_running")
	colorscheme darkspectrum
	highlight StatusLine guibg=darkred
	highlight StatusLineNC guifg=purple
	highlight Comment guifg=salmon
	set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ 8 
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

set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set autowrite		" Automatically save before commands like :next and :make
set hidden             " Hide buffers when they are abandoned
"set mouse=a		" Enable mouse usage (all modes)
set number		"line numbers
set hlsearch		"Highlight search
set fileencodings=utf-8 
set wildmenu		"autocomplete menu
set wildmode=list:longest
set autoread 		"read file from disk when it changes
:set cpoptions+=$	"show a dollar sign when doing a change command
set nowrap		"dont word wrap
set laststatus=2	"always show the statusline
set encoding=utf-8
set autoindent
set scrolloff=3		"always show 3 line +/- from the cursor position
set cursorline		"show where the cursor is
set ttyfast
set gdefault		"all s// commands are global
set noexpandtab
set tabstop=4
set shiftwidth=4
set completeopt =menu,menuone,longest "get rid of scratch pad
set wildignore+=*.git,*.png,*.gif,*.jpg,tags
set magic	"make regexs more perl like
set backspace=indent,eol,start  " more powerful backspacing
set history=50          " keep 50 lines of command line history
set ruler               " show the cursor position all the time
set nomodeline
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc " Suffixes that get lower priority when doing tab completion for filenames.

"ctrlp
let g:ctrlp_working_path_mode = 0
let g:ctrlp_max_height = 20
let g:ctrlp_regexp_search = 1
let g:ctrlp_user_command = ['.git/', 'cd %s && git ls-files', 'find %s -type f']
let g:ctrlp_dont_split = ''
let g:ctrlp_arg_map = 0
let g:ctrlp_open_multi = '1'
let g:ctrlp_extensions = ['tag']

"taqbar
let g:tagbar_autoclose = 1
let g:tagbar_left = 1

"syntastic options
let s:php_executable = "/usr/bin/php"
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=1

"minibuf options
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplCheckDupeBufs = 0
let g:miniBufExplVSplit = 40   " column width in chars
let g:miniBufExplSplitBelow=1  " Put new window on right
hi MBENormal guifg=gray ctermfg=gray

"easy motion
let g:EasyMotion_leader_key = '<Leader>'

"vimclojure
let g:vimclojure#ParenRainbow = 1
let g:vimclojure#DynamicHighlighting = 1
let vimclojure#HighlightBuiltins=1
let vimclojure#HighlightContrib=1
"let vimclojure#WantNailgun = 1

"powerline
let g:Powerline_symbols='fancy'

"fix bug with easymotion
if has('gui_running')
	hi EasyMotionShade guifg=darkgrey guibg=black
else
	hi EasyMotionShade ctermfg=darkgrey ctermbg=black
endif

"highligh indents with whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
au BufEnter *.[^c][^l][^j] match ExtraWhitespace /^\s\{-}\zs[ ]\+/
