" Enable filetype plugins
filetype plugin on
filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
set t_Co=256
set virtualedit=onemore
au CursorHold * checktime  
" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","

" Fast saving
nmap <leader>w :w!<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set 7 lines to the cursor - when moving vertically using j/k
set so=7
 
" Avoid garbled characters in Chinese language windows OS
let $LANG='en' 
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the Wild menu
set wildchar=<Tab> wildmenu wildmode=full

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

"
set cursorline
" show command in bottom bari
set showcmd
"Always show current position
set ruler
"show number
set number
set rnu
" Height of the command bar
set cmdheight=2

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases 
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch 

" Don't redraw while executing macros (good performance config)
set lazyredraw 

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch 
" How many tenths of a second to blink when matching brackets
set mat=2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Enable syntax highlighting
syntax enable 
"color dracula
colorscheme gruvbox
set background=dark
"colorscheme solarized
" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8
" Use Unix as the standard file type
set ffs=unix,dos,mac


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use spaces instead of tab
set expandtab

" Be smart when using tabs ;)
set smarttab

" setting tab  spaces
set shiftwidth=4
set tabstop=4

nmap <leader>t4 :set expandtab tabstop=4 shiftwidth=4 softtabstop=4<CR>
nmap <leader>t2 :set expandtab tabstop=2 shiftwidth=2 softtabstop=2<CR>

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set nowrap "noWrap lines


""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""
" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

""""""""""""""""""""""""""""""
" =>quick save 
"""""""""""""""""""""""""""""
noremap <Leader>s :update<CR>

""""""""""""""""""""""""""""""
" =>clipboard copy
"""""""""""""""""""""""""""""
"set clipboard=unnamedplus
noremap <Leader>y "*y
"noremap <Leader>p "*p
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Map <Space> to / (search) and Ctrl-<Space> to ? (backwards search)
map <space> /
map <c-space> ?

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer
map <leader>bd :Bclose<cr>:tabclose<cr>gT

" Close all the buffers
map <leader>ba :bufdo bd<cr>

map <leader>l :bnext<cr>
map <leader>h :bprevious<cr>
" go to buffer n
map <leader>gn :bn<cr>
noremap <leader>b :ls<CR>:b<Space>


" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 
map <leader>t<leader> :tabnext 

" Let 'tl' toggle between this and the last accessed tab
let g:lasttab = 1
nmap <Leader>tl :exe "tabn ".g:lasttab<CR>
au TabLeave * let g:lasttab = tabpagenr()


" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" Format the status line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap VIM 0 to first non-blank character
map 0 ^
" delete word after cusor
inoremap <C-F> <C-o>de

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

if has("mac") || has("macunix")
  nmap <D-j> <M-j>
  nmap <D-k> <M-k>
  vmap <D-j> <M-j>
  vmap <D-k> <M-k>
endif

" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

if has("autocmd")
    autocmd BufWritePre *.txt,*.js,*.py,*.wiki,*.sh,*.coffee :call CleanExtraSpaces()
endif


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Quickly open a buffer for scribble
map <leader>q :e ~/buffer<cr>

" Quickly open a markdown buffer for scribble
map <leader>x :e ~/buffer.md<cr>

" Toggle paste mode on and off
map <leader>pp :setlocal paste!<cr>

""""""""""""""""""""""""""""""
" => JavaScript section
"""""""""""""""""""""""""""""""
au FileType javascript call JavaScriptFold()
au FileType javascript setl fen
au FileType javascript setl nocindent

au FileType javascript imap <c-t> $log();<esc>hi
au FileType javascript imap <c-a> alert();<esc>hi

au FileType javascript inoremap <buffer> $r return
au FileType javascript inoremap <buffer> $f // --- PH<esc>FP2xi

function! JavaScriptFold()
    setl foldmethod=syntax
    setl foldlevelstart=100
    syn region foldBraces start=/{/ end=/}/ transparent fold keepend extend

    function! FoldText()
        return substitute(getline(v:foldstart), '{.*', '{...}', '')
    endfunction
    setl foldtext=FoldText()
endfunction
au FileType javascript setl nofoldenable 
"hight light syntax
"let g:javascript_plugin_jsdoc = 1
"let g:jsx_pragma_required = 0
"let g:jsx_ext_required = 0 " Works on files other than .jsx
let g:indentLine_char = '.'

"""""""""""""""""""""""""""""
"for CSS
""""""""
augroup VimCSS3Syntax
  autocmd!

  autocmd FileType css setlocal iskeyword+=-
augroup END

""""""""""""""""""""""""""""""
" => Shell section
""""""""""""""""""""""""""""""
set termguicolors

" disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Properly disable sound on errors on MacVim
if has("gui_macvim")
    autocmd GUIEnter * set vb t_vb=
endif
" """""""""""""""""""""""""""""""
" =>vim plugins
" """""""""""""""""""""""""""""""""
call plug#begin('~/.vim/plugged')

Plug 'https://github.com/scrooloose/nerdtree.git'
"Plug 'itchyny/lightline.vim'
Plug 'https://github.com/itchyny/vim-gitbranch.git'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'morhetz/gruvbox'
Plug 'https://github.com/pangloss/vim-javascript.git'
Plug 'mxw/vim-jsx'
Plug 'https://github.com/hail2u/vim-css3-syntax.git'
Plug 'https://github.com/ap/vim-css-color.git'
Plug 'kien/ctrlp.vim'
Plug 'ruanyl/vim-fixmyjs'
Plug 'https://github.com/Yggdroot/indentLine.git'
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdcommenter'
Plug 'https://github.com/elmcast/elm-vim.git'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'https://github.com/tpope/vim-fugitive'
Plug 'https://github.com/mhinz/vim-signify'
Plug 'https://github.com/chrisbra/changesPlugin'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'prettier/vim-prettier', {
  \ 'do': 'yarn install',
  \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss', 'json', 'graphql', 'markdown', 'vue', 'yaml', 'html'] }
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'chr4/nginx.vim'
Plug 'elixir-editors/vim-elixir'
Plug 'mhinz/vim-mix-format'

call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDTreeWinPos = "left"
let NERDTreeShowHidden=0
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
let g:NERDTreeWinSize=35
let g:NERDTreeNodeDelimiter = "\u00a0"
map <leader>nn :NERDTreeToggle<cr>
map <leader>nb :OpenBookmark<Space>
map <leader>nf :NERDTreeFind<cr>
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CtrlP
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 0
map <c-b> :CtrlPMix <cr>
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe  " Windows

"let g:ctrlp_custom_ignore = {
  "\ 'dir':  '\v[\/]\.(git|hg|svn)$',
  "\ 'file': '\v\.(exe|so|dll)$',
  "\ 'link': 'some_bad_symbolic_links',
  "\ }
"let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git|elm-stuff'
let g:ctrlp_custom_ignore = {
  \ 'dir':  'node_modules\|DS_Store\|giti\|elm-stuff',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'some_bad_symbolic_links',
  \ }
let g:ctrlp_max_files=0
let g:ctrlp_max_depth=100
""let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
"if executable('ag')
  "let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
"endif
""let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_match_window = 'results:100'

 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => light line
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:lightline = {
      "\ 'colorscheme': 'one',
      "\ 'active': {
      "\   'left': [ [ 'mode', 'paste' ],
      "\             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      "\ },
      "\ 'component_function': {
      "\   'gitbranch': 'gitbranch#name'
      "\ },
      "\ }
"let g:airline_theme='base16'
let g:airline#extensions#branch#enabled=1
let g:airline#extensions#tabline#enabled                      = 1
let g:airline#extensions#tabline#buffer_min_count             = 1
let g:airline#extensions#tabline#tab_min_count                = 1
let g:airline#extensions#tabline#buffer_idx_mode              = 1
let g:airline#extensions#tabline#buffer_nr_show               = 0
let g:airline#extensions#tabline#show_buffers                 = 1
let g:airline#extensions#branch#enabled                       = 1
let g:airline#extensions#tagbar#enabled                       = 1
let g:airline_powerline_fonts                                 = 1
let g:airline#extensions#whitespace#enabled       = 0
let g:airline#extensions#tabline#fnamemod         = ':t'
let g:airline_theme                               = 'base16'
let g:airline_section_c                           = '%{fnamemodify(expand("%"), ":~:.")}'
let g:airline_section_x                           = '%{fnamemodify(getcwd(), ":t")}'
let g:airline_section_y                           = airline#section#create(['filetype'])

" Easier tab/buffer switching
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"JS syxtax check
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:ale_linters = {
"\   'javascript': ['eslint'],
"\}

"nmap <silent> <leader>a <Plug>(ale_next_wrap)
"let g:syntastic_javascript_checkers = ['eslint']

"" autofix with eslint
"let g:syntastic_javascript_eslint_args = ['--fix']
"function! SyntasticCheckHook(errors)
  "checktime
"endfunction

" Disabling highlighting
"let g:ale_set_highlights = 0

" Only run linting when saving the file
"let g:ale_lint_on_text_changed = 'never'
"let g:ale_lint_on_enter = 0
"eslint auto fix
noremap <Leader><Leader>f :Fixmyjs<CR>
"vim repeat
silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)

"comment code
let g:NERDCustomDelimiters={
	\ 'javascript': { 'left': '//', 'right': '', 'leftAlt': '{/*', 'rightAlt': '*/}' },
\}
"""""""
"elm lang
"""""
"let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
"let g:syntastic_elm_checkers = [ 'elm_make' ]

"let g:elm_syntastic_show_warnings = 1
"let g:elm_make_show_warnings = 0
"let g:elm_jump_to_error = 1
let g:ycm_semantic_triggers = {
     \ 'elm' : ['.'],
     \}
"""'ctags configs
"g:easytags_always_enabled = 1
"let g:easytags_events = ['BufWritePost']
"let g:easytags_suppress_report = 1
"let g:easytags_always_enabled = 1
let g:easytags_cmd = '/usr/local/bin/ctags'

let g:easytags_languages = {
\   'language': {
\     'cmd': g:easytags_cmd,
\	    'args': [],
\	    'fileoutput_opt': '-f',
\	    'stdout_opt': '-f-',
\	    'recurse_flag': '-R'
\   }
\}

"""""""
"elixir
"""""""
let g:mix_format_on_save = 1


"prettier
let g:prettier#autoformat = 1
autocmd FileType javascript setlocal equalprg=js-beautify\ --stdin
"gotags
let g:tagbar_type_go = {
	\ 'ctagstype' : 'go',
	\ 'kinds'     : [
		\ 'p:package',
		\ 'i:imports:1',
		\ 'c:constants',
		\ 'v:variables',
		\ 't:types',
		\ 'n:interfaces',
		\ 'w:fields',
		\ 'e:embedded',
		\ 'm:methods',
		\ 'r:constructor',
		\ 'f:functions'
	\ ],
	\ 'sro' : '.',
	\ 'kind2scope' : {
		\ 't' : 'ctype',
		\ 'n' : 'ntype'
	\ },
	\ 'scope2kind' : {
		\ 'ctype' : 't',
		\ 'ntype' : 'n'
	\ },
	\ 'ctagsbin'  : 'gotags',
	\ 'ctagsargs' : '-sort -silent'
\ }
