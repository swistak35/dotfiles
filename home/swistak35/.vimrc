
"""" Vundle section
set nocompatible              " be iMproved, required
filetype off                  " required
set hidden

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/syntastic'
Plugin 'vim-ruby/vim-ruby'
Plugin 'tpope/vim-markdown'
Plugin 'pangloss/vim-javascript'
Plugin 'groenewege/vim-less'
Plugin 'kchmck/vim-coffee-script'
Plugin 'jimmyhchan/dustjs.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ap/vim-css-color'
Plugin 'docunext/closetag.vim'
Plugin 'hail2u/vim-css3-syntax'
Plugin 'mattn/webapi-vim'
Plugin 'mattn/gist-vim'
Plugin 'mhinz/vim-signify'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-endwise'
Plugin 'Raimondi/delimitMate'
Plugin 'elzr/vim-json'
Plugin 'tpope/vim-fugitive'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'szw/vim-tags'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-sleuth'
Plugin 'taglist.vim'
Plugin 'tpope/vim-unimpaired'
Plugin 'bkad/CamelCaseMotion'
Plugin 'argtextobj.vim'
Plugin 'kana/vim-textobj-user'
Plugin 'nelstrom/vim-textobj-rubyblock'
Plugin 'whatyouhide/vim-textobj-xmlattr'
Plugin 'lucapette/vim-textobj-underscore'
Plugin 'mattn/emmet-vim'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-repeat'
Plugin 'mileszs/ack.vim'
Plugin 'tpope/vim-surround'
Plugin 'Align'
Plugin 'Rename2'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin on    " required
" filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on

" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set backup

" when there are cvs, backup is more trouble than gain
set noswapfile
set undofile
set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif




 """Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
			  \ | wincmd p | diffthis
endif

" Colorscheme
if has('gui_running')
  syntax enable
  set background=dark
  colorscheme solarized
else

endif

set wildmenu
set wildmode=list:longest,full
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.pyc,*.class,.svn,*.gem
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*.jpg,*.png,*.gif,*.avi,*.wmv,*.ogg,*.mp3,*.mov
set wildignore+=*/vendor/cache/*,*/.sass-cache/*,*/node_modules/*,*/bower_components/*
set wildignore+=*.swp,*~,._*
set number
set shiftround
set shiftwidth=2
map <F2> :NERDTreeToggle<CR>
map <F3> :TlistToggle<CR>
imap <c-k> <esc>ddi
nmap ; :
set smartcase
let mapleader=","

set scrolloff=5

cmap w!! %!sudo tee > /dev/null %

" Remove trailing spaces after save
au BufWritePre *.json,*.rb,*.py,*.c,*.h,*.feature,*.conf,*rc,README,CHANGELOG,README.* :%s/\s\+$//e

" Don't create .un~ and swp files in my working directory!
set backupdir=~/.vim/tmp,.
set directory=~/.vim/swp,.
set undodir=~/.vim/undo,.

" Expand tabs to spaces
" set expandtab
" set tabstop=2

" Per directory vimrc
set exrc
set secure

" Make command autocompletion case insensitive
set ignorecase

" Arrows? No. Just No. Not now.
map <Left> <C-w>h
map <Right> <C-w>l
map <Up> <C-w>k
map <Down> <C-w>j
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

inoremap jj <ESC>

nnoremap <leader>w <C-w>v<C-w>l



" Redrawing the screen also removes the hightlight from search
noremap <silent> <C-l> :nohls<CR><C-l>

set relativenumber

" Let's make global changing default
set gdefault

""""""" Plugin settings

" Syntastic
let g:syntastic_ruby_checkers = ['mri']
let g:syntastic_auto_jump = 1

""" CamelCase
map <S-W> <Plug>CamelCaseMotion_w
map <S-B> <Plug>CamelCaseMotion_b
map <S-E> <Plug>CamelCaseMotion_e

""" NerdTree
" Run Nerdtree on startup
" autocmd vimenter * if !argc() | NERDTree | endif
" If the only window left when quitting is NERDTree, then quit whole vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

""" Gist plugin settings
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1
let g:gist_browser_command = 'firefox %URL%'

""" Signify plugin settings
let g:signify_vcs_list = [ 'git', 'hg' ]
" let g:signify_line_highlight = 1

""" Airline
set laststatus=2
set noshowmode

""" Emmet (zen coding)
" map <C-Y>y zey yoyiygyyy


"""""""" Other functions
" This allows for change paste motion cp{motion}
nmap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction

" Create absent directories on edit
function s:MkNonExDir(file, buf)
  if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
    let dir=fnamemodify(a:file, ':h')
    if !isdirectory(dir)
      call mkdir(dir, 'p')
    endif
  endif
endfunction
augroup BWCCreateDir
  autocmd!
  autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
