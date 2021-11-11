function! Cond(cond, ...)
  let opts = get(a:000, 0, {})
  return a:cond ? opts : extend(opts, { 'on': [], 'for': [] })
endfunction

if has('nvim')
  call plug#begin('~/.nvim/plugged')
else
  call plug#begin('~/.vim/plugged')
endif 

" Obvious
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim'
Plug 'bling/vim-airline'
Plug 'tpope/vim-rails'
Plug 'tpope/vim-projectionist'
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --tern-completer' }
Plug 'tpope/vim-fugitive'
" Plug 'fisadev/vim-ctrlp-cmdpalette'
Plug 'mileszs/ack.vim'
" Plug 'gorkunov/smartgf.vim'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'dense-analysis/ale'

" Languages
" Plug 'vim-ruby/vim-ruby'
Plug 'pangloss/vim-javascript'
Plug 'groenewege/vim-less'
" Plug 'kchmck/vim-coffee-script'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-cucumber'
Plug 'hail2u/vim-css3-syntax'
Plug 'elixir-lang/vim-elixir'
Plug 'idris-hackers/idris-vim'
Plug 'cypok/vim-sml'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'LnL7/vim-nix'
Plug 'ElmCast/elm-vim'
Plug 'rust-lang/rust.vim'
Plug 'leafgarland/typescript-vim'

" Langserver support
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'bash install.sh' }
" Plug 'tjdevries/nvim-langserver-shim'

""" Color schemes
" Solarized
" Plug 'altercation/vim-colors-solarized'
" PaperColor
Plug 'NLKNguyen/papercolor-theme'
" Pencil
Plug 'reedes/vim-colors-pencil'
" Inkpot
Plug 'ciaranm/inkpot'
" Flattened (solarized simplified)
Plug 'romainl/flattened'

" Support for github in fugitive.vim
Plug 'tpope/vim-rhubarb'
" Highlight colors in CSS files
Plug 'ap/vim-css-color'
" Nice incremental searching
Plug 'haya14busa/incsearch.vim', Cond(!has('nvim'))
" Autoclose xml tags
" Plug 'docunext/closetag.vim'

" Underscoring words which are the same as current
" Crashing in newest version
" Plug 'vim-cursorword'

" Let vim know about end keyword
Plug 'tpope/vim-endwise'
" Hunks about modified lines
Plug 'mhinz/vim-signify'
" Autocompletion for quotes, braces etc.
Plug 'Raimondi/delimitMate'
" Sublime-like multiple cursors
Plug 'terryma/vim-multiple-cursors'
" auto good tabswidth
Plug 'tpope/vim-sleuth'
" cool ][ mappings
Plug 'tpope/vim-unimpaired'
" taglist bound to F3
Plug 'vim-scripts/taglist.vim'
" Some plugins need it, and then they can run tasks in background
Plug 'tpope/vim-dispatch'
" Changing theme between day & night
" Plug 'swistak35/bgshift.vim'
" snippets plugin
" Plug 'SirVer/ultisnips'
" snippets
Plug 'swistak35/my-snippets.vim'
" show "At match #N out out M matches" during search
Plug 'henrik/vim-indexed-search'
" Move with same shortcuts between panes and vim splits
Plug 'christoomey/vim-tmux-navigator'
" For ability to press v, vv, vvv, ...
Plug 'terryma/vim-expand-region'
" Display git information in NERDTree
Plug 'Xuyuanp/nerdtree-git-plugin'
" Buffer explorer. Bar at the top
" Removed because it didn't fit my flow
" Plug 'weynhamz/vim-plugin-minibufexpl'
" Undotree
Plug 'mbbill/undotree'
" Better matcher for CtrlP - requires python
" Plug 'FelikZ/ctrlp-py-matcher'
" Make focus events work with vim inside a tmux
Plug 'tmux-plugins/vim-tmux-focus-events'
" Display full YAML path of a key in a YAML file
" Plug 'Einenlum/yaml-revealer'

" Github support for fugitive
" Plug 'tpope/vim-rhubarb'

" Plug 'vim-utils/vim-man'

" Text objects
Plug 'bkad/CamelCaseMotion'
Plug 'vim-scripts/argtextobj.vim'
Plug 'kana/vim-textobj-user'
Plug 'nelstrom/vim-textobj-rubyblock'
Plug 'whatyouhide/vim-textobj-xmlattr'
Plug 'lucapette/vim-textobj-underscore'
Plug 'mattn/vim-textobj-url'
Plug 'whatyouhide/vim-textobj-erb'


Plug 'joker1007/vim-ruby-heredoc-syntax'

" Disables some features when opening very large files ( > 100 MB )
Plug 'vim-scripts/LargeFile'

" Extensions (very experimental!) to show commit msgs in fugitive
Plug 'tommcdo/vim-fugitive-blame-ext'

" Distraction free mode
Plug 'junegunn/goyo.vim'

" Focus only on paragraph
Plug 'junegunn/limelight.vim'

" Running Ruby specs
Plug 'janko-m/vim-test'

" Run cmds in tmux
Plug 'christoomey/vim-tmux-runner'

" Close all invisible buffers
Plug 'artnez/vim-wipeout'

" Add .gitignore to wildignore
Plug 'vim-scripts/gitignore'

" Support for nice use for JSON files (don't display quotes, etc.)
" Plug 'elzr/vim-json'

" Improvements for clipboard
Plug 'svermeulen/vim-easyclip'

Plug 'mattn/emmet-vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'junegunn/vim-easy-align'
Plug 'vim-scripts/Rename2'
Plug 'mmozuras/vim-github-comment'
Plug 'vim-scripts/indentLine.vim'
Plug 'vim-scripts/matchit.zip'
Plug 'ecomba/vim-ruby-refactoring'
Plug 'tpope/vim-commentary'
Plug 'vim-scripts/pipe2eval', { 'do': 'chmod +x plugin/pipe2eval.sh' }
Plug 'vim-scripts/textobj-indent'
Plug 'vim-scripts/dbext.vim'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'AndrewRadev/switch.vim'
Plug 'KabbAmine/zeavim.vim'
Plug 't9md/vim-chef'
" Plug 'benekastah/neomake', Cond(has('nvim'))
Plug 'szw/vim-tags'

Plug 'the-lambda-church/merlin'

Plug '~/projs/my-prototype-plugin'

" VimOS fun

""" DISABLED PLUGINS
" Plug 'tpope/vim-markdown'
" Plug 'Shougo/vimproc.vim', { 'do': 'make' }
" Plug 'Shougo/neocomplete.vim'
" Plug 'osyo-manga/vim-monster'
" Plug 'tpope/vim-markdown'
" Plug 'tacahiroy/ctrlp-funky'
" Plug 'garbas/vim-snipmate'
" Plug 'netrw.vim'
" Plug 'vim-hackernews'

" Dependencies for vim-snipmate
" Plug 'marcweber/vim-addon-mw-utils'
" Plug 'tomtom/tlib_vim'
" Not using snipmate

" Plug 'FredKSchott/CoVim'
" Plug 'floobits/floobits-neovim'
" Because tmate ftw

" Deleting buffers in CtrlP
" Plug 'd11wtq/ctrlp_bdelete.vim'
" Doesn't really work

" Shortcut for changing do..end blocks to {..}
" Plug 'jgdavey/vim-blockle'
" not needed now, splitjoin.vim is faster

" For writing (i.e. blogposts)
" Plug 'reedes/vim-pencil'
" Very nice soft wrapping, but it slows down vim a lot, no idea why

" Plug 'gabebw/vim-spec-runner'

call plug#end()

" Don't use Ex mode, use Q for formatting
" map Q g

"""""""""""""""""""""""""""""""""""""
""""""""""    My options
"""""""""""""""""""""""""""""""""""""

set autowriteall
set background=dark
set backspace=indent,eol,start
set backup
set backupdir=~/.vim/tmp,.
set clipboard=unnamed,unnamedplus           "save to system clipboard by default
set nocompatible
set directory=~/.vim/swp,.
set expandtab                   "expand tabs to spaces
set exrc                        "per directory vimrc
set gdefault                    "let's make global changing default
set history=50                  "keep 50 lines of command line history
set ignorecase                  "make command autocompletion case insensitive
set incsearch                   "do incremental searching
set laststatus=2                "needed by Airline
set list                        "look of invisible characters
set listchars=tab:▸\ ,eol:¬
set number
if has('mouse')                 " in many terminal emulators
  set mouse=a                   "   the mouse works just fine,
endif                           "   thus enable it.

" Potentially can lag ruby files
set relativenumber " https://github.com/vim-ruby/vim-ruby/issues/243
set lazyredraw " https://github.com/vim-ruby/vim-ruby/issues/243

" set cursorline
set ruler                       "show the cursor position all the time
set scrolloff=5
set secure
set shiftround
set shiftwidth=2
set showcmd                     "display incomplete commands
set noshowmode
set smartcase
set noswapfile
set tabstop=4
set colorcolumn=80
set t_Co=256                    "256colors support. Required by airline
" set t_AB=^[[48;5;%dm
" set t_AF=^[[38;5;%dm
set undodir=~/.vim/undo,.
set undofile
set wildignore+=*.o,*.out,*.obj,.git,*.rbc,*.pyc,*.class,.svn,*.gem
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
set wildignore+=*.jpg,*.png,*.gif,*.avi,*.wmv,*.ogg,*.mp3,*.mov
set wildignore+=*/vendor/cache/*,*/.sass-cache/*,*/node_modules/*,*/bower_components/*
set wildignore+=*.swp,*~,._*
set wildmenu
set wildmode=list:longest,full
set autoread " Automatically reload files if they've changed on the disk

" Folding
" foldmethod=syntax works better, but on big ruby files it slows vim down very much
set foldmethod=indent
set foldlevel=2
set foldnestmax=5

" New feature in Neovim: https://neovim.io/news/2016/11/
if has('nvim')
  set inccommand=split
endif

colorscheme PaperColor

"""""""""""""""""""""""""""""""""""""
""""""""""    Functions
"""""""""""""""""""""""""""""""""""""

function! s:MkNonExDir(file, buf)
  if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
    let dir=fnamemodify(a:file, ':h')
    if !isdirectory(dir)
      call mkdir(dir, 'p')
    endif
  endif
endfunction

function! GetYaml()
  let s:yaml_path = system('ruby /home/swistak35/projs/priv/give-me-full-key.rb ' . expand('%') . ' ' . line('.'))
  echo s:yaml_path
endfunction

"""""""""""""""""""""""""""""""""""""
""""""""""    Mappings
"""""""""""""""""""""""""""""""""""""

let mapleader=","
let maplocalleader="\\"

nnoremap ; :
vnoremap ; :

" Save even if you don't have permissions
cmap w!! %!sudo tee > /dev/null %

" Display hidden characters
" nmap <leader>s :set nolist!<CR>

" Arrows? No. Just No. Not now.
nnoremap <Left> <C-w>h
nnoremap <Right> <C-w>l
nnoremap <Up> <C-w>k
nnoremap <Down> <C-w>j
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" Move by editor lines, not file lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k

" Join lines
nnoremap gJ J

" Move by pages
nnoremap J <C-D>
vnoremap J <C-D>
nnoremap K <C-U>
vnoremap K <C-U>

" Escapes
inoremap jj <ESC>

" Splits
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <leader>h <C-w>s<C-w>j

" Change tabs
noremap [z :tabp<CR>
noremap ]z :tabn<CR>

" Play q macro
nnoremap Q @q

" Save all
nnoremap <leader>w :wa<CR>

" Insert just one character
" nnoremap s i_<ESC>r
" nnoremap S a_<ESC>r

" Reloading config
nnoremap <leader>R :source ~/.config/nvim/init.vim<CR>

" Jumping to next occurs of the searched letter
nnoremap gn ;
nnoremap gN ,

" Redrawing the screen also removes the hightlight from search
noremap <silent> <C-l> :nohls<CR><C-l>

" List buffers
nnoremap <leader>bls :ls<CR>:b<Space>

" Copy to clipboard
nnoremap <leader>cy "*y

" Pastemode
nnoremap <F7> :set paste!<CR>

" turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

" Nice ruby mappings
nmap <localleader>f $varzf

" open/closes folds
" nnoremap <space> za
" nnoremap <S-space> zA
nnoremap <expr> <space> foldclosed('.') != -1 ? 'zO' : 'zc'
nnoremap zX :set foldlevel=2<CR>zX

" Copy file path of current file to clipboard
nnoremap <localleader>fpy :let @+ = expand("%")<CR>

"""""""""""""""""""""""""""""""""""""
""""""""""    Commands
"""""""""""""""""""""""""""""""""""""

command! JoinLines execute "g/^$/,/./-j"

command! DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis

"""""""""""""""""""""""""""""""""""""
""""""""""    Plugins
"""""""""""""""""""""""""""""""""""""

""" dense-analysis/ale
let g:ale_fixers = {'javascript': ['prettier', 'eslint'], 'ruby': ['rubocop']}
let g:ale_fix_on_save = 0
let g:ale_linters = {'typescript': [], 'ruby': ['rubocop']}
let g:ale_linters_explicit = 1
let g:ale_completion_enabled = 1
set omnifunc=ale#completion#OmniFunc



"""
let g:ruby_heredoc_syntax_defaults = {}
let g:ruby_heredoc_syntax_filetypes = {
      \ "json" : {
      \   "start" : "JSON",
      \},
      \ "javascript" : {
      \   "start" : "JS",
      \},
      \ "sql" : {
      \   "start" : "SQL",
      \},
      \ "html" : {
      \   "start" : "HTML",
      \},
      \ "sh" : {
      \   "start" : "SH",
      \},
\}

""" ternsjs/tern_for_vim
" let g:tern_show_argument_hints = "on_move"
" let g:tern_show_signature_in_pum = 1

""" svermeulen/vim-easyclip
" Autoformat doesn't seem to work?
let g:EasyClipAutoFormat = 1
" Disable the feature that dd does not yank anymore
let g:EasyClipEnableBlackHoleRedirect = 0
" Disable keybindings which override marking
let g:EasyClipUseCutDefaults = 0
" Disable default keybindings
let g:EasyClipUsePasteDefaults = 0
nmap P <plug>EasyClipPasteBefore
nmap p <plug>EasyClipPasteAfter
xmap P <plug>EasyClipPasteBefore
xmap p <plug>EasyClipPasteAfter
nmap [w <plug>EasyClipSwapPasteForward
nmap ]w <plug>EasyClipSwapPasteBackwards
" Yank additional keybindings
nmap [W <plug>EasyClipRotateYanksForward
nmap ]W <plug>EasyClipRotateYanksBackward

""" autozimu/LanguageClient-neovim
set hidden

let g:LanguageClient_serverCommands = {
    \ 'ruby': ['orbaclerun', 'file-server'],
    \ 'python': ['pyls'],
    \ }
nnoremap <localleader>lj :call LanguageClient_textDocument_definition()<CR>
nnoremap T :call LanguageClient_textDocument_hover()<CR>
nnoremap <localleader>ls :LanguageClientStart<CR>
nnoremap <localleader>lr :LanguageClientStop<CR>:LanguageClientStart<CR>
let g:LanguageClient_loggingFile = '/tmp/LanguageLog.log'
let g:LanguageClient_loggingLevel = 'DEBUG'
let g:LanguageClient_waitOutputTimeout = 240
" let g:LanguageClient_hoverPreview = 'Always'

""" mbbill/undotree
nnoremap <F5> :UndotreeToggle<cr>
let g:undotree_WindowLayout = 2
function! g:Undotree_CustomMap()
  nmap <buffer> K <plug>UndotreeGoNextState
  nmap <buffer> J <plug>UndotreeGoPreviousState
endfunc

function! g:JumpToFact()
  let line_to_jump = system("./script/editor_jump_to_fact.rb ". @% . " " . (line(".") - 1) . " " . (col(".") - 1) . " 2> /dev/null")
  :execute ":e config/initializers/application_subscriptions.rb"
  :execute "normal! " . (line_to_jump + 1) . "G"
endfunction
nnoremap <localleader>ff :call JumpToFact()<CR>

""" KabbAmine/zeavim.vim
let g:zv_zeal_directory = "/usr/bin/zeal"
let g:zv_added_files_type = {
            \ 'rb': 'Ruby'
            \ }

""" scrooloose/nerdtree
nnoremap <F2> :NERDTreeToggle<CR>
" Run Nerdtree on startup
" autocmd vimenter * if !argc() | NERDTree | endif
" If the only window left when quitting is NERDTree, then quit whole vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
" DOESNT WORK - Single click for opening in trees
" let NERDTreeMouseMode = 3
" Show Bookmarks after startup
let NERDTreeShowBookmarks = 1
" Don't display additional messages in nerdtree
let NERDTreeMinimalUI = 1
" Don't display confirmation step after removing/renaming a file
" let NERDTreeAutoDeleteBuffer=1
let NERDTreeIgnore = ['\.pyc$']


""" taglist.vim
nnoremap <F3> :TlistToggle<CR>

""" AndrewRadev/switch.vim
nnoremap gs :Switch<CR>

""" docunext/closetag.vim
let g:closetag_filenames = "*.html,*.xml,*.html.erb"

""" pipe2eval
let g:pipe2eval_map_key = '<Leader>ev'

""" tpope/vim-fugitive
nnoremap <leader>gtc :Gcommit<CR>
nnoremap <leader>gtp :Gpush<CR>
nnoremap <leader>gtu :Gpull<CR>
nnoremap <leader>gtd :Gdiff<CR>
nnoremap <leader>gtb :Gblame -w -M<CR>

""" bling/vim-airline
let g:airline_powerline_fonts = 1

""" elzr/vim-json ???
let g:vim_json_syntax_conceal = 0

""" benekastah/neomake
" if has('nvim')
"   autocmd! BufWritePost,BufEnter * Neomake
"   let g:neomake_c_enabled_markers = ['clang']
"   let g:neomake_cpp_enabled_markers = ['clang++']
"   let g:neomake_coffeescript_enabled_markers = ['coffeelint']
"   let g:neomake_ruby_enabled_markers = ['rubocop']
"   let g:neomake_sh_enabled_markers = ['shellcheck']
"   let g:neomake_zsh_enabled_markers = ['shellcheck']
"   let g:neomake_jsx_enabled_markers = ['jsxlint']
"   let g:neomake_json_enabled_markers = ['jsonlint']
"   let g:neomake_javascript_enabled_markers = ['eslint']
" endif

""" bkad/CamelCaseMotion
map <S-W> <Plug>CamelCaseMotion_w
map <S-B> <Plug>CamelCaseMotion_b
map <S-E> <Plug>CamelCaseMotion_e

""" mattn/gist-vim
let g:gist_detect_filetype = 1
let g:gist_open_browser_after_post = 1
let g:gist_post_private = 1
let g:gist_browser_command = 'firefox %URL%'

""" mhinz/vim-signify
let g:signify_vcs_list = [ 'git', 'hg' ]
" let g:signify_line_highlight = 1

""" junegunn/vim-easy-align
vnoremap <leader>a= :EasyAlign =<CR>
vnoremap <leader>asp :EasyAlign *\ <CR>
vnoremap <leader>as1 :EasyAlign\ <CR>

""" indentLine.vim
" let g:indentLine_noConcealCursor=""

""" mileszs/ack.vim
nnoremap <leader>a :Ack!<Space>
if executable('ag')
  let g:ackprg = 'ag --vimgrep --column'
endif
if executable('rg')
  let g:ackprg = 'rg --no-heading --smart-case --vimgrep'
endif
let g:ack_autofold_results = 0
let g:ackpreview = 0
" let g:ack_default_options = " -s -H --nocolor --nogroup --column --smart-case"
let g:ack_default_options = " -s -H --nocolor --nogroup --column --smart-case --ignore-dir=log --ignore-dir=vendor --ignore-dir=tmp --ignore-dir=locale"
" let g:ack_use_dispatch = 1

""" ctrlpvim/ctrlp.vim
" let g:ctrlp_extensions = ['funky']
" let g:ctrlp_funky_syntax_highlight = 1
" let g:ctrlp_custom_tag_files = ['.git/tags']
" Always open in new buffers
" let g:ctrlp_switch_buffer = 0
" Fixes the bug with files opening in nerdtree
" let g:ctrlp_dont_split = 'NERD'
" let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" if executable("ag")
"   set grepprg=ag\ --nogroup\ --nocolor
"   let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --ignore ''.git'' --ignore ''.DS_Store'' --ignore ''node_modules'' --hidden -g ""'
" endif
" Set no file limit, we are building a big project
" let g:ctrlp_max_files = 0
" nmap <Leader>pp :CtrlP<CR>
" nmap <Leader>pb :CtrlPBuffer<CR>
" nmap <Leader>pc :CtrlPCmdPalette<CR>
" nmap <Leader>pm :CtrlPMRUFiles<CR>
" nmap <leader>pr :ClearCtrlPCache<CR>
" nmap <leader>pt :CtrlPTag<CR>
nmap <leader>pb :Buffers<CR>
nmap <leader>pp :GFiles --cached --others --exclude-standard<CR>
nmap <leader>pl :BLines<CR>

""" FelikZ/ctrlp-py-matcher
" PyMatcher for CtrlP
" if !has('python')
"   echo 'In order to use pymatcher plugin, you need +python compiled vim'
" else
"   let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
" endif

""" mmozuras/vim-github-comment
let g:github_user='swistak35'
let g:github_open_browser=1

""" haya14busa/incsearch.vim
if !has('nvim')
  set hlsearch
  let g:incsearch#magic = '\v'
  let g:incsearch#auto_nohlsearch = 1
  map / <Plug>(incsearch-forward)
  map ? <Plug>(incsearch-backward)
  map g/ <Plug>(incsearch-stay)
  map n  <Plug>(incsearch-nohl-n)
  map N  <Plug>(incsearch-nohl-N)
  map *  <Plug>(incsearch-nohl-*)
  map #  <Plug>(incsearch-nohl-#)
  map g* <Plug>(incsearch-nohl-g*)
  map g# <Plug>(incsearch-nohl-g#)
endif

""" gorkunov/smartgf.vim
" let g:smartgf_create_default_mappings = 0
" let g:smartgf_auto_refresh_ctags = 0
" let g:smartgf_enable_gems_search = 1
" nmap gu <Plug>(smartgf-search)
" vmap gu <Plug>(smartgf-search)
" nmap gU <Plug>(smartgf-search-unfiltered)
" vmap gU <Plug>(smartgf-search-unfiltered)
" map <F4> :SmargfRefreshTags<CR>
" autocmd! BufWritePost * SmargfRefreshTags

""" christoomey/vim-tmux-runner
let g:VtrPercentage = 15
let g:VtrClearEmptyLines = 1
let g:VtrInitialCommand = "ls"

""" junegunn/limelight.vim
let g:limelight_default_coefficient = 0.5
let g:limelight_paragraph_span = 1
" Color name (:help cterm-colors) or ANSI code
let g:limelight_conceal_ctermfg = 'gray'
" let g:limelight_conceal_ctermfg = 240
" Color name (:help gui-colors) or RGB color
let g:limelight_conceal_guifg = 'DarkGray'
" let g:limelight_conceal_guifg = '#777777'

""" junegunn/goyo.vim
autocmd User GoyoEnter Limelight
autocmd User GoyoLeave Limelight!
nnoremap <localleader>gy :Goyo<CR>

""" vim-test
let test#strategy = "vtr"
nmap <silent> <localleader>tn :TestNearest<CR>
nmap <silent> <localleader>tf :TestFile<CR>
nmap <silent> <localleader>ts :TestSuite<CR>
nmap <silent> <localleader>tl :TestLast<CR>
nmap <silent> <localleader>tv :TestVisit<CR>

""" SirVer/ultisnips
" let g:UltiSnipsExpandTrigger="<c-e>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"

""" tpope/vim-rails
nmap <localleader>rm :Rmigration<CR>

""" christoomey/vim-tmux-navigator
let g:tmux_navigator_no_mappings = 1

nnoremap <silent> <C-w>h :TmuxNavigateLeft<cr>
nnoremap <silent> <C-w>j :TmuxNavigateDown<cr>
nnoremap <silent> <C-w>k :TmuxNavigateUp<cr>
nnoremap <silent> <C-w>l :TmuxNavigateRight<cr>
tnoremap <C-w>h <C-\><C-N><C-w>h
tnoremap <C-w>j <C-\><C-N><C-w>j
tnoremap <C-w>k <C-\><C-N><C-w>k
tnoremap <C-w>l <C-\><C-N><C-w>l

""" the-lambda-church/merlin
set rtp+=/usr/local/share/ocamlmerlin/vim

""" szw/vim-tags
" mostly obsolete now, smartgf is cooler than simple CTags
let g:vim_tags_directories = [".git"]
let g:vim_tags_auto_generate = 0
let g:vim_tags_use_vim_dispatch = 0
nnoremap <leader>tt g<C-]>
nnoremap <leader>tr <C-t>
nnoremap <leader>ty :tag<CR>

""" vim-ruby/vim-ruby
let g:ruby_indent_block_style = 'do'
let g:ruby_indent_assignment_style = 'variable'

"""""""""""""""""""""""""""""""""""""
""""""""""    Other things
"""""""""""""""""""""""""""""""""""""

" Because self is not python keyword stuff
syn keyword Keyword self

""" Other
cabbrev help tab help

" Create absent directories on edit
augroup BWCCreateDir
  autocmd!
  autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END

" Remove trailing spaces after save
au BufWritePre *.json,*.rb,*.py,*.c,*.h,*.feature,*.conf,*rc,README,CHANGELOG,README.* :%s/\s\+$//e

au BufNewFile,BufRead *.pl set filetype=prolog

" This allows for change paste motion cp{motion}
nmap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction

""""""""" DISABLED PLUGINS

" Superseded by vim-easyclip, because of easyclip having more features (and
" used anyway)
" Plug 'maxbrunsfeld/vim-yankstack'
""" maxbrunsfeld/vim-yankstack
" nmap [w <Plug>yankstack_substitute_older_paste
" nmap ]w <Plug>yankstack_substitute_newer_paste

" Superseded by neomake
" Plug 'scrooloose/syntastic'
""" scrooloose/syntastic
" let g:syntastic_ruby_checkers = ['rubocop']
" Alternative: 'mri'
" let g:syntastic_auto_jump = 3

" Plug 'rorymckinley/vim-symbols-strings'
""" rorymckinley/vim-symbols-strings
" let g:symbolise_strings_map_keys = 0
" nnoremap <silent> <Leader>sy :set opfunc=symbolsstrings#SymboliseStrings<CR>g@
" nnoremap <silent> <Leader>st :set opfunc=symbolsstrings#StringifySymbols<CR>g@p

" Vim-test has more features
""" gabebw/vim-spec-runner
" let g:spec_runner_dispatcher = 'call VtrSendCommand("be {command}")'
" map <localleader>sc <Plug>RunCurrentSpecFile
" map <localleader>sf <Plug>RunFocusedSpec
" map <localleader>sr <Plug>RunMostRecentSpec

""" reedes/vim-pencil
" augroup pencil
"   autocmd!
"   autocmd FileType markdown,mkd call pencil#init()
"   autocmd FileType text         call pencil#init()
" augroup END

" Not working
""" tacahiroy/ctrlp-funky
" nmap <Leader>pf :CtrlPFunky<CR>
" let g:ctrlp_funky_syntax_highlight = 1

" Not working
""" d11wtq/ctrlp_bdelete.vim
" let g:ctrlp_bdelete_map = '<c-_>'
" execute "nnoremap <buffer> <silent> ".g:ctrlp_bdelete_map." :call <sid>DeleteMarkedBuffers()<cr>"
" call ctrlp_bdelete#init()

""" garbas/vim-snipmate
" imap <C-J> <ESC>a<Plug>snipMateNextOrTrigger
" smap <C-J> <ESC>a<Plug>snipMateNextOrTrigger

""" jgdavey/vim-blockle
" let g:blockle_mapping = '<Leader>bl'

""" weynhamz/vim-plugin-minibufexpl
" nnoremap <leader>bd :MBEbd<CR>
" nnoremap <leader>bn :MBEbn<CR>
" nnoremap <leader>bp :MBEbp<CR>
" nnoremap <leader>bf :MBEbf<CR>
" nnoremap <leader>bb :MBEbb<CR>
" nnoremap <C-i> :MBEbn<CR>
" nnoremap <C-u> :MBEbp<CR>

if has('nvim')
"   if !($TMUX =~ ".*tmate.*")
"     let $NVIM_TUI_ENABLE_TRUE_COLOR=1
"     " let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
"   endif

"   let g:solarized_italic=0
endif

function! SetColorOnFocusLost()
  " if &background == 'dark'
  highlight Normal ctermbg=231
  " else
  " endif
endfunction

function! SetColorOnFocusGained()
  " if &background == 'dark'
  highlight Normal ctermbg=255
  " else
  " endif
endfunction

" au FocusLost * call SetColorOnFocusLost()
" au FocusGained * call SetColorOnFocusGained()

" Very filetype dependent

" Increase the limit of textwidth in git commit. 120 is enough for me to keep
" one line with "Issue: " and then some link to trello afterwards.
autocmd FileType gitcommit set textwidth=120


command! -range RubyReplaceLetWithLvar :<line1>,<line2>!~/.bin/ruby_replace_let_with_lvar.rb
