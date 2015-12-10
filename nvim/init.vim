call plug#begin('~/.nvim/plugged')
exec 'source ' . $HOME . '/.vimrc_plugins'
call plug#end()

" Don't use Ex mode, use Q for formatting
" map Q g

exec 'source ' . $HOME . '/.vimrc_pluginrc'

if has('nvim')
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

  let g:solarized_italic=0
endif

au VimEnter * colorscheme solarized
