install: install-vim install-zsh install-git install-bin

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.vim/vimrc ~/.vimrc
	git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall
	[ -d ~/.eclim ] && ln -s ~/.eclim ~/.vim/eclim

install-zsh:
	rm -f ~/.zsh ~/.zshrc 
	ln -s `pwd`/zsh ~/.zsh
	ln -s ~/.zsh/zshrc ~/.zshrc
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

install-git:
	rm -f ~/.global_gitignore ~/.gitconfig
	ln -s `pwd`/git/global_gitignore ~/.global_gitignore
	ln -s `pwd`/git/gitconfig ~/.gitconfig

install-bin:
	rm -f ~/.bin
	ln -s `pwd`/bin ~/.bin

install-other:
	rm -f ~/.ghci
	ln -s `pwd`/other/ghci ~/.ghci
	rm -f ~/.ocamlinit
	ln -s `pwd`/other/ocamlinit ~/.ocamlinit
