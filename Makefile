install: install-vim install-zsh install-git install-bin install-other install-nvim

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.vim/vimrc ~/.vimrc
	vim +PlugInstall +qall
	# [ -d ~/.eclim ] && ln -s ~/.eclim ~/.vim/eclim

install-zsh:
	rm -rf ~/.zsh ~/.zshrc ~/.oh-my-zsh
	ln -s `pwd`/zsh ~/.zsh
	ln -s ~/.zsh/zshrc ~/.zshrc
	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh

install-git:
	rm -f ~/.global_gitignore ~/.gitconfig
	ln -s `pwd`/git/global_gitignore ~/.global_gitignore
	ln -s `pwd`/git/gitconfig ~/.gitconfig

install-bin:
	rm -rf ~/.bin
	ln -s `pwd`/bin ~/.bin

install-other:
	rm -f ~/.ghci
	ln -s `pwd`/other/ghci ~/.ghci
	rm -f ~/.ocamlinit
	ln -s `pwd`/other/ocamlinit ~/.ocamlinit
	rm -f ~/.tmux.conf
	ln -s `pwd`/other/tmux.conf ~/.tmux.conf
	rm -f ~/.gemrc
	ln -s `pwd`/other/gemrc ~/.gemrc

install-nvim:
	rm -rf ~/.nvimrc ~/.nvim
	ln -s `pwd`/nvim ~/.nvim
	ln -s ~/.nvim/nvimrc ~/.nvimrc
	nvim +PlugInstall +qall

install-vifm:
	rm -rf ~/.vifm
	ln -s `pwd`/vifm ~/.vifm

install-vimperator:
	rm -rf ~/.vimperator ~/.vimperatorrc
	ln -s `pwd`/vimperator ~/.vimperator
	ln -s ~/.vimperator/vimperatorrc ~/.vimperatorrc
