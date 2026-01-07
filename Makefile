install: install-vim install-bin install-other install-nvim install-xfce

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.config/nvim/init.vim ~/.vimrc
	vim +PlugInstall +qall
	# [ -d ~/.eclim ] && ln -s ~/.eclim ~/.vim/eclim

install-nvim:
	rm -rf ~/.config/nvim
	ln -s `pwd`/nvim ~/.config/nvim
	mkdir -p ~/.cache/nvim/swp
	mkdir -p ~/.cache/nvim/tmp
	mkdir -p ~/.cache/nvim/undo
	nvim +PlugInstall +qall

install-bin:
	rm -rf ~/.bin
	ln -s `pwd`/bin ~/.bin

install-other:
	rm -f ~/.ghci
	ln -s `pwd`/other/ghci ~/.ghci
	rm -f ~/.ocamlinit
	ln -s `pwd`/other/ocamlinit ~/.ocamlinit
	rm -f ~/.Xmodmap
	ln -s `pwd`/other/Xmodmap ~/.Xmodmap
	rm -f ~/.taskrc
	ln -s `pwd`/other/taskrc ~/.taskrc

install-git:
	rm -f ~/.gitconfig
	ln -s `pwd`/git/gitconfig ~/.gitconfig
	rm -f ~/.global_gitattributes
	ln -s `pwd`/git/global_gitattributes ~/.global_gitattributes
	rm -f ~/.global_gitignore
	ln -s `pwd`/git/global_gitignore ~/.global_gitignore

install-vifm:
	rm -rf ~/.vifm
	ln -s `pwd`/vifm ~/.vifm

install-vimperator:
	rm -rf ~/.vimperator ~/.vimperatorrc
	ln -s `pwd`/vimperator ~/.vimperator
	ln -s ~/.vimperator/vimperatorrc ~/.vimperatorrc

install-weechat:
	rm -rf ~/.weechat
	[ -d "$$HOME/.weechat-logs" ] || mkdir ~/.weechat-logs
	ln -s `pwd`/weechat ~/.weechat
	[ -e "$$HOME/.weechat/logs" ] || ln -s ~/.weechat-logs ~/.weechat/logs
	[ -f "$$HOME/.weechat/sec.conf" ] || cp ~/.weechat/sec.conf.example ~/.weechat/sec.conf
