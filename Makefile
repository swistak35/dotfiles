install: install-vim install-bin install-other install-nvim install-xfce

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.config/nvim/init.vim ~/.vimrc
	vim +PlugInstall +qall
	# [ -d ~/.eclim ] && ln -s ~/.eclim ~/.vim/eclim

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
	rm -f ~/.config/redshift.conf
	ln -s `pwd`/other/redshift.conf ~/.config/redshift.conf


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
