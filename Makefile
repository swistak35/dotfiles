install: install-vim install-zsh install-git install-bin install-other install-nvim install-xfce

install-emacs:
	rm -rf ~/.emacs.d
	ln -s `pwd`/emacs.d ~/.emacs.d

install-vim:
	rm -rf ~/.vim ~/.vimrc
	ln -s `pwd`/vim ~/.vim
	ln -s ~/.config/nvim/init.vim ~/.vimrc
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
	ln -s `pwd`/git/global_gitattributes ~/.global_gitattributes
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
	rm -f ~/.Xmodmap
	ln -s `pwd`/other/Xmodmap ~/.Xmodmap
	rm -f ~/.taskrc
	ln -s `pwd`/other/taskrc ~/.taskrc
	rm -f ~/.irbrc
	ln -s `pwd`/other/irbrc ~/.irbrc
	rm -f ~/.config/autostart-scripts/ssh-add.sh
	ln -s `pwd`/other/ssh-add.sh ~/.config/autostart-scripts/ssh-add.sh
	rm -f ~/.config/redshift.conf
	ln -s `pwd`/other/redshift.conf ~/.config/redshift.conf
	rm -f ~/.rspec
	ln -s `pwd`/other/rspec ~/.rspec

install-xfce:
	rm ~/.config/Thunar/uca.xml
	ln -s `pwd`/thunar/uca.xml ~/.config/Thunar/uca.xml
	rm ~/.config/Thunar/renamerrc
	ln -s `pwd`/thunar/renamerrc ~/.config/Thunar/renamerrc

install-nvim:
	rm -rf ~/.nvimrc ~/.nvim ~/.config/nvim
	ln -s `pwd`/nvim ~/.config/nvim
	nvim +PlugInstall +qall

install-vifm:
	rm -rf ~/.vifm
	ln -s `pwd`/vifm ~/.vifm

install-vimperator:
	rm -rf ~/.vimperator ~/.vimperatorrc
	ln -s `pwd`/vimperator ~/.vimperator
	ln -s ~/.vimperator/vimperatorrc ~/.vimperatorrc

install-awesome:
	rm -rf ~/.config/awesome
	ln -s `pwd`/awesome ~/.config/awesome

install-weechat:
	rm -rf ~/.weechat
	[ -d "$$HOME/.weechat-logs" ] || mkdir ~/.weechat-logs
	ln -s `pwd`/weechat ~/.weechat
	[ -e "$$HOME/.weechat/logs" ] || ln -s ~/.weechat-logs ~/.weechat/logs
	[ -f "$$HOME/.weechat/sec.conf" ] || cp ~/.weechat/sec.conf.example ~/.weechat/sec.conf

install-dropbox:
	ln -s /media/magazyn/Dropbox/Dokumenty ~/Dokumenty
	ln -s /media/magazyn/Dropbox/Obrazy ~/Obrazy

install-qnapi:
	mkdir -p ~/.kde/share/apps/dolphin/servicemenus/
	rm -f ~/.kde/share/apps/dolphin/servicemenus/qnapi-download.desktop
	ln -s /usr/share/doc/qnapi/qnapi-download.desktop ~/.kde/share/apps/dolphin/servicemenus/qnapi-download.desktop
	mkdir -p ~/.kde/share/apps/d3lphin/servicemenus/
	rm -f ~/.kde/share/apps/d3lphin/servicemenus/qnapi-download.desktop
	ln -s /usr/share/doc/qnapi/qnapi-download.desktop ~/.kde/share/apps/d3lphin/servicemenus/qnapi-download.desktop
	mkdir -p ~/.kde/share/apps/konqueror/servicemenus/
	rm -f ~/.kde/share/apps/konqueror/servicemenus/qnapi-download.desktop
	ln -s /usr/share/doc/qnapi/qnapi-download.desktop ~/.kde/share/apps/konqueror/servicemenus/qnapi-download.desktop
