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
	rm -f ~/.config/autostart-scripts/ssh-add.sh
	mkdir -p ~/.config/autostart-scripts
	ln -s `pwd`/other/ssh-add.sh ~/.config/autostart-scripts/ssh-add.sh
	rm -f ~/.config/redshift.conf
	ln -s `pwd`/other/redshift.conf ~/.config/redshift.conf

install-xfce:
	rm ~/.config/Thunar/uca.xml
	ln -s `pwd`/thunar/uca.xml ~/.config/Thunar/uca.xml
	rm ~/.config/Thunar/renamerrc
	ln -s `pwd`/thunar/renamerrc ~/.config/Thunar/renamerrc

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
