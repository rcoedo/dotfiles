install:
	cd atom && make install
	cd emacs && make install
	cd fish && make install
	cd git && make install
	cd hammerspoon && make install
	cd ideavim && make install
	cd nvim && make install
	cd tmux	&& make install

get:
	cd atom && make get
	cd emacs && make get
	cd fish && make get
	cd git && make get
	cd hammerspoon && make get
	cd ideavim && make get
	cd nvim && make get
	cd tmux	&& make get
