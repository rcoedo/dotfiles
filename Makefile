IDEAVIM_SOURCE=ideavim/ideavimrc
IDEAVIM_TARGET=~/.config/nvim/init.vim

install-nvim:
	cp $(NVIM_TARGET) $(NVIM_TARGET).backup
	cp $(NVIM_SOURCE) $(NVIM_TARGET)

get-nvim:
	cp $(NVIM_SOURCE) $(NVIM_TARGET)
