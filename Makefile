IDEAVIM_SOURCE=ideavim/ideavimrc
IDEAVIM_TARGET=~/.config/nvim/init.vim

NVIM_SOURCE=nvim/init.vim
NVIM_TARGET=~/.config/nvim/init.vim

ATOM_SOURCE_DIR=atom
ATOM_TARGET_DIR=~/.atom

install-nvim:
	cp $(NVIM_TARGET) $(NVIM_TARGET).backup
	cp $(NVIM_SOURCE) $(NVIM_TARGET)

get-ideavim:
	cp $(NVIM_SOURCE) $(NVIM_TARGET)

install-nvim:
	cp $(NVIM_TARGET) $(NVIM_TARGET).backup
	cp $(NVIM_SOURCE) $(NVIM_TARGET)

get-nvim:
	cp $(NVIM_SOURCE) $(NVIM_TARGET)

install-atom:
	cp -r $(ATOM_TARGET_DIR) $(ATOM_TARGET_DIR).backup
	cp -r $(ATOM_SOURCE_DIR) $(ATOM_TARGET_DIR)
	cd $(ATOM_TARGET_DIR) && make install-packages

get-atom:
	cd $(ATOM_TARGET_DIR) && make generate-package-list
	cp $(ATOM_TARGET_DIR)/config.cson   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/init.coffee   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/keymap.cson   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/Makefile      $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/packages.txt  $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/snippets.cson $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/styles.less   $(ATOM_SOURCE_DIR)/
