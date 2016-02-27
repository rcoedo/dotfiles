ATOM_SOURCE_DIR=.
ATOM_TARGET_DIR=~/.atom

install:
	rm -rf $(ATOM_TARGET_DIR).backup
	mv $(ATOM_TARGET_DIR) $(ATOM_TARGET_DIR).backup
	rsync -av --exclude=Makefile $(ATOM_SOURCE_DIR) $(ATOM_TARGET_DIR)
	mv $(ATOM_TARGET_DIR)/Makefile.atom $(ATOM_TARGET_DIR)/Makefile
	cd $(ATOM_TARGET_DIR) && make install-packages

get:
	cd $(ATOM_TARGET_DIR) && make generate-package-list
	cp $(ATOM_TARGET_DIR)/config.cson   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/init.coffee   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/keymap.cson   $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/Makefile      $(ATOM_SOURCE_DIR)/Makefile.atom
	cp $(ATOM_TARGET_DIR)/packages.txt  $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/snippets.cson $(ATOM_SOURCE_DIR)/
	cp $(ATOM_TARGET_DIR)/styles.less   $(ATOM_SOURCE_DIR)/
