EMACS_SOURCE_DIR=.
EMACS_TARGET_DIR=~/.emacs.d

install:
	# Backup
	cp $(EMACS_TARGET_DIR)/Cask $(EMACS_TARGET_DIR)/Cask.backup
	cp $(EMACS_TARGET_DIR)/init.org $(EMACS_TARGET_DIR)/init.org.backup
	cp $(EMACS_TARGET_DIR)/eshell-aliases $(EMACS_TARGET_DIR)/eshell-aliases.backup
	rm -rf $(EMACS_TARGET_DIR)/snippets.backup
	mv $(EMACS_TARGET_DIR)/snippets $(EMACS_TARGET_DIR)/snippets.backup
	# Copy files
	cp $(EMACS_SOURCE_DIR)/Cask $(EMACS_TARGET_DIR)/Cask
	cp $(EMACS_SOURCE_DIR)/init.el $(EMACS_TARGET_DIR)/init.el
	cp $(EMACS_SOURCE_DIR)/eshell-aliases $(EMACS_TARGET_DIR)/eshell-aliases
	cp -r $(EMACS_SOURCE_DIR)/snippets $(EMACS_TARGET_DIR)/snippets

get:
	cp $(EMACS_TARGET_DIR)/Cask $(EMACS_SOURCE_DIR)/Cask
	cp $(EMACS_TARGET_DIR)/init.el $(EMACS_SOURCE_DIR)/init.el
