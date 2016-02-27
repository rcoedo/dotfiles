EMACS_SOURCE_DIR=.
EMACS_TARGET_DIR=~/.emacs.d

install:
	# Backup
	cp $(EMACS_TARGET_DIR)/Cask $(EMACS_TARGET_DIR)/Cask.backup
	cp $(EMACS_TARGET_DIR)/init.org $(EMACS_TARGET_DIR)/init.org.backup
	cp $(EMACS_TARGET_DIR)/eshell-aliases $(EMACS_TARGET_DIR)/eshell-aliases.backup
	# Copy files
	cp $(EMACS_SOURCE_DIR)/Cask $(EMACS_TARGET_DIR)/Cask
	cp $(EMACS_SOURCE_DIR)/init.org $(EMACS_TARGET_DIR)/init.org
	cp $(EMACS_SOURCE_DIR)/eshell-aliases $(EMACS_TARGET_DIR)/eshell-aliases
	# Tangle the init.org file
	orgtangle $(EMACS_TARGET_DIR)/init.org

get:
	cp $(EMACS_TARGET_DIR)/Cask $(EMACS_SOURCE_DIR)/Cask
	cp $(EMACS_TARGET_DIR)/init.org $(EMACS_SOURCE_DIR)/init.org
