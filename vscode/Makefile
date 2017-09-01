VSCODE_SOURCE_DIR=.
VSCODE_TARGET_DIR=~/Library/Application\ Support/Code/User

install:
	rm -rf $(VSCODE_TARGET_DIR).backup
	mv $(VSCODE_TARGET_DIR) $(VSCODE_TARGET_DIR).backup
	rsync -av --exclude=Makefile $(VSCODE_SOURCE_DIR) $(VSCODE_TARGET_DIR)

get:
	cd $(VSCODE_TARGET_DIR)
	cp $(VSCODE_TARGET_DIR)/keybindings.json $(VSCODE_SOURCE_DIR)/
	cp $(VSCODE_TARGET_DIR)/settings.json $(VSCODE_SOURCE_DIR)/
	rm -rf $(VSCODE_SOURCE_DIR)/snippets
	cp -r $(VSCODE_TARGET_DIR)/snippets $(VSCODE_SOURCE_DIR)/snippets
