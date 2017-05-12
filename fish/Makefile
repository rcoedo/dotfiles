FISH_TARGET_DIR=~/.config/fish

install:
	cp $(FISH_TARGET_DIR)/config.fish $(FISH_TARGET_DIR)/config.fish.backup
	cp $(FISH_TARGET_DIR)/fishfile $(FISH_TARGET_DIR)/fishfile.backup
	cp -r $(FISH_TARGET_DIR)/hosts $(FISH_TARGET_DIR)/hosts.backup
	cp ./config.fish $(FISH_TARGET_DIR)/config.fish
	cp ./fishfile $(FISH_TARGET_DIR)/fishfile
	cp -r ./hosts $(FISH_TARGET_DIR)/hosts

get:
	cp $(FISH_TARGET_DIR)/config.fish ./config.fish
	cp $(FISH_TARGET_DIR)/fishfile ./fishfile
	cp -r $(FISH_TARGET_DIR)/hosts ./hosts
