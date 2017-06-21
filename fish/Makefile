FISH_TARGET_DIR=~/.config/fish
FISH_SOURCE_DIR=.

install:
	rsync -av --exclude=Makefile $(FISH_SOURCE_DIR) $(FISH_TARGET_DIR)

get:
	cp $(FISH_TARGET_DIR)/config.fish ./config.fish
	cp $(FISH_TARGET_DIR)/fishfile ./fishfile
	rsync -av $(FISH_TARGET_DIR)/hosts/ $(FISH_SOURCE_DIR)/hosts/
