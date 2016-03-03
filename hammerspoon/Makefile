HS_SOURCE_DIR=.
HS_TARGET_DIR=~/.hammerspoon

install:
	rm -rf $(HS_TARGET_DIR).backup
	cp -rf $(HS_TARGET_DIR) $(HS_TARGET_DIR).backup
	rsync -av --exclude=Makefile $(HS_SOURCE_DIR) $(HS_TARGET_DIR)

get:
	rsync -av $(HS_TARGET_DIR)/ $(HS_SOURCE_DIR)/
