EMACS_TARGET_DIR=~/.emacs.d
EMACS_SOURCE_DIR=./emacs.d/

install:
	rsync -av $(EMACS_SOURCE_DIR) $(EMACS_TARGET_DIR)

get:
	rsync -av $(EMACS_TARGET_DIR) $(EMACS_SOURCE_DIR)
