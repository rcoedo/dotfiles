FISH_SOURCE=./config.org
FISH_TARGET=~/.config/fish/config.org

install:
	cp $(FISH_TARGET) $(FISH_TARGET).backup
	cp $(FISH_SOURCE) $(FISH_TARGET)
	orgtangle $(FISH_TARGET)

get:
	cp $(FISH_TARGET) $(FISH_SOURCE)
