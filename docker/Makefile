DOCKER_SOURCE=./config.json
DOCKER_TARGET=~/.docker/config.json

install:
	cp $(DOCKER_TARGET) $(DOCKER_TARGET).backup
	cp $(DOCKER_SOURCE) $(DOCKER_TARGET)

get:
	cp $(DOCKER_TARGET) $(DOCKER_SOURCE)
