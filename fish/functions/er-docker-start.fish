function er-docker-start
  docker-compose -f $EVERREAL_ROOT/er-scripts/docker-compose/docker-compose-essential.yml --project-name=essentials up -d
  sleep 3
  docker-compose -f $EVERREAL_ROOT/er-scripts/docker-compose/docker-compose-main.yml --project-name=microservices $argv[1] $argv[2] $argv[3]
end

