function er-docker-essentials-start
  docker-compose -f $EVERREAL_ROOT/er-scripts/docker-compose/docker-compose-essential.yml --project-name=essentials up -d
end

