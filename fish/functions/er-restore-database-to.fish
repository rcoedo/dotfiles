function er-restore-database-to
    set FILE_PATH $argv[1]
    set TO_SERVER $argv[2]
    set NEW_DBNAME $argv[3]

    docker cp $FILE_PATH postgres:/home/backup.sql
    echo " ==> Start importing database"
    docker exec postgres psql $TO_SERVER -c "drop database \"$NEW_DBNAME\";"
    docker exec postgres psql $TO_SERVER -c "create database \"$NEW_DBNAME\";"
    docker exec postgres psql -d $TO_SERVER/$NEW_DBNAME -f /home/backup.sql
    echo " ==> Import database finished"
end

