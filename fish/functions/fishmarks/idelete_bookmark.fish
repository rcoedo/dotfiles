function idelete_bookmark
    list_bookmarks | tr -s " " | awk '{ print $1 }' | peco | tr "\n" " " | read -l tempvar
    if [ $tempvar ]
        delete_bookmark (echo $tempvar | tr " " "\n" | cut -c 8-)
    end
end

