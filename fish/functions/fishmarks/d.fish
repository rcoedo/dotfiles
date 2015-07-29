function d
    list_bookmarks | tr -s " " | awk '{ print $1 }' | peco | read -l tempvar
    if test (count $tempvar) -gt 0
        delete_bookmark (echo $tempvar | cut -c 8-)
    end
end

