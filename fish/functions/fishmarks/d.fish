function d
    set -l temp /tmp/temp_bookmark.result
    list_bookmarks | tr -s " " | awk '{ print $1 }' | peco > $temp
    set -l bookmark (cat $temp)

    if test (count $bookmark) -gt 0
        delete_bookmark (echo $bookmark | cut -c 8-)
    end
end

