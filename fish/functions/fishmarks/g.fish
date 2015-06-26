function g
    set -l temp /tmp/temp_bookmark.result
    list_bookmarks |  tr -s " " | awk '{ print $1 }' | peco > $temp
    set -l bookmark (cat $temp)

    if test (count $bookmark) -gt 0
        cd (list_bookmarks | grep -F $bookmark | tr -s " " | awk '{ print $3 }')
        #cd (print_bookmark $bookmark)
    end
end

