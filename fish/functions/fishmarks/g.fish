function g
    set -l temp /tmp/temp_bookmark.result
    list_bookmarks |  tr -s " " | awk '{ print $1 }' | peco | perl -pe 's/\x1b.*?[mGKH]//g' > $temp

    set -l bookmark (cat $temp)
    if test (count $bookmark) -gt 0
        cd (print_bookmark $bookmark)
    end
end

