function igo_bookmark
    list_bookmarks |  tr -s " " | awk '{ print $1 }' | peco | perl -pe 's/\x1b.*?[mGKH]//g' | read -l tempvar
    if test (count $tempvar) -gt 0
        cd (print_bookmark $tempvar)
    end
end

