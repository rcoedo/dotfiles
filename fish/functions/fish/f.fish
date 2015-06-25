function f
    set dir $argv
    if test (count $argv) -gt 2 -o (count $argv) -lt 1
        set dir (pwd)
    end
    # Could use `find $dir -type f` instead, need to work on a ignored dir file and load it.
    # ls -R $dir | awk '/:$/&&f{s=$0;f=0} /:$/&&!f{sub(/:$/,"");s=$0;f=1;next} NF&&f{ print s"/"$0 }' | peco | tr -d '\n'
    find $dir | peco | tr -d '\n'
end

