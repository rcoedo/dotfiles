function ifind
    set dir $argv
    if test (count $argv) -gt 2 -o (count $argv) -lt 1
        set dir (pwd)
    end
    find $dir | peco | tr -d '\n' | read tempvar
    if [ $tempvar ]
        commandline -i $tempvar
    end
end

