function igitfile
    git status --short --porcelain | cut -d' ' -f3 | peco | tr -d '\n' | read tempvar
    if [ $tempvar ]
        commandline -i $tempvar
    end
end

