function ihistory
    history | peco | read tempvar
    if [ $tempvar ]
        commandline $tempvar
    end
end
