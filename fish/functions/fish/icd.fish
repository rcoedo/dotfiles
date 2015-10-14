function icd
    tree --noreport -d -i -L 1 $CDPATH | grep -v '\(Users\|\.\)' | peco | read tempvar
    if [ $tempvar ]
        cd $tempvar
        commandline -f repaint
    end
end

