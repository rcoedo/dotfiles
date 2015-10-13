function icd
    ls -d */ | peco | read tempvar
    if [ $tempvar ]
        cd $tempvar
    end
end

