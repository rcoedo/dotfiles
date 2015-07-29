function ips
    ps -fea | awk '{$1=$3=$4=$5=$6=$7=""; print $0}' | peco | awk '{print $1}' | read tempvar
    if [ $tempvar ]
        commandline -i $tempvar
    end
end

