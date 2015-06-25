function h
    history | percol | read foo
    if [ $foo ]
        commandline $foo
    else
       commandline ''
    end
end
