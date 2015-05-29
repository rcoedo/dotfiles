function ranger-cd
    set tempfile '/tmp/ranger-cd'
    ranger --choosedir=$tempfile (pwd)

    if test -f $tempfile
        if test (cat $tempfile) != (pwd)
            cd (cat $tempfile)
        end
    end
    rm -f $tempfile
end
