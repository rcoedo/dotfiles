function ighq
    ghq list | peco | read tempvar
    if test (count $tempvar) -gt 0
        cd (ghq root)/$tempvar
        commandline -f repaint
    end
end

