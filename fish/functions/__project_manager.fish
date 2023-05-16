function __project_manager
    set command ""

    if test -e "package.json"
        jq '.packageManager' package.json \
            | tr -d '"' \
            | cut -d@ -f1 \
            | read -l package_manager

        if [ $package_manager = null ]
            set command npm
        else
            set command $package_manager
        end
    end

    if test -e "deno.jsonc" -o -e "deno.json"
        set command deno
    end

    if test -z "$command"
        return 1
    end

    echo $command
end
