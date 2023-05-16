function __project_run -a interactive = false
    set command ""

    if test -e "package.json"
        jq '.packageManager' package.json \
            | tr -d '"' \
            | cut -d@ -f1 \
            | read result

        if [ $result = null -o $result = npm ]
            set command "npm run"
        else if [ $result = pnpm -o $result = yarn ]
            set command $result
        end
    else if test -e "deno.json" -o -e "deno.jsonc"
        set command "deno task"
    end

    if test -z "$command"
        return 1
    end

    echo $command
end
