function __add_to_path
    if test (count $argv) = 1
        set -gx PATH $PATH $argv[1]
    else
        echo "Usage: __add_to_path <path>"
    end
end
