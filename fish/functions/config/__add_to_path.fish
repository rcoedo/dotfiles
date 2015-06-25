function __add_to_path
    if test (count $argv) = 1
        set -gx PATH $argv[1] $PATH
    else
        echo "Usage: __add_to_path <path>"
    end
end
