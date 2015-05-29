function __user_host
  set -l content 
  echo -n (set_color --bold yellow)
  echo -n $USER@(hostname|cut -d . -f 1) (set color normal)
end

function __prompt_pwd
  echo -n ' '(set_color green)(pwd)(set_color normal)
end

function __rb_prompt
  echo -n (set_color red)'<'(rbenv version | awk '{print $1}')'>'(set_color normal)
end

set __fish_git_prompt_color 'magenta'
set __fish_git_prompt_show_informative_status 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_char_stateseparator ' '

set __fish_git_prompt_char_stagedstate '+'
set __fish_git_prompt_color_stagedstate 'green'

set __fish_git_prompt_char_dirtystate '*'
set __fish_git_prompt_color_dirtystate 'red'

set __fish_git_prompt_color_cleanstate 'green'

function fish_prompt
  echo -e ''
  __user_host
  __prompt_pwd
  __fish_git_prompt " "
  echo -e ''
  echo (set_color white)"> "(set_color normal)
end

function fish_right_prompt
  __rb_prompt
  set -l st $status
  if [ $st != 0 ];
    echo (set_color red) ↵ $st(set_color normal)
  end
end
