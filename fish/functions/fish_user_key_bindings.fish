function fish_user_key_bindings
  bind \eb 'prevd; commandline -f repaint'
  bind \eu 'cd ..; commandline -f repaint'
  bind \ei '__fuzzy_package_json_script'
  bind \em '__fuzzy_git_commit'
  bind \cr '__fuzzy_history'
  bind \eo '__fuzzy_lsof'
  bind \ep '__fuzzy_ps'
  bind \ec '__fuzzy_rcd'
  bind \ew '__fuzzy_ghq'
end
