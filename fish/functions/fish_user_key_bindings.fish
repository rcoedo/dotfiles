function fish_user_key_bindings
  bind \eb 'prevd; commandline -f repaint'
  bind \eu 'cd ..; commandline -f repaint'
  bind \cr '__fuzzy_history; commandline -f repaint'
  bind \eo '__fuzzy_ps; commandline -f repaint'
  bind \ep '__fuzzy_file; commandline -f repaint'
  bind \ec '__fuzzy_rcd; commandline -f repaint'
  bind \ew '__fuzzy_ghq; commandline -f repaint'
end
