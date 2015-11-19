# Fish shell configuration

The [init.org](https://github.com/rcoedo/dotfiles/blob/master/fish/config.org) file serves both as a cheatsheet and as source code to generate a `config.fish` file.

This configuration uses emacs' `org-mode` to build the `config.fish` file.  
You can tangle the [config.org](https://github.com/rcoedo/dotfiles/blob/master/fish/config.org) using `org-babel-tangle-file` inside emacs
or you can use the [orgtangle](https://github.com/rcoedo/orgtangle) script.

## Installation
```
git clone git@github.com:rcoedo/dotfiles.git
cd dotfiles/
cp -r fish/ ~/.config/fish/
cd ~/.config/fish/
orgtangle config.org
```

![Fish rocks!](http://fishshell.com/docs/current/ascii_fish.png)