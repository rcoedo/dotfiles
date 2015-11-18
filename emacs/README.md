# Emacs configuration (Evil)

The [init.org](https://github.com/rcoedo/dotfiles/blob/master/emacs/init.org) file serves both as a cheatsheet and as source code to generate an `init.el` file.

This configuration uses `Cask` as package manager and `org` to build the `init.el` file.  
You can tangle the [init.org](https://github.com/rcoedo/dotfiles/blob/master/emacs/init.org) using `org-mode` inside emacs
or you can use the [orgtangle](https://github.com/rcoedo/orgtangle) script.

## Installation
```
git clone git@github.com:rcoedo/dotfiles.git
cd dotfiles/
cp -r emacs/ ~/.emacs.d/
cd ~/.emacs.d
orgtangle init.org
cask install
```

![They love each other :)](http://i.imgur.com/dObnwrW.jpg)
