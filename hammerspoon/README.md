# Hammerspoon configuration

The [init.org](https://github.com/rcoedo/dotfiles/blob/master/hammerspoon/init.org) file serves as documentation and as source code to generate an `init.lua` file.

This configuration uses emac's `org-mode` to build the `init.lua` file.  
You can tangle the [init.org](https://github.com/rcoedo/dotfiles/blob/master/hammerspoon/init.org) using `org-babel-tangle-file` inside emacs
or you can use the [orgtangle](https://github.com/rcoedo/orgtangle) script.

## Installation
```
git clone git@github.com:rcoedo/dotfiles.git
cd dotfiles/
cp -r hammerspoon/ ~/.hammerspoon
cd ~/.hammerspoon
orgtangle init.org
```

![Hammerspoon rocks!](http://www.hammerspoon.org/images/hammerspoon.png)
