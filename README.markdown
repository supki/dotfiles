.dotfiles
=========
[![Build Status](https://secure.travis-ci.org/supki/.dotfiles.png?branch=master)](https://travis-ci.org/supki/.dotfiles)

Hello! This is my dotfiles repository.  
If you *really* want to know what's going on here, here's [a link][biegunka]

---

In case of memory loss
----------------------

Install [Nix](https://nixos.org/nix):

```
% curl https://nixos.org/nix/install | sh
% source $HOME/.nix-profile/etc/profile.d/nix.sh
```

Prepare the environment:

```
% nix-shell
```

Run biegunka to populate `~` with dotfiles:

```
[nix-shell]$ biegunka run biegunka -- [--work|--laptop]
```

 [biegunka]: http://biegunka.budueba.com/
