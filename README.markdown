# ???

~~This is how I cope with the impeding doom of having to learn the `home-manager` tool.~~

I actually think I'm happy enough with the setup and won't switch to `home-manager` in the future.

To use this, clone the repository to `~/.config` and run `nix-rebuild-env`. That may require adding `export ZDOTDIR=$HOME/.config/zsh` to `/etc/nixos/configuration.nix`, which is a good idea anyway.

## Some small kludges I had to insert to make this actually work

### .ghci

~~GHC is unaware of the XDG crap~~ Apparently it *is* aware, but GHC on NixOS is set up in a weird way and thinks its `appdir` (whatever that is; it's only mentioned once in the user guide) is in `~/.ghc` and not `~/.config/ghc`, so symlinking is unfortunately necessary:

```
% ln -s ~/.config/ghc/ghci.conf ~/.ghc/ghci.conf
```
