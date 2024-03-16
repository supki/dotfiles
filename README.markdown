# ???

This is how I cope with the impeding doom of having to learn the `home-manager` tool.
I hope I will never have to do that, but deep down I know it's going to happen.

## Some small kludges I had to insert to make this actually work

### Beating up Zsh to conform to XDG crap.

Unfortunately, due to The Boostrap Problem™ I had to add the following:

```
export ZDOTDIR=$HOME/.config/zsh
```

to `/etc/zshenv.local`. Otherwise, `zsh` will try to read its configuration from `$HOME` directly
and I will have to mess around with symlinking to `$HOME/.config/zsh` myself.

### .ghci

It's still unaware of the XDG crap (at least at the GHC version I'm using), so I had to symlink
`~/.config/ghc/ghci.conf` to `~/.ghci`.

### Separate keyboard layouts for each window

For some reason this is not the default:

```
% gsettings set org.gnome.desktop.input-sources per-window true
```
