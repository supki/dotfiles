# ???

This is how I cope with the impeding doom of having to learn the `home-manager` tool.
I hope I will never have to do that, but deep down I know it's going to happen.

## Some small kludges I had to insert to make this actually work

### Beating up Zsh to conform to XDG crap.

Unfortunately, due to The Boostrap Problem™ I had to add the following:

```
export ZDOTDIR=$HOME/.config/zsh
```

to `/etc/zshenv`. Otherwise, `zsh` will try to read its configuration from `$HOME` directly
and I will have to mess around with symlinking to `$HOME/.config/zsh` myself.

### Setting up default shell

Some nix-free interaction with Ubuntu was obviously required to change the default shell:

```
$ sudo chsh -s /home/m/.nix-profile/bin/zsh m
```

Unsurprisingly, Ubuntu is still shit in 2022, so that broke the login screen and my user is nowhere to
be found there now. How they are able to produce such a awful user experience after 10+ years of trying
to make something usable is still a question I have no answer for.

After some additional research, the culprit was identified. I also had to add `/home/m/.nix-profile/bin/zsh`
to the valid login shells listed in `/etc/shells`. This file is consulted when Ubuntu tries to determine
whether a user is a "normal" one.

### Launching `alacritty`

Since it's imperative that terminal emulators should use GPU in 2022, I had to
create a `x-terminal-emulator` wrapper for Ubuntu to pick up the `alacritty` executable.
This wrapper can be found in this repository as `kludges/alacritty.wrapper`. (Hopefully, no one will
name their tool `kludges` in the future.)  
It's in Perl because the original `gnome-terminal.wrapper` that I butchered was written in Perl.
I don't particularly like the language.

For Ubuntu to actually pick up the new wrapper, I had to run

```
% sudo update-alternatives --install /usr/bin/x-terminal-emulator x-terminal-emulator /usr/bin/alacritty.wrapper
```

### Custom fonts

This one is pretty easy: just need to add `<dir>/home/m/.nix-profile/share/fonts</dir>` after
`<!-- Font directory list -->` in `/etc/fonts/fonts.conf` and then run `% fc-cache -fv`. Probably makes more
sense to muck around with `~/.local/fonts` or whatever than polluting system-wide configuration, but
I just didn't feel like it.

### .ghci

It's still unaware of the XDG crap (at least at the GHC version I'm using), so I had to symlink
`~/.config/ghc/ghci.conf` to `~/.ghci`.

### pwfeedback

To see the feedback when typing sudo passwords, just add

```
Defaults pwfeedback
```

to `/etc/sudoers`. Hopefully, there's no more exploits.

### Solid background

To save my eyes from the default Ubuntu background:

```
% gsettings set org.gnome.desktop.background picture-options 'none'
% gsettings set org.gnome.desktop.background primary-color '#222222'
```

### Separate keyboard layouts for each window

For some reason this is not the default:

```
% gsettings set org.gnome.desktop.input-sources per-window true
```

### Remove ads from my terminal

```
% sudo pro config set apt_news=false
```

### Set up `bookworm`

This is pretty basic. First, it's necessary to create a `.desktop` entry in `~/.local/share/applications` by copying `./kludges/bookworm.desktop` there. Then, just drop an icon from `~/.nix-profile/share/icons/hicolor/128x128/apps/com.github.babluboy.bookworm.svg` to `~/.local/share/icons/bookworm.svg`
