# ???

This is how I cope with the impeding doom of having to learn the `home-manager` tool.
I hope I will never have to do that, but deep down I know it's going to happen.

## Some small kludges I had to insert to make this actually work

  1. Unfortunately, due to The Boostrap Problem™ I had to add the following:

     ```
     export ZDOTDIR=$HOME/.config/zsh
     ```

     to `/etc/zshenv`. Otherwise, `zsh` will try to read its configuration from `$HOME` directly
     and I will have to mess around with symlinking to `$HOME/.config/zsh` myself.


  2. Some nix-free interaction with Ubuntu was obviously required to change the default shell:

     ```
     sudo chsh -s /home/m/.nix-profile/bin/zsh m
     ```

     Unsurprisingly, Ubuntu is still shit in 2022, so that broke the login screen and my user is nowhere to
     be found there now. How they are able to produce such a awful user experience after 10+ years of trying
     to make something usable is still a question I have no answer for.
