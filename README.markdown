# ???

This is how I cope with the impeding doom of having to learn the `home-manager` tool.
I hope I will never have to do that, but deep down I know it's going to happen.

## Some small kludges I had to insert to make this actually work

### .ghci

It's still unaware of the XDG crap (at least at the GHC version I'm using), so I had to symlink
`~/.config/ghc/ghci.conf` to `~/.ghci`.
