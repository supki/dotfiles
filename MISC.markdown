# Miscellaneous tweaks/tricks

(Very) rarely useful bits of knowledge that I will certainly forget
unless I write them down. (Probably even then.)

## SSH

### Using `Match` and `ProxyCommand` to connect to servers unavailable on public network

This will connect to all "hidden" domains under `exmaple.com` using
the proxy at `proxy.example.com`:

```
Match Host *.example.com Exec "[[ -z $(dig +short %h) ]]"
  ProxyCommand ssh -q -W %h:%p proxy.example.com
```

## nvim-treesitter

Strange people at https://github.com/nvim-treesitter/nvim-treesitter/pull/3066
have decided to fuck Haskell syntax highlighting up for no reason. To fix that, fish for
`/nix/store/*-nvim-treesitter-*/queries/haskell/highlights.scm` and remove/comment the "exp_infix" lines. I should probably set up a proper Nix patch thingy at some point.

## Random Windows-related crap

### Easy 30 steps to get non-enraging editing experience in PowerShell

0. Press Ctrl-P to get a nice looking `^P` typed in your terminal one last time.
1. Run `$profile` to get the required location for the PowerShell's `.profile`-like.
2. Run `mkdir DIRECTORY_OF_THAT_FILE`, because it is not there by default (obviously, why would it be there?)
3. Run `notepad $profile` and paste `Set-PSReadLineOption -EditMode Emacs` into it; save and exit
4. Start another instance of PowerShell as Administrator and run one more spell: `Set-ExecutionPolicy -Scope CurrentUser unrestricted`
5. Should be fine now if you restart the console.
