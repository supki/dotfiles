{ lib, pkgs, ... }:
let
  buildZshPlugin = { name, src }:
    pkgs.stdenv.mkDerivation {
      inherit name src;

      installPhase = ''
        runHook preInstall
        target=$out/share/zsh/${name}
        mkdir -p $target
        cp -r . $target
        runHook postInstall
      '';
    };
  overridenHaskellPackages = pkgs.haskellPackages.override (_: {
    overrides = pkgs.haskell.lib.packageSourceOverrides {
      t = pkgs.fetchFromGitHub {
        owner = "supki";
        repo = "t";
        rev = "main";
        sha256 = "sha256-fAY6dtLcV2WNX0+W89t+f//AqLeCspJaPELgRjcHk4E=";
      };
      dazu = pkgs.fetchFromGitHub {
        owner = "supki";
        repo = "da";
        rev = "main";
        sha256 = "sha256-VOEPXnAg5cPaKyxtf/plNbBJuVfoqptuPGRvZjmxsuk=";
      };
      relocant = pkgs.fetchFromGitHub {
        owner = "supki";
        repo = "relocant";
        rev = "main";
        sha256 = "sha256-CrYxUotiVZwRncbquxTE2Cm1uIsvxow+tJVWkUYKyTQ=";
      };
    };
  });
  pristine-pkgs = with pkgs; [
    alacritty
    diff-so-fancy
    difftastic
    dig
    eza
    feh
    fira-code
    firefox
    fzf
    gnome-tweaks
    htop
    inetutils
    iotop
    jq
    moreutils
    mpv
    nmap
    openssl
    pass
    scrot
    shellcheck
    spotify
    foliate
    transmission_4-gtk
    wl-clipboard
    xxd
    zathura
    zellij
    zsh

    # packages Nix wants
    glibcLocales
  ];
  improved-pkgs = builtins.attrValues {
    zsh-autosuggestions = buildZshPlugin {
      name = "zsh-autosuggestions";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "master";
        sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
      };
    };
    zsh-syntax-highlighting = buildZshPlugin {
      name = "zsh-syntax-highlighting";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "master";
        sha256 = "sha256-VMne38IQwqB4jwGUI2f3eEiSkT2ww7+G5ch7w+65GT0=";
      };
    };
    neovim =
      let
        customVimPlugins = {
          vim-bling = pkgs.vimUtils.buildVimPlugin {
            name = "vim-bling";
            src = pkgs.fetchFromGitHub {
              owner = "ivyl";
              repo = "vim-bling";
              rev = "master";
              sha256 = "sha256-iJ/uaoq71IM1hQSrnZ86MBlpUWRp9vs1Grd1+9C1QFM=";
            };
          };
          vim-languages = pkgs.vimUtils.buildVimPlugin {
            name = "vim-languages";
            src = pkgs.fetchFromGitHub {
              owner = "supki";
              repo = "vim-languages";
              rev = "master";
              sha256 = "sha256-9RuVISRz7Xkdbp2X4er36X+BcqgK/TeY2qahWYOYI/0=";
            };
          };
          kdl-vim = pkgs.vimUtils.buildVimPlugin {
            name = "kdl-vim";
            src = pkgs.fetchFromGitHub {
              owner = "imsnif";
              repo = "kdl.vim";
              rev = "master";
              sha256 = "sha256-IajKK1EjrKs6b2rotOj+RlBBge9Ii2m/iuIuefnjAE4=";
            };
          };
        };
      in
        pkgs.neovim.override {
          vimAlias = true;
          configure = {
            customRC = ''
              lua require("init")
            '';
            packages.myVimPackage = with pkgs.vimPlugins // customVimPlugins; {
              start = let
                nvim-treesitter-with-plugins = nvim-treesitter.withPlugins (
                  plugins: [
                    plugins.tree-sitter-clojure
                    plugins.tree-sitter-haskell
                    plugins.tree-sitter-json
                    plugins.tree-sitter-lua
                    plugins.tree-sitter-nix
                    plugins.tree-sitter-purescript
                    plugins.tree-sitter-ruby
                    plugins.tree-sitter-yaml
                  ]
                );
              in [
                fzf-vim
                gitsigns-nvim
                golden-ratio
                haskell-tools-nvim
                nightfox-nvim
                lualine-nvim
                lsp-status-nvim
                nvim-treesitter-with-plugins
                rainbow-delimiters-nvim
                vim-bling
                vim-commentary
                vim-languages
                vim-sandwich
                which-key-nvim

                # fallback for treesitter failures
                haskell-vim
                purescript-vim
                vim-nix
                kdl-vim
              ];
              opt = [
              ];
            };
          };
        };
    stack-but-xdg-aware = (
      pkgs.writeScriptBin "stack" ''
        #!${pkgs.runtimeShell}

        xdg_config_home_config_yaml=''${XDG_CONFIG_HOME:-$HOME/.config}/stack/config.yaml
        stack_root=$HOME/.stack
        stack_root_config_yaml=$stack_root/config.yaml
        if [ -f "$xdg_config_home_config_yaml" ]; then
          mkdir -p "$stack_root"
          cp "$xdg_config_home_config_yaml" "$stack_root_config_yaml"
        fi

        exec "${pkgs.stack}/bin/stack" "$@"
      ''
    ) // {
      name = "stack";
      version = pkgs.stack.version;
    };
    dazu = overridenHaskellPackages.dazu;
    t = overridenHaskellPackages.t;
    # relocant's tests require a running PostgreSQL server; no way
    # I'm figuring out how to set that up properly here.
    relocant = pkgs.haskell.lib.dontCheck overridenHaskellPackages.relocant;
  };
in
{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "spotify"
  ];
  users.users.m = {
    packages = pristine-pkgs ++ improved-pkgs;
  };
}
