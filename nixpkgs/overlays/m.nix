self: super:
let
  buildZshPlugin = { name, src }:
    super.stdenv.mkDerivation {
      inherit name src;

      installPhase = ''
        runHook preInstall
        target=$out/share/zsh/${name}
        mkdir -p $target
        cp -r . $target
        runHook postInstall
      '';
    };
in
rec {
  zsh-autosuggestions = buildZshPlugin {
    name = "zsh-autosuggestions";
    src = super.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-autosuggestions";
      rev = "master";
      sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
    };
  };
  zsh-syntax-highlighting = buildZshPlugin {
    name = "zsh-syntax-highlighting";
    src = super.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-syntax-highlighting";
      rev = "master";
      sha256 = "sha256-KRsQEDRsJdF7LGOMTZuqfbW6xdV5S38wlgdcCM98Y/Q=";
    };
  };
  neovim =
    let
      customVimPlugins = {
        vim-bling = super.vimUtils.buildVimPlugin {
          name = "vim-bling";
          src = super.fetchFromGitHub {
            owner = "ivyl";
            repo = "vim-bling";
            rev = "master";
            sha256 = "sha256-iJ/uaoq71IM1hQSrnZ86MBlpUWRp9vs1Grd1+9C1QFM=";
          };
        };
        vim-languages = super.vimUtils.buildVimPlugin {
          name = "vim-languages";
          src = super.fetchFromGitHub {
            owner = "supki";
            repo = "vim-languages";
            rev = "master";
            sha256 = "sha256-2neE7qKT2/zpn78JNEpkaKAnj7rD1HzygAurqbyUc8M=";
          };
        };
        kdl-vim = super.vimUtils.buildVimPlugin {
          name = "kdl-vim";
          src = super.fetchFromGitHub {
            owner = "imsnif";
            repo = "kdl.vim";
            rev = "master";
            sha256 = "sha256-IajKK1EjrKs6b2rotOj+RlBBge9Ii2m/iuIuefnjAE4=";
          };
        };
      };
    in
      super.neovim.override {
        vimAlias = true;
        configure = {
          customRC = ''
            lua require("init")
          '';
          packages.myVimPackage = with super.vimPlugins // customVimPlugins; {
            start = let
              nvim-treesitter-with-plugins = nvim-treesitter.withPlugins (
                plugins: [
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
  stack = (
    self.writeScriptBin "stack" ''
      #!${self.runtimeShell}

      xdg_config_home_config_yaml=''${XDG_CONFIG_HOME:-$HOME/.config}/stack/config.yaml
      stack_root=$HOME/.stack
      stack_root_config_yaml=$stack_root/config.yaml
      if [ -f "$xdg_config_home_config_yaml" ]; then
        mkdir -p "$stack_root"
        cp "$xdg_config_home_config_yaml" "$stack_root_config_yaml"
      fi

      exec "${super.stack}/bin/stack" "$@"
    ''
  ) // {
    name = "stack";
    version = super.stack.version;
  };
  x-selection-sync = self.stdenv.mkDerivation {
    name = "x-selection-sync";
    src = self.fetchFromGitHub {
      owner = "supki";
      repo = "x-selection-sync";
      rev = "master";
      sha256 = "sha256-w/nt6LKB8QbM8GxkiI7DcS4xNqhVfR/LKznWjQ2qle8=";
    };
    buildInputs = [
      self.pkg-config
      self.xorg.libX11
      self.xorg.libXfixes
    ];
    installPhase = ''
      runHook preInstall
      target=$out/bin
      mkdir -p $target
      cp ./build/x-selection-sync $target
      runHook postInstall
    '';
  };
  haskellPackages = super.haskellPackages.override (_: {
    overrides = self.haskell.lib.packageSourceOverrides {
      t = self.fetchFromGitHub {
        owner = "supki";
        repo = "t";
        rev = "main";
        sha256 = "sha256-vA3/JVAXU2XkeRjfHRGTmUhXIOmWtkd5weIWAr+SO74=";
      };
      dazu = self.fetchFromGitHub {
        owner = "supki";
        repo = "da";
        rev = "main";
        sha256 = "sha256-VOEPXnAg5cPaKyxtf/plNbBJuVfoqptuPGRvZjmxsuk=";
      };
      relocant = self.fetchFromGitHub {
        owner = "supki";
        repo = "relocant";
        rev = "main";
        sha256 = "sha256-CrYxUotiVZwRncbquxTE2Cm1uIsvxow+tJVWkUYKyTQ=";
      };
    };
  });
  haskell-language-servers = ghcs: super.haskell-language-server.override {
    supportedGhcVersions = ghcs;
  };
  dazu = haskellPackages.dazu;
  t = haskellPackages.t;
  # relocant's tests require a running PostgreSQL server; no way
  # I'm figuring out how to set that up properly here.
  relocant = super.haskell.lib.dontCheck haskellPackages.relocant;
  nix-rebuild-env = super.writeScriptBin "nix-rebuild-env" ''
    #!${super.stdenv.shell}
    exec nix-env -r -iA nixos.m-env
  '';
  m-env = super.buildEnv {
    name = "m-env";
    paths = with super; [
      # packages I want
      alacritty
      bat
      bookworm
      chromium
      dazu
      diff-so-fancy
      difftastic
      dig
      eza
      feh
      fira-code
      fzf
      git
      gnome-tweaks
      (haskell-language-servers ["984"])
      htop
      inetutils
      iotop
      jq
      moreutils
      mpv
      neovim
      nix-rebuild-env
      openssl
      pass
      relocant
      scrot
      shellcheck
      spotify
      stack
      steam
      foliate
      t
      tmux
      transmission_3-gtk
      x-selection-sync
      xsel
      zathura
      zellij
      zsh
      zsh-autosuggestions
      zsh-syntax-highlighting

      # packages Nix wants
      glibcLocales

      self.p-env
    ];
  };
}
