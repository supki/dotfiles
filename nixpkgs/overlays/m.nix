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
      sha256 = "sha256-YV9lpJ0X2vN9uIdroDWEize+cp9HoKegS3sZiSpNk50=";
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
            sha256 = "sha256-Dd7GwlGZoVG2VLu+2uLRR11BROZSgiJRuO0quEYofW0=";
          };
        };
      };
    in
      super.neovim.override {
        vimAlias = true;
        configure = {
          customRC = builtins.readFile ../../nvim/init.vim;
          packages.myVimPackage = with super.vimPlugins // customVimPlugins; {
            start = [
              fzf-vim
              golden-ratio
              haskell-vim
              rainbow
              seoul256-vim
              vim-airline
              vim-airline-themes
              vim-bling
              vim-commentary
              vim-languages
              vim-nix
              vim-sandwich
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
  m-env = super.buildEnv {
    name = "m-env";
    paths = with super; [
      # packages I want
      alacritty
      diff-so-fancy
      dig
      feh
      fira-code
      git
      htop
      iotop
      jq
      moreutils
      mpv
      neovim
      nodejs
      pass
      scrot
      shellcheck
      stack
      tmux
      transmission-gtk
      tree
      x-selection-sync
      xsel
      yarn
      zellij
      zsh
      zsh-autosuggestions
      zsh-syntax-highlighting

      # packages Nix wants
      glibcLocales
      self.nixgl.nixGLIntel
    ];
  };
}
