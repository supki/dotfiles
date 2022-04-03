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
  nixgl = import <nixgl> {};
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
      sha256 = "sha256-UqeK+xFcKMwdM62syL2xkV8jwkf/NWfubxOTtczWEwA=";
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
            sha256 = "sha256-c0opD24dFBQxiYITOUKTyX0msqT5B3hMb2rKnPuLqEo=";
          };
        };
        vim-languages = super.vimUtils.buildVimPlugin {
          name = "vim-languages";
          src = super.fetchFromGitHub {
            owner = "supki";
            repo = "vim-languages";
            rev = "master";
            sha256 = "sha256-ThAkYrykRAwk/jWj3ymjxwSZKfnyJcOdB3lmPUJxLcc=";
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
              rainbow_parentheses-vim
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
  m-packages = super.buildEnv {
    name = "m-packages";
    paths = with super; [
      # packages I want
      alacritty
      diff-so-fancy
      fira-code
      git
      stack
      htop
      jq
      mpv
      neovim
      nodejs
      pass
      tmux
      tree
      x-selection-sync
      yarn
      zsh
      zsh-autosuggestions
      zsh-syntax-highlighting

      # packages Nix wants
      glibcLocales
      nixgl.nixGLIntel
    ];
  };
}
