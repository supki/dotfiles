{
  packageOverrides = pkgs: rec {
    nixgl = import <nixgl> {};
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
    m-zsh-autosuggestions = buildZshPlugin {
      name = "zsh-autosuggestions";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-autosuggestions";
        rev = "master";
        sha256 = "sha256-KLUYpUu4DHRumQZ3w59m9aTW6TBKMCXl2UcKi4uMd7w=";
      };
    };
    m-zsh-syntax-highlighting = buildZshPlugin {
      name = "zsh-syntax-highlighting";
      src = pkgs.fetchFromGitHub {
        owner = "zsh-users";
        repo = "zsh-syntax-highlighting";
        rev = "master";
        sha256 = "sha256-UqeK+xFcKMwdM62syL2xkV8jwkf/NWfubxOTtczWEwA=";
      };
    };
    m-neovim =
      let
        customVimPlugins = {
          vim-bling = pkgs.vimUtils.buildVimPlugin {
            name = "vim-bling";
            src = pkgs.fetchFromGitHub {
              owner = "ivyl";
              repo = "vim-bling";
              rev = "master";
              sha256 = "sha256-c0opD24dFBQxiYITOUKTyX0msqT5B3hMb2rKnPuLqEo=";
            };
          };
          vim-languages = pkgs.vimUtils.buildVimPlugin {
            name = "vim-languages";
            src = pkgs.fetchFromGitHub {
              owner = "supki";
              repo = "vim-languages";
              rev = "master";
              sha256 = "sha256-ThAkYrykRAwk/jWj3ymjxwSZKfnyJcOdB3lmPUJxLcc=";
            };
          };
        };
      in
        pkgs.neovim.override {
          vimAlias = true;
          configure = {
            customRC = builtins.readFile ./init.vim;
            packages.myVimPackage = with pkgs.vimPlugins // customVimPlugins; {
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
    x-selection-sync = pkgs.stdenv.mkDerivation {
      name = "x-selection-sync";
      src = pkgs.fetchFromGitHub {
        owner = "supki";
        repo = "x-selection-sync";
        rev = "master";
        sha256 = "sha256-w/nt6LKB8QbM8GxkiI7DcS4xNqhVfR/LKznWjQ2qle8=";
      };
      buildInputs = [
        pkgs.pkg-config
        pkgs.xorg.libX11
        pkgs.xorg.libXfixes
      ];
      installPhase = ''
        runHook preInstall
        target=$out/bin
        mkdir -p $target
        cp ./build/x-selection-sync $target
        runHook postInstall
      '';
    };
    m-packages = pkgs.buildEnv {
      name = "m-packages";
      paths = with pkgs; [
        # packages I want
        alacritty
        diff-so-fancy
        git
        stack
        htop
        jq
        nodejs
        m-neovim
        m-zsh-autosuggestions
        m-zsh-syntax-highlighting
        pass
        tmux
        yarn
        x-selection-sync
        zsh

        # packages Nix wants
        glibcLocales
        nixgl.nixGLIntel
      ];
    };
  };
}
