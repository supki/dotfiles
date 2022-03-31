{
  packageOverrides = pkgs: with pkgs; rec {
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
        neovim.override {
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
    m-packages = pkgs.buildEnv {
      name = "m-packages";
      paths = [
        git
        pass
        m-neovim
        tmux
        zsh
      ];
    };
  };
}
