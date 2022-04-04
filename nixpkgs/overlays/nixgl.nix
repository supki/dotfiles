self: super:
{
  nixgl = import (builtins.fetchTarball "https://github.com/guibou/nixGL/archive/main.tar.gz") { pkgs = self; };
}
