{
  description = "/etc/nixos/flake.nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs, ... }: {
    nixosConfigurations.depression = nixpkgs.lib.nixosSystem {
      modules = [
        ./configuration.nix
        ./packages.nix
        ./packages-private.nix
      ];
    };
  };
}
