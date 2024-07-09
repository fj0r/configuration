{
  inputs.nixpkgs.url = "nixpkgs";

  outputs = all@{ self, nixpkgs, ... }: {

    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
      ] ;
    };
  };
}
