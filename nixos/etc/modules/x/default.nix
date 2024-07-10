{  pkgs,  lib,  ...}:

{
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  environment.systemPackages = with pkgs; [
    gparted
    xclip
    neovide
    alacritty
    vivaldi
  ];

  environment.variables = {
    TERM = "xterm-256color";
  };

}