# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "ruciferno";
  networking.networkmanager = {
    enable = true;
    packages = [ pkgs.networkmanager_openvpn ];
  };

  time.timeZone = "Europe/Helsinki";

  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;
  
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    extraConfig = "load-module module-switch-on-connect";
  };
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };
  
  services.xserver = {
    desktopManager = {
    	default = "gnome";
    	gnome3.enable = true;
    };
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "eurosign:e";
    libinput.enable = true;
  };

  nixpkgs.config.allowUnfree = true;
  virtualisation.docker.enable = true;
  
  programs.fish.enable = true;
  programs.fish.shellInit = "starship init fish | source";
  
  users.users.rucy = {
    isNormalUser = true;
    extraGroups  = [ "docker" "wheel" "audio" "networkmanager"];
    shell = pkgs.fish;
  };

  nix = {
    
    autoOptimiseStore = true;

    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 7d";
    };

    extraOptions = ''
      keep-outputs     = true
      keep-derivations = true
    '';

    trustedUsers = [ "root" "rucy" ];
  };

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "3270" "BigBlueTerminal" "Hasklig" "FantasqueSansMono" "Mononoki" ]; })
    font-awesome
  ];

  services.pcscd.enable = true;
  
  # environment.systemPackages = with pkgs; [ google-chrome ];
  
  system.stateVersion = "20.09";

}
