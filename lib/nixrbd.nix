### Various Nix helpers for creating iPXE boot scripts

{ writeText, lib }:

with lib;

rec {
  inherit (builtins) hasAttr getAttr attrNames head;

  mkIpxe = s: writeText "script.ipxe" "#!ipxe\n${s}";

  mkInitrdEntry = nixos: "initrd ${nixos.system}/initrd";

  mkKernelEntry = nixos: let sys = nixos.system; cfg = nixos.config; in
    "kernel ${sys}/kernel systemConfig=${sys} init=${sys}/init " +
    builtins.toString (cfg.boot.kernelParams ++ cfg.boot.extraKernelParams);

  mkNixosBootEntry = nixos: ''
    ${mkInitrdEntry nixos}
    ${mkKernelEntry nixos}
    boot
  '';

  mkNixosBootScript = nixos: mkIpxe (mkNixosBootEntry nixos);

  mkMenu = entries:
    let
      entryNames = attrNames entries;
      labels = concatMapStrings
        (l: ":${l}\nchain /${l}\n") entryNames;
      menuEntries = concatMapStrings
        (l: "item ${l} ${getAttr l entries}\n") entryNames;
    in mkIpxe ''
      :__menu
      menu Select a boot alternative
      ${menuEntries}
      choose target && goto ''${target} || goto __menu
      ${labels}
    '';

  mkHandler = request: handler:
    let
      path = if request.pathInfo == [] then "" else head request.pathInfo;
    in if hasAttr path handler
      then getAttr path handler
      else handler.default;
}
