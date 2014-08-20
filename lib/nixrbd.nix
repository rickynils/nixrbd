### Various Nix helpers for creating iPXE boot scripts

{ writeText, lib }:

with lib;

rec {
  inherit (builtins) hasAttr getAttr attrNames head;

  mkIpxe = s: writeText "script.ipxe" "#!ipxe\n${s}";

  mkInitrdEntry = nixosPath: "initrd ${nixosPath}/initrd";

  mkKernelEntry = nixosPath:
    "kernel ${nixosPath}/kernel systemConfig=${nixosPath} " +
    "init=${nixosPath}/init " +
    builtins.readFile "${nixosPath}/kernel-params";

  mkNixosBootEntry = nixos: ''
    ${mkInitrdEntry "${nixos.system}"}
    ${mkKernelEntry "${nixos.system}"}
    boot
  '';

  mkNixosBootEntryFromPath = nixosPath: ''
    ${mkInitrdEntry nixosPath}
    ${mkKernelEntry nixosPath}
    boot
  '';

  mkNixosBootScript = nixos: mkIpxe (mkNixosBootEntry nixos);

  pathPrefix = request: let
    inherit (request) path fullPath;
    l = builtins.length;
    ps = take (builtins.sub (l fullPath) (l path)) fullPath;
  in "${optionalString (ps != []) "/${concatStringsSep "/" ps}"}/";

  mkMenu = request: entries:
    let
      entryNames = attrNames entries;
      labels = concatMapStrings
        (l: ":${l}\nchain ${pathPrefix request}${l}\n") entryNames;
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
      path = if request.path == [] then "" else head request.path;
    in if hasAttr path handler
      then getAttr path handler
      else handler.default;
}
