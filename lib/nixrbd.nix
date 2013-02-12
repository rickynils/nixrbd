### Various Nix helpers for creating iPXE boot scripts

{ writeText, lib }:

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

  mkMenu = entries:
    let
      entryNames = attrNames entries;
      labels = lib.concatMapStrings (l: ":${l}\nchain /${l}\n") entryNames;
      menuEntries = lib.concatMapStrings (l: "item ${l} ${(getAttr l entries).title}\n") entryNames;
    in mkIpxe ''
      :__menu
      menu Select a boot alternative
      ${menuEntries}
      choose target && goto ''${target} || goto __menu
      ${labels}
    '';

   mkDefaultMenu = request: entries:
     let
       path = if request.pathInfo == [] then "" else head request.pathInfo;
     in if hasAttr path entries
       then mkIpxe ((getAttr path entries).script)
       else mkMenu entries;
}
