Enabling Bluetooth on your system may require an additional
firmware to be flashed into the device at boot.
Here is how you can do it on NixOS.

First of all it is necessary to identify the hardware.
This can be normally done with
`lsusb` command from `usbutils` package:
```bash
nix-env -f /path/to/nixpkgs -iA pkgs.usbutils
lsusb
```
Find the manufacturer and the model `ID abcd:0123` in the output.

The next goal is to obtain an `hcd` file matching the detected hardware.
This can be a little tricky since the manufacturer might not officially
support GNU/Linux. It might be possible to get a `hex` file which
should then be converted to `hcd` with a `hex2hcd` utility.

Suppose that the required file is available, and that the manufacturer
happens to be Broadcom: `broadcom-abcd-0123.hcd`.
Make a directory `/path/to/myBluetooth` and
put this file in there. Create in the same directory two other files,
`default.nix`:
```nix
{ stdenv }:
stdenv.mkDerivation {
  name = "myBluetooth";
  src = ./.;
  builder = ./builder.sh;
}
```
and `builder.sh`:
```bash
source $stdenv/setup
mkdir -p $out/lib/firmware/brcm
cp $src/broadcom-abcd-0123.hcd $out/lib/firmware/brcm/BCM.hcd
```
The name of the directory `$out/lib/firmware/brcm`
corresponds to Broadcom, and the name of the copy of the file
should be `BCM.hcd`, not `broadcom-abcd-0123.hcd`.
One might need different names for another manufacturer.

This defines a package `myBluetooth`. Put the following
in `/etc/nixos/configuration.nix`:
```nix
{
  hardware = {
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    bluetooth.enable = true;
    firmware = [ (pkgs.callPackage "/path/to/myBluetooth" {}) ];
  };
}
```
Override the default choice (version 4) of `bluez`:
```nix
{
  nixpkgs.config.packageOverrides = pkgs: {
    bluez = pkgs.bluez5;
  };
}
```
Rebuild your system:
```bash
nixos-rebuild dry-run -I nixpkgs=/path/to/nixpkgs
sudo -i nixos-rebuild switch -I nixpkgs=/path/to/nixpkgs
```
Do not forget `nixpkgs=` in front of `/path/to/nixpkgs`.

Turn off the computer. Wait a few seconds and boot NixOS.
Check the logs:
```bash
journalctl -b | grep -i blue
systemctl status -l dbus-org.bluez.service
```
If everything is fine it should be possible to switch on the  Bluetooth:
```bash
hciconfig
sudo hciconfig hci0 up
hciconfig -a
```
where in place of `hci0` one might need to put a different parameter.
To get the MAC address of the chip, use:
```bash
hcitool dev
```
and to see the surrounding devices, execute:
```bash
hcitool scan
```
Establish a pairing with an external device as follows:
```bash
bluez-simple-agent hci0 00:11:22:33:44:55
```
where `00:11:22:33:44:55` is the address found with scanning.
The program will request to enter a PIN code.

I was able to transfer a file from a Bluetooth phone as follows:
```bash
obexftp -b 00:11:22:33:44:55 -c /'Memory card' -l
obexftp -b 00:11:22:33:44:55 -g /'Memory card'/Image001.jpg
```
To switch off the Bluetooth, run:
```bash
sudo hciconfig hci0 down
```
