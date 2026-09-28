# Pinned Intel firmware and redistribution license for host tests and image
# realization. Packaging does not authorize upload or establish compatibility.
{
  blob = builtins.fetchurl {
    url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/20250917/i915/tgl_guc_70.bin";
    sha256 = "2f1f57a1b23d186f2592318d1e07a1365968932841ccb3e7177c516ba006e2f6";
  };
  license = builtins.fetchurl {
    url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/20250917/LICENSE.i915";
    sha256 = "8542aeabf2761935122d693561e16766ce1bcc2b0d003204f9040b7d6d929f2e";
  };
}
