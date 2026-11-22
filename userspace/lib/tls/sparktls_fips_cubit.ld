/* FIPS 140-3 module boundary for SPARKTLS's software integrity test, for
   CuBit's link (userspace/c/link.ld as the main -T script).

   The pinned SPARKTLS ships ld/sparktls_fips.ld, which names the module by
   its Alire archives (libSparktls.a, libsparktlscrypto.a, ...). CuBit
   compiles the same crates in place, unmodified, as loose objects in
   userspace/lib/tls/build (sparktls_cubit.gpr), so this copy names that
   directory instead: every object in it is part of the module (sparktls
   and its RecordFlux code, sparktlscrypto, sparkx509, sparkmlkem,
   SPARKNaCl, libkeccak, sparkentropy) and nothing else is built there.
   The CuBit runtime and libc stay outside, as upstream's GNAT runtime and
   libc do.

   SPARKTLS.Integrity HMACs .fips_text and .fips_rodata at start-up (in
   RBG.Init, FIPS mode) and compares them with .fips_hmac, which the
   pinned crate's tools/fips_inject fills in after the final link
   (kernel/Makefile, the fips-inject tool). A program linked without that
   step fails the self-test and never starts a session. */

SECTIONS
{
  .fips_text : ALIGN(64)
  {
    __fips_text_start = .;
    */lib/tls/build/*.o(.text .text.*)
    __fips_text_end = .;
  }
}
INSERT AFTER .text;

SECTIONS
{
  .fips_rodata : ALIGN(64)
  {
    __fips_rodata_start = .;
    */lib/tls/build/*.o(.rodata .rodata.*)
    __fips_rodata_end = .;
  }
}
INSERT AFTER .rodata;
