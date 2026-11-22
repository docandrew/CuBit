------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Generated typed-setting decoders (ccl-manifest --setting-codecs DIR).
--
--  For each CCL-typed setting in the catalog, one Ada package
--  CCL.Setting_Codecs.<Root>: the root type and every type it reaches as
--  Ada types (records, enumerations, variant records, range types, Boolean,
--  Integer_64 and CuBit.Canonical_Readers.Text for String), and Decode, which
--  reads the value's canonical source (CCL.Typed_Settings renders it) with
--  CuBit.Canonical_Readers. The type is analysed here, at build time, by the
--  same CCL analyser Config uses; a reader then needs no CCL at run time.
--
--  The type declarations come from CCL.Setting_Kinds as compiled into this
--  tool, which ccl-manifest --setting-kinds generated from the same catalog:
--  Emit refuses a catalog whose typed settings differ from it (regenerate
--  the kinds and rebuild the tool first; make -C kernel setting-kinds does
--  both).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Failures;
with Setting_Catalogs;

package Setting_Codecs is
   procedure Emit
     (Settings : Setting_Catalogs.Catalog; Directory : String;
      Success : out Boolean; Why : out CuBit.Failures.Failure);
end Setting_Codecs;
