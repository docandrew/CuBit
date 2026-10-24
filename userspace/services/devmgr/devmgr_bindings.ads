------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which driver each present device binds (HW-002, docs/device-matching.md).
--
--  @description
--  devmgr loads the image's driver index (devices.index, generated from
--  every driver's .cubit.devices section when the image was built) and binds
--  each PCI function it found to the one driver whose declared entry
--  matches it most specifically (CCL.Device_Index.Bind). The index is
--  untrusted input: it is decoded by the proved decoder, and before a
--  driver runs devmgr checks that the driver's own section declares the
--  same devices (Agrees), so a tampered index cannot bind a driver to a
--  device it never declared.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Device_Sections;
with CCL.Device_Index;

package Devmgr_Bindings with SPARK_Mode => On is
   package DS renames CCL.Device_Sections;
   package DI renames CCL.Device_Index;

   --  Decode Data as the driver index; Loaded False (and an empty index)
   --  when it is malformed.
   procedure Load (Data : DI.Byte_Array; Status : out DI.Decode_Status)
   with Pre => Data'First = 1 and then Data'Length <= DI.MAX_INDEX_BYTES;

   function Index return DI.Index
   with Post => DI.Valid_Index (Index'Result);

   --  The driver file named Name, or 0.
   function Driver_Named (Name : String) return DI.Driver_Count;
   function Name_Of (Driver : DI.Driver_Number) return String
   with Pre => Driver <= Index.Count;

   procedure Bind (F : DS.PCI_Function; Result : out DI.Binding);

   --  The first driver declaring platform device P, or 0.
   function Platform_Driver (P : DS.Platform_Device) return DI.Driver_Count;

   --  Whether Own (a driver's .cubit.devices) declares exactly the entries
   --  the index lists for Driver, in the same order.
   function Agrees (Driver : DI.Driver_Number; Own : DS.Match_Table) return Boolean
   with Pre => Driver <= Index.Count;

   --  "8086:46D2" and similar renderings for devmgr's match log.
   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String
   with Pre => Digits_Count <= 16,
        Post => Hex'Result'First = 1 and then Hex'Result'Length = Digits_Count;
   function Platform_Name (P : DS.Platform_Device) return String;
   --  An entry in the manifest's notation: "PCI_IDs 8086:46D0..46D4".
   function Image (M : DS.Device_Match) return String
   with Pre => DS.Valid_Match (M);
end Devmgr_Bindings;
