------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Which driver each present device binds - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CCL.Resource_Sections;
package body Devmgr_Bindings with SPARK_Mode => On is
   use type DI.Decode_Status;
   use type DS.Match_Kind;
   use type DS.Platform_Device;

   Loaded_Index : DI.Index;

   procedure Load (Data : DI.Byte_Array; Status : out DI.Decode_Status) is
   begin
      DI.Decode (Data, Loaded_Index, Status);
      if Status /= DI.Decoded then
         Loaded_Index := (others => <>);
      end if;
   end Load;

   --  Load keeps only a decoded index; an empty one otherwise.
   function Index return DI.Index is
     (if DI.Valid_Index (Loaded_Index) then Loaded_Index else (others => <>));

   function Driver_Named (Name : String) return DI.Driver_Count is
     (DI.Find (Loaded_Index, Name));

   function Name_Of (Driver : DI.Driver_Number) return String is
     (Loaded_Index.Drivers (Driver).Name.Text (1 .. Loaded_Index.Drivers (Driver).Name.Length));

   procedure Bind (F : DS.PCI_Function; Result : out DI.Binding) is
   begin
      DI.Bind (Index, F, Result);
   end Bind;

   function Platform_Driver (P : DS.Platform_Device) return DI.Driver_Count is
   begin
      for D in 1 .. Loaded_Index.Count loop
         if DS.Declares_Platform (Loaded_Index.Drivers (D).Matches, P) then
            return D;
         end if;
      end loop;
      return 0;
   end Platform_Driver;

   --  Field by field: no memcmp in devmgr's freestanding runtime.
   function Same (A, B : DS.Device_Match) return Boolean is
     (A.Kind = B.Kind and then A.Vendor = B.Vendor and then A.First = B.First
      and then A.Last = B.Last and then A.Class = B.Class and then A.Subclass = B.Subclass
      and then A.Program = B.Program and then A.ID_Length = B.ID_Length
      and then (for all C in DS.ACPI_Index => A.ID (C) = B.ID (C))
      and then A.Device = B.Device);

   function Agrees (Driver : DI.Driver_Number; Own : DS.Match_Table) return Boolean is
      Listed : DS.Match_Table renames Loaded_Index.Drivers (Driver).Matches;
   begin
      return Listed.Count = Own.Count
        and then (for all I in 1 .. Own.Count => Same (Listed.Entries (I), Own.Entries (I)));
   end Agrees;

   function Hex (Value : Unsigned_64; Digits_Count : Positive) return String is
      Characters : constant String (1 .. 16) := "0123456789ABCDEF";
      Result : String (1 .. Digits_Count);
      Remaining : Unsigned_64 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Characters (Natural (Remaining and 15) + 1);
         Remaining := Shift_Right (Remaining, 4);
      end loop;
      return Result;
   end Hex;

   function Platform_Name (P : DS.Platform_Device) return String is
     (case P is
         when CCL.Resource_Sections.PS2_Controller => "PS2_Controller",
         when CCL.Resource_Sections.ATA_Primary    => "ATA_Primary",
         when CCL.Resource_Sections.CMOS_RTC       => "CMOS_RTC");

   CLASS_TRIPLE_TEXT : constant := 8;   --  "CC.SS.PP"
   function Triple (M : DS.Device_Match) return String
   with Post => Triple'Result'Length = CLASS_TRIPLE_TEXT;
   function Triple (M : DS.Device_Match) return String is
     (Hex (Unsigned_64 (M.Class), 2) & "." & Hex (Unsigned_64 (M.Subclass), 2) & "."
      & Hex (Unsigned_64 (M.Program), 2));

   function Image (M : DS.Device_Match) return String is
     (case M.Kind is
         when DS.PCI_IDs =>
            "PCI_IDs " & Hex (Unsigned_64 (M.Vendor), 4) & ":" & Hex (Unsigned_64 (M.First), 4)
            & ".." & Hex (Unsigned_64 (M.Last), 4),
         when DS.PCI_Class => "PCI_Class " & Triple (M),
         when DS.USB_Class => "USB_Class " & Triple (M),
         when DS.ACPI_ID => "ACPI_ID " & String (M.ID (1 .. M.ID_Length)),
         when DS.Platform => "Platform " & Platform_Name (M.Device));
end Devmgr_Bindings;
