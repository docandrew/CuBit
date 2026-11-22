pragma Ada_2022;
with Firmware_Tables;
with AML_Table_Backing;
with AML_Copied_Tables;
with AML_Table_Sources;
with Firmware_Tables.Identifiers;
-- Exact reads from caller-owned RAM only. The containing service keeps both
-- inputs stable during a call. No callback, allocation or mutable alias escapes.
-- These helpers do not admit AML, validate boot checksums, reserve definitions,
-- charge execution fuel or confer authority. Callers charge work before copying.
package AML_Table_Readers with SPARK_Mode, Pure is
   package F renames Firmware_Tables;
   use type F.Byte;
   package Sources renames AML_Table_Sources;
   use type Sources.Source_Kind;
   type Read_Status is (Readable, Invalid_Inventory, Source_Missing, Invalid_Window);
   subtype Table_Length is Natural range F.Table_Header_Size .. Natural'Last;
   type Table_Info (Status : Read_Status := Source_Missing) is record
      case Status is
         when Readable => Extent : Table_Length;
         when others => null;
      end case;
   end record;
   -- Checks inventory counts, combined quota representability and the selected
   -- span. Other boot spans are not scanned: the containing frozen owner checks
   -- its whole boot catalog on admission. Copied storage validity is rechecked.
   function Describe
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Item : Sources.Source) return Table_Info
     with Post => (if Describe'Result.Status = Readable then
       AML_Copied_Tables.Valid (Copies)
       and then Sources.Present
         ((Boot.Table_Capacity, Copies.Table_Capacity),
          (Boot.Count, AML_Copied_Tables.Count (Copies)), Item)
       and then (case Item.Kind is
         when Sources.No_Source => False,
         when Sources.Boot_Source =>
           AML_Table_Backing.Valid_Span (Boot, Positive (Item.Boot))
           and then Describe'Result.Extent = Boot.Tables (Positive (Item.Boot)).Extent,
         when Sources.Copied_Source =>
           AML_Copied_Tables.Present (Copies, AML_Copied_Tables.Table_ID (Item.Copied))
           and then Describe'Result.Extent =
             AML_Copied_Tables.Length (Copies, AML_Copied_Tables.Table_ID (Item.Copied))));
   type Selection_Status is
     (Table_Selected, No_Matching_Table, Invalid_Selection_Inventory);
   type Selection_Result (Status : Selection_Status := No_Matching_Table) is record
      case Status is
         when Table_Selected =>
            Item : Sources.Source;
            Extent : Table_Length;
         when others => null;
      end case;
   end record;
   -- Stable inventory order: boot slots first, then retained-copy slots.
   -- Match exact fixed-width header identifiers, with the query's optional OEM
   -- filters. Validate the whole boot span inventory before selecting anything.
   -- The owner admits checksums and charges the bounded header scan separately;
   -- a selected header grants neither namespace admission nor hardware access.
   function Find_Table
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Requested : Firmware_Tables.Identifiers.Selection) return Selection_Result
     with Post => (if Find_Table'Result.Status = Table_Selected then
       Find_Table'Result.Item.Kind /= Sources.No_Source
       and then Describe (Boot, Copies, Find_Table'Result.Item).Status = Readable
       and then Describe (Boot, Copies, Find_Table'Result.Item).Extent =
         Find_Table'Result.Extent);
   -- Zero-length windows at the exact end are valid. Refusals zero ALL output;
   -- no truncation, partial prefix or byte from a neighbouring table is returned.
   procedure Read_Window
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      Item : Sources.Source; Offset : Natural; Data : out F.Bytes;
      Status : out Read_Status)
     with Post => (if Status /= Readable then (for all B of Data => B = 0));
   Bits_Per_Octet : constant Positive := 8;
   -- A value descriptor identifies its immutable owner pool explicitly. It is
   -- valid only with those same containing owners; it is not an address or an
   -- authority token. Every read rechecks source, extent and bit bounds.
   type Field_View is record
      Item : Sources.Source := Sources.Absent_Source;
      Extent, Offset, Bits : Natural := 0;
   end record;
   function Field_Length (View : Field_View) return Natural is
     (View.Bits / Bits_Per_Octet +
      (if View.Bits mod Bits_Per_Octet = 0 then 0 else 1));
   function Valid_Field
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      View : Field_View) return Boolean;
   -- Logical bytes include an unaligned start and a zero-padded final partial
   -- byte. At most two physical octets are copied. Refusals return byte zero
   -- with an explicit status; no partial read is reported as success.
   procedure Read_Field_Byte
     (Boot : aliased AML_Table_Backing.State; Copies : AML_Copied_Tables.State;
      View : Field_View; Index : Natural; Value : out F.Byte;
      Status : out Read_Status)
     with Post => (if Status /= Readable then Value = 0);
end AML_Table_Readers;
