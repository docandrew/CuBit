pragma Ada_2022;
with Interfaces; use Interfaces;
private with Heap_Arrays;

--  metrics.svc's string table (MET-002, DESIGN-metrics-store.md): every
--  label name and value and every series name is interned once, and
--  everything else holds its String_Id. Samples never carry strings.
--
--  The text, the entries and the hash index grow on the process heap
--  (CuAlloc, Heap_Arrays) by doubling, up to Maximum_Bytes of text and
--  Maximum_Strings strings; past either, or when the heap refuses, Intern
--  answers No_String and counts the refusal.
package Metric_Strings with SPARK_Mode is
   Maximum_Bytes : constant := 4 * 1024 * 1024;
   Maximum_Strings : constant := 262_144;
   Maximum_Length : constant := 256;
   subtype String_Id is Natural range 0 .. Maximum_Strings;
   No_String : constant String_Id := 0;

   type Table is limited private;

   --  FNV-1a, 64 bits: the string index's and the series IDs' hash.
   Offset_Basis : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   Prime : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
   function Hash (Text : String; Seed : Unsigned_64 := Offset_Basis) return Unsigned_64;

   --  Text's ID, interning it when it is new; No_String when Text is empty,
   --  longer than Maximum_Length, or the table is at a ceiling.
   procedure Intern (T : in out Table; Text : String; Id : out String_Id);
   --  Text's ID if it is interned, else No_String (nothing added).
   function Find (T : Table; Text : String) return String_Id;
   function Text_Of (T : Table; Id : String_Id) return String;

   function Count (T : Table) return Natural;
   function Bytes_Held (T : Table) return Unsigned_64;
   function Refused (T : Table) return Unsigned_64;
private
   pragma SPARK_Mode (Off);
   type Entry_Record is record
      Offset : Natural := 0;
      Length : Natural := 0;
      Hash : Unsigned_64 := 0;
   end record;
   package Text_Blocks is new Heap_Arrays (Character);
   package Entry_Blocks is new Heap_Arrays (Entry_Record);
   package Slot_Blocks is new Heap_Arrays (String_Id);
   type Table is limited record
      Text : Text_Blocks.Block := Text_Blocks.Empty;
      Used_Bytes : Natural := 0;
      Entries : Entry_Blocks.Block := Entry_Blocks.Empty;
      Used : Natural := 0;
      Slots : Slot_Blocks.Block := Slot_Blocks.Empty;
      Refusals : Unsigned_64 := 0;
   end record;
end Metric_Strings;
