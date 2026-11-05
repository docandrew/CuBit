pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Catalog;
with ACPI_FADT.Transactions;
-- Read-only firmware-data records, separate from fixed-register scalar I/O.
-- Requests select a named NVS record, bounded byte offset and aligned width, never an
-- address or write. Scope comes from authenticated kernel receive metadata.
-- One captured catalog/backing owner outlives this context and every read.
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
   Hardware : in out Catalog.Context;
   -- Fresh independent kernel class/cache fact for this retained window.
   with function Is_NVS (Window : Catalog.W.Window_ID) return Boolean;
   with procedure Transfer
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_FADT.Transactions.Access_Width;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean);
   Maximum_Records : Positive := 16;
package ACPI_Backend_NVS_Records with SPARK_Mode is
   package T renames ACPI_FADT.Transactions;
   subtype Word is Interfaces.Unsigned_64;
   type Record_ID is new Word;
   No_Record : constant Record_ID := 0;
   type Epoch_Number is new Word;
   No_Epoch : constant Epoch_Number := 0;
   subtype Record_Count is Natural range 0 .. Maximum_Records;
   type Phase is (Fresh, Building, Serving, Failed);
   type Outcome is (Configured, Added, Sealed, Ready, Done, Denied, Stale,
     Not_Ready, Already_Consumed, Invalid_Record, Duplicate_Record,
     Capacity_Exceeded, Catalog_Changed, Backing_Unavailable, Read_Failed, Reentered);
   type Context is limited private;
   type Statistics is record
      Stage : Phase;
      Records : Record_Count;
      Working : Boolean;
   end record;
   function Observe (State : Context) return Statistics;
   procedure Configure (State : in out Context; Scope : Word; Result : out Outcome);
   -- Trusted startup only. The window must already be mapped, readable NVS;
   -- no caller-supplied extent or class may create or widen this record.
   procedure Add
     (State : in out Context; ID : Record_ID; Window : Catalog.W.Window_ID; Result : out Outcome);
   procedure Seal (State : in out Context; Result : out Outcome);
   type Header_Report is record
      Status : Outcome := Not_Ready;
      Epoch : Epoch_Number := No_Epoch;
      Count : Record_Count := 0;
   end record;
   type Description_Report is record
      Status : Outcome := Not_Ready;
      ID : Record_ID := No_Record;
      Base, Length : Word := 0;
   end record;
   procedure Header (State : in out Context; Scope : Word; Result : out Header_Report);
   procedure Describe
     (State : in out Context; Scope : Word; Epoch : Epoch_Number;
      Ordinal : Natural; Result : out Description_Report);
   procedure Read
     (State : in out Context; Scope : Word; Epoch : Epoch_Number;
      ID : Record_ID; Index : Word; Width : T.Access_Width; Value : out Word; Result : out Outcome);
   -- No reset, remap, write or implicit retry exists. Catalog revision changes,
   -- lost NVS backing and failed transfers permanently stop this context.
   -- Metadata/epochs are tied to this backend peer; a new peer needs a new
   -- client binding. Callbacks return normally and cannot alias/revoke state.
private
   subtype Row_Index is Positive range 1 .. Maximum_Records;
   type Row is record
      ID : Record_ID := No_Record;
      Window : Catalog.W.Window_ID := Catalog.W.No_Window;
      Base, Length : Word := 0;
   end record;
   type Rows is array (Row_Index) of Row;
   type Context is limited record
      Stage : Phase := Fresh;
      Scope : Word := 0;
      Revision : Catalog.Revision_Number := Catalog.No_Revision;
      Count : Record_Count := 0;
      Items : Rows;
      Working : Boolean := False;
   end record;
end ACPI_Backend_NVS_Records;
