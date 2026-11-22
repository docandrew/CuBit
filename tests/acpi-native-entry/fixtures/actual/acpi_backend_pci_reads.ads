pragma Ada_2022;
with Interfaces;
with ACPI_Backend_Catalog;
with ACPI_PCI_Config_Reads;
-- One named, retained segment-zero/bus-zero ECAM record. Logical PCI offsets
-- never authorize SystemMemory access. Kernel receive metadata supplies Scope.
generic
   with package Catalog is new ACPI_Backend_Catalog (<>);
   Hardware : in out Catalog.Context;
   -- Fresh independent tag/cache/rights/geometry inspection of the retained
   -- kernel handle. True attests the kernel's exact read-only/uncached bus0 tag.
   with function Is_PCI (Window : Catalog.W.Window_ID) return Boolean;
   with procedure Read_Clock
     (Now : out Catalog.B.Tick; Available : out Boolean);
   with procedure Transfer
     (Virtual : Interfaces.Unsigned_64; Width : ACPI_PCI_Config_Reads.T.Access_Width;
      Value : out Interfaces.Unsigned_64; Completed : out Boolean);
package ACPI_Backend_PCI_Reads with SPARK_Mode is
   package P renames ACPI_PCI_Config_Reads;
   subtype Word is Interfaces.Unsigned_64;
   type Record_ID is new Word;
   No_Record : constant Record_ID := 0;
   type Epoch_Number is new Word;
   No_Epoch : constant Epoch_Number := 0;
   type Phase is (Fresh, Serving, Failed);
   type Outcome is (Configured, Ready, Done, Denied, Stale, Not_Ready,
     Already_Consumed, Invalid_Record, Catalog_Changed, Backing_Unavailable,
     Policy_Refused, Clock_Unavailable, Clock_Reversed, Deadline_Expired, Read_Failed, Reentered);
   type Context is limited private;
   function Status (State : Context) return Phase;
   -- Trusted startup only. Captures a named ID and already-retained mapped
   -- root; no IPC-supplied address/class/extent installs or broadens a record.
   procedure Configure
     (State : in out Context; Scope : Word; ID : Record_ID;
      Window : Catalog.W.Window_ID; Result : out Outcome);
   type Header_Report is record
      Status : Outcome := Not_Ready;
      Epoch : Epoch_Number := No_Epoch;
      ID : Record_ID := No_Record;
      First : Word := 0;
   end record;
   -- Physical base is metadata for bounding aliases, never a mapping or
   -- virtual address. It is populated only after fresh root inspection. The
   -- protocol fact P.Bus_Bytes, with P.Function_Bytes per function, bus0 only.
   procedure Header (State : in out Context; Scope : Word; Result : out Header_Report);
   procedure Read
     (State : in out Context; Scope : Word; Epoch : Epoch_Number; ID : Record_ID;
      Region : P.Region; Offset : Word; Width : P.T.Access_Width;
      Value : out Word; Result : out Outcome; Reason : out P.Outcome;
      Deadline : Catalog.B.Tick; For_Write : Boolean := False);
   -- No reset/remap/write/retry operation. Catalog/backing/transfer failure
   -- stops the context. Owners serialize access; callbacks return normally.
private
   type Context is limited record
      Stage : Phase := Fresh;
      Scope : Word := 0;
      ID : Record_ID := No_Record;
      Revision : Catalog.Revision_Number := Catalog.No_Revision;
      Window : Catalog.W.Window_ID := Catalog.W.No_Window;
      Base : Word := 0;
      Working : Boolean := False;
   end record;
end ACPI_Backend_PCI_Reads;
