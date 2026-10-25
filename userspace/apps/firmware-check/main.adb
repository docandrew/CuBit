------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  FIRMWARE_SNAPSHOT regression (Firmware_Snapshot_ABI): lists the sealed
--  boot snapshot's catalog, describes every table, copies each whole in
--  Chunk_Max pieces, and checks every table's checksum, the FADT and DSDT,
--  the catalog's totals, end-of-table and the typed refusals.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Firmware_Snapshot; use CuBit.Firmware_Snapshot;
with ACPI_Protocol;
with Startup;

procedure Main is
   use ASCII;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         debugPrint ("firmware-check: " & Name & " PASS" & LF);
      else
         Failures := Failures + 1;
         debugPrint ("firmware-check: " & Name & " FAIL" & LF);
      end if;
   end Check;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Text (Signature : Signature_Bytes) return String is
     ([for I in Signature_Index => Character'Val (Signature (I))]);

   --  ACPI 6.5, 5.2.6 (system description table header): Signature (4),
   --  Length (4, little-endian), Revision (1), Checksum (1); all bytes of
   --  the table sum to zero modulo 256.
   Length_First  : constant := 5;
   Length_Bytes  : constant := 4;
   Revision_Byte : constant := 9;
   Header_Bytes  : constant := 36;
   FADT : constant String := "FACP";
   DSDT : constant String := "DSDT";
   FACS : constant String := "FACS";

   Tables : constant Authority := Startup.Tables;
   Catalog : Catalog_Info;
   Status : Outcome;
   Seen_FADT, Seen_DSDT, Seen_FACS : Boolean := False;
   Largest, Total : Byte_Count := 0;
   Chunk : Byte_Array (1 .. Chunk_Max);
   Empty : Byte_Array (1 .. 0);
   Copied : Byte_Count;
begin
   debugPrint ("firmware-check: starting" & LF);
   Info (Tables, Catalog, Status);
   Check (Status = Ok, "catalog info");
   Check (Catalog.Ready and then Catalog.Table_Count > 0, "snapshot ready with tables");
   debugPrint ("firmware-check: catalog tables=" & Image (Unsigned_64 (Catalog.Table_Count)) &
               " largest=" & Image (Unsigned_64 (Catalog.Largest_Table)) &
               " total=" & Image (Unsigned_64 (Catalog.Total_Bytes)) & LF);

   for T in 1 .. Table_ID (Catalog.Table_Count) loop
      declare
         D : Table_Description;
         Offset : Byte_Count := 0;
         Sum : Unsigned_8 := 0;
         Header_Length : Unsigned_64 := 0;
         Header_Ok : Boolean := True;
         Steps : Natural := 0;
      begin
         Describe (Tables, T, D, Status);
         Check (Status = Ok, "describe table " & Image (Unsigned_64 (T)));
         Seen_FADT := Seen_FADT or else Text (D.Signature) = FADT;
         Seen_DSDT := Seen_DSDT or else Text (D.Signature) = DSDT;
         Seen_FACS := Seen_FACS or else Text (D.Signature) = FACS;
         Largest := Byte_Count'Max (Largest, D.Extent);
         Total := Total + D.Extent;
         --  The whole table, Chunk_Max at a time, from its described extent.
         while Status = Ok and then Offset < D.Extent loop
            Copy_Part (Tables, T, Offset, Chunk, Copied, Status);
            exit when Status /= Ok or else Copied = 0;
            for I in 1 .. Natural (Copied) loop
               declare
                  At_Byte : constant Byte_Count := Offset + Byte_Count (I);
               begin
                  Sum := Sum + Chunk (I);
                  if At_Byte <= Signature_Length then
                     Header_Ok := Header_Ok and then Chunk (I) = D.Signature (Natural (At_Byte));
                  elsif At_Byte in Length_First .. Length_First + Length_Bytes - 1 then
                     Header_Length := Header_Length or
                       Shift_Left (Unsigned_64 (Chunk (I)), 8 * Natural (At_Byte - Length_First));
                  elsif At_Byte = Revision_Byte then
                     Header_Ok := Header_Ok and then Chunk (I) = D.Revision;
                  end if;
               end;
            end loop;
            --  Past the table's bytes the chunk is zero.
            Header_Ok := Header_Ok and then
              (for all I in Natural (Copied) + 1 .. Chunk'Last => Chunk (I) = 0);
            Offset := Offset + Copied;
            Steps := Steps + 1;
         end loop;
         Check (Status = Ok and then Offset = D.Extent, "copy table " & Text (D.Signature) & " whole");
         Check (Steps = Natural ((D.Extent + Chunk_Max - 1) / Chunk_Max), "chunk count " & Text (D.Signature));
         Check (D.Extent >= Header_Bytes and then Header_Length = Unsigned_64 (D.Extent) and then Header_Ok,
                "header of " & Text (D.Signature) & " matches its description");
         Check (Sum = 0, "checksum " & Text (D.Signature));
         debugPrint ("firmware-check: table " & Image (Unsigned_64 (T)) & " " & Text (D.Signature) &
                     " revision=" & Image (Unsigned_64 (D.Revision)) &
                     " length=" & Image (Unsigned_64 (D.Extent)) & LF);
         --  End of the table: Ok with nothing copied, and a zero chunk.
         Copy_Part (Tables, T, D.Extent, Chunk, Copied, Status);
         Check (Status = Ok and then Copied = 0 and then (for all B of Chunk => B = 0),
                "end of " & Text (D.Signature));
         Copy_Part (Tables, T, D.Extent + 1, Chunk, Copied, Status);
         Check (Status = Beyond_End and then Copied = 0, "beyond end of " & Text (D.Signature));
         Copy_Part (Tables, T, 0, Empty, Copied, Status);
         Check (Status = Empty_Before_End, "empty before end of " & Text (D.Signature));
      end;
   end loop;
   Check (Seen_FADT, "FADT present");
   Check (Seen_DSDT, "DSDT present");
   Check (not Seen_FACS, "FACS excluded");
   Check (Largest = Catalog.Largest_Table and then Total = Catalog.Total_Bytes, "catalog totals");

   declare
      D : Table_Description;
      Past : constant Table_ID := Table_ID (Catalog.Table_Count) + 1;
   begin
      Describe (Tables, Past, D, Status);
      Check (Status = Unknown_Table, "unknown table refused");
      Describe (Tables, 0, D, Status);
      Check (Status = Unknown_Table, "table zero refused");
      Copy_Part (Tables, Past, 0, Chunk, Copied, Status);
      Check (Status = Unknown_Table and then Copied = 0, "copy of an unknown table refused");
      Copy_Part (Tables, 1, Byte_Count'Last, Chunk, Copied, Status);
      Check (Status = Beyond_End, "full-width offset refused");
   end;

   --  The declared "acpi" client endpoint carries the observer badge
   --  (procmgr mints it; acpi-standin.app reports the stamped tag).
   declare
      Probe_Label : constant Unsigned_32 := 16#AC01#;   --  acpi-standin's probe
      REPLY_OK    : constant Unsigned_32 := 16#F000#;
      Probe_Deadline_Ms : constant := 2_000;
      Probe : Message := NULL_MESSAGE;
   begin
      Probe.tag.label := Probe_Label;
      Probe.tag := capCall (Startup.Acpi, Probe, Deadline_After (Probe_Deadline_Ms));
      Check (Probe.tag.label = REPLY_OK
             and then ACPI_Protocol.Is_Observer (ACPI_Protocol.Role_Tag (Probe.words (0))),
             "acpi endpoint carries the observer badge");
   end;

   if Failures = 0 then
      debugPrint ("TEST: PASS firmware-snapshot" & LF);
   else
      debugPrint ("TEST: FAIL firmware-snapshot" & Natural'Image (Failures) & LF);
   end if;
end Main;
