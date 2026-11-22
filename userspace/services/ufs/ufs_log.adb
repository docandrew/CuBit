------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CuBit.Logging;
with CuBit.Log_Records;
with CuBit.Messages;
with Startup;
with UFS_Text; use UFS_Text;

package body UFS_Log is
   Prefix : constant String := "ufs: ";

   --  The last stage that passed, if any.
   Reached : Natural := 0;

   function Name (Item : Stage) return String is
     (case Item is
        when Identify          => "identify",
        when Enable            => "enable",
        when Link_Up           => "link-up",
        when Lists             => "lists",
        when NOP               => "nop",
        when Device_Init       => "device-init",
        when Device_Descriptor => "device-desc",
        when Geometry          => "geometry",
        when LUN               => "lun",
        when Unit_Ready        => "unit-ready",
        when Inquiry           => "inquiry",
        when Capacity          => "capacity",
        when Partitions        => "partitions");

   --  Optional in the manifest: without logstore, serial is all there is.
   Publisher : CuBit.Logging.Publisher :=
     CuBit.Logging.Bound_To (Startup.Logstore);

   procedure Line
     (Text : String;
      Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information)
   is
      Made : constant CuBit.Log_Records.Decoded :=
        CuBit.Log_Records.Make (Prefix & Text, Level);
      Submitted : Boolean;
   begin
      CuBit.Messages.debugPrint (Prefix & Text & ASCII.LF);
      if Made.Success then
         CuBit.Logging.Emit (Publisher, Made.Value, Submitted);
      end if;
   end Line;

   procedure Passed (Item : Stage; Details : String := "") is
   begin
      Reached := Natural'Max (Reached, Stage'Pos (Item) + 1);
      Line ("stage " & Name (Item) & " ok" & Details);
   end Passed;

   procedure Failed (Item : Stage; Reason : String; Details : String := "") is
   begin
      Line ("stage " & Name (Item) & " fail " & Reason & Details,
            CuBit.Log_Records.Warning);
   end Failed;

   procedure Summary (Serving : String) is
      Total : constant Unsigned_64 := Stage'Pos (Stage'Last) + 1;
   begin
      if Reached = Natural (Total) then
         Line ("bring-up complete (" & Decimal (Total) & "/" & Decimal (Total) &
               ")" & Serving);
      elsif Reached = 0 then
         Line ("bring-up reached nothing (0/" & Decimal (Total) & ")" & Serving,
               CuBit.Log_Records.Warning);
      else
         Line ("bring-up reached " & Name (Stage'Val (Reached - 1)) & " (" &
               Decimal (Unsigned_64 (Reached)) & "/" & Decimal (Total) & ")" &
               Serving, CuBit.Log_Records.Warning);
      end if;
   end Summary;

   function Hex_Field (Key : String; Value : Unsigned_64; Width : Positive)
     return String is
     (" " & Key & "=0x" & Hex (Value, Hex_Width'Min (Width, Hex_Digits)));

   function Field (Key : String; Value : Unsigned_64) return String is
     (" " & Key & "=" & Decimal (Value));

   procedure Note (Text : String) is
   begin
      Line (Text);
   end Note;
end UFS_Log;
