pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with Device_Memory_Publication;

with CuBit.Messages;

package body Kernel_Device_Memory is
   package Pub renames Device_Memory_Publication;
   package Logs renames CuBit.Log_Records;

   procedure Record_Line (Text : String; Level : Logs.Severity) is
   begin
      CuBit.Messages.debugPrint (Text & ASCII.LF);
      Note (Text, Level);
   end Record_Line;

   Page : Pub.Diagnostics_Page with
     Import, Volatile, Address => System'To_Address (Pub.Page_Address);
   Total_Word : Unsigned_64 with
     Import, Atomic, Address => Page.Total'Address;

   Seen       : Unsigned_64 := 0;     --  the last record taken
   Kills_Seen : Unsigned_64 := 0;     --  the last kill record taken
   Map_Logged : Boolean := False;
   Spans_Per_Line : constant := 3;

   Digits_Of : constant String := "0123456789ABCDEF";
   function Hex (Value : Unsigned_64) return String is
      Text : String (1 .. 16);
      V : Unsigned_64 := Value;
      First : Natural := Text'Last;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digits_Of (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
         if Text (I) /= '0' then First := I; end if;
      end loop;
      return "0x" & Text (First .. Text'Last);
   end Hex;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);   --  without the space
   end Decimal;

   function Trimmed (Name : Pub.Process_Name) return String is
      Last : Natural := 0;
   begin
      for I in Name'Range loop
         exit when Name (I) = ASCII.NUL;
         if Name (I) /= ' ' then Last := I; end if;
      end loop;
      return Name (Name'First .. Last);
   end Trimmed;

   procedure Log_Map is
      Count : constant Unsigned_64 := Page.Span_Count;
      Shown : constant Natural :=
        Natural (Unsigned_64'Min (Count, Pub.Span_Capacity));
      First : Natural := 1;
   begin
      Record_Line ("kernel: physical map:" & Unsigned_64'Image (Count) & " spans" &
            (if Page.Complete = 1 then "" else ", INCOMPLETE (no device memory)") &
            (if Natural (Unsigned_64'Min (Count, Unsigned_64 (Natural'Last))) > Shown
             then ", first" & Natural'Image (Shown) & " shown" else ""),
            Logs.Information);
      for I in 1 .. Natural (Unsigned_64'Min (Page.Root_Count, Pub.Root_Capacity)) loop
         Record_Line ("kernel: physical map root" & Natural'Image (I) & ": " &
               Hex (Page.Roots (I).First) & "-" & Hex (Page.Roots (I).Last) &
               " (devmgr device memory)", Logs.Information);
      end loop;
      while First <= Shown loop
         declare
            Line : String (1 .. 200);
            Used : Natural := 0;
            procedure Add (Text : String) is
            begin
               if Used + Text'Length <= Line'Length then
                  Line (Used + 1 .. Used + Text'Length) := Text;
                  Used := Used + Text'Length;
               end if;
            end Add;
         begin
            for I in First .. Natural'Min (Shown, First + Spans_Per_Line - 1) loop
               Add ((if I = First then "" else ", ") &
                    Hex (Page.Spans (I).First) & "-" & Hex (Page.Spans (I).Last) &
                    " " & Pub.Class_Name (Page.Spans (I).Class));
            end loop;
            Record_Line ("kernel: physical map" & Natural'Image (First) & ": " & Line (1 .. Used),
                  Logs.Information);
         end;
         First := First + Spans_Per_Line;
      end loop;
   end Log_Map;

   procedure Log_Denial (Number : Unsigned_64; Item : Pub.Denial_Record) is
      use type Pub.Reason;
      Blocked : constant Boolean :=
        Item.Why = Unsigned_64 (Pub.Reason'Pos (Pub.Blocked_Page));
   begin
      --  "#N" is the kernel's record number: a reader that has seen #N has
      --  every record the kernel counted up to N (System facts).
      Record_Line ("kernel: device-memory denied #" & Decimal (Number) &
            " " & Trimmed (Item.Name) &
            " (identity " & Hex (Item.Caller) & ") " &
            Pub.Operation_Name (Item.Request) & " " & Hex (Item.Base) &
            " +" & Hex (Item.Bytes) & " " & Pub.Mode_Name (Item.Mode) & ": " &
            Pub.Reason_Name (Item.Why) &
            (if Blocked then " at " & Hex (Item.Page) & " (" &
               Pub.Class_Name (Item.Class) & ")" else ""),
            Logs.Warning);
   end Log_Denial;

   --  "kernel: process killed #N name=... pid=... kind=... vector=...
   --  addr=... rip=... t=...ms" (Warning).
   procedure Log_Kill (Number : Unsigned_64; Item : Pub.Kill_Record) is
   begin
      Record_Line ("kernel: process killed #" & Decimal (Number) &
            " name=" & Trimmed (Item.Name) & " pid=" & Hex (Item.Victim) &
            " kind=" & Pub.Kill_Kind_Name (Item.Kind) &
            " vector=" & Decimal (Item.Vector) &
            " addr=" & Hex (Item.Address) & " rip=" & Hex (Item.RIP) &
            " t=" & Decimal (Item.Time_Ms) & "ms",
            Logs.Warning);
   end Log_Kill;

   procedure Poll_Kills is
      Total_Word : Unsigned_64 with
        Import, Atomic, Address => Page.Kill_Total'Address;
      Total : constant Unsigned_64 := Total_Word;
      Lost  : Unsigned_64 := 0;
      First : Unsigned_64;
   begin
      if Total < Kills_Seen then
         Kills_Seen := 0;
      end if;
      if Total = Kills_Seen then
         return;
      end if;
      First := Pub.First_Kill_Available (Kills_Seen, Total);
      Lost := First - Kills_Seen - 1;
      for Number in First .. Total loop
         declare
            Slot : constant Pub.Kill_Slot := Pub.Kill_Slot_Of (Number);
            Sequence : Unsigned_64 with
              Import, Atomic, Address => Page.Kills (Slot).Sequence'Address;
            Item : Pub.Kill_Record;
         begin
            if Sequence = Number then
               Item := Page.Kills (Slot);
               if Sequence = Number then
                  Log_Kill (Number, Item);
               else
                  Lost := Lost + 1;
               end if;
            else
               Lost := Lost + 1;
            end if;
         end;
      end loop;
      Kills_Seen := Total;
      if Lost /= 0 then
         Record_Line ("kernel: process killed records lost:" & Unsigned_64'Image (Lost),
                      Logs.Warning);
      end if;
   end Poll_Kills;

   procedure Poll is
      Total : Unsigned_64;
      First : Unsigned_64;
      Lost  : Unsigned_64 := 0;
   begin
      if Page.Version /= Pub.Layout_Version then
         return;
      end if;
      if not Map_Logged then
         Map_Logged := True;
         Log_Map;
      end if;
      Poll_Kills;
      Total := Total_Word;
      if Total < Seen then
         Seen := 0;
      end if;
      if Total = Seen then
         return;
      end if;
      First := Pub.First_Available (Seen, Total);
      Lost := First - Seen - 1;
      for Number in First .. Total loop
         declare
            Slot : constant Pub.Denial_Slot := Pub.Slot_Of (Number);
            Sequence : Unsigned_64 with
              Import, Atomic, Address => Page.Denials (Slot).Sequence'Address;
            Item : Pub.Denial_Record;
         begin
            if Sequence = Number then
               Item := Page.Denials (Slot);
               if Sequence = Number then
                  Log_Denial (Number, Item);
               else
                  Lost := Lost + 1;   --  overwritten while copied
               end if;
            else
               Lost := Lost + 1;
            end if;
         end;
      end loop;
      Seen := Total;
      if Lost /= 0 then
         Record_Line ("kernel: device-memory denied records lost:" & Unsigned_64'Image (Lost),
               Logs.Warning);
      end if;
   end Poll;
end Kernel_Device_Memory;
