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
   Faults_Seen : Unsigned_64 := 0;    --  the last DMA fault record taken
   Map_Logged : Boolean := False;
   IOMMU_Logged : Boolean := False;
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
   --  t=...ms" (Warning). Every logstore reader sees it, so no address or
   --  instruction: those are the kernel's restricted fault detail
   --  (Fault_Reports, LOG-004), which stays out of the store.
   procedure Log_Kill (Number : Unsigned_64; Item : Pub.Kill_Record) is
   begin
      Record_Line ("kernel: process killed #" & Decimal (Number) &
            " name=" & Trimmed (Item.Name) & " pid=" & Hex (Item.Victim) &
            " kind=" & Pub.Kill_Kind_Name (Item.Kind) &
            " vector=" & Decimal (Item.Vector) &
            " t=" & Decimal (Item.Time_Ms) & "ms; details restricted",
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

   function State_Field (Word : Unsigned_64; Shift : Natural) return Unsigned_64 is
     (Shift_Right (Word, Shift) and Pub.State_Field_Mask);

   --  "kernel: IOMMU active, units 1 of 1 translating" (Information) or
   --  "kernel: IOMMU degraded: DMA unconfined (no DMAR table), units 0 of
   --  0 translating" (Warning): the boot's DMA protection, never
   --  silent (IOMMU-001).
   procedure Log_IOMMU is
      Word : constant Unsigned_64 := Page.IOMMU;
      Status : constant Unsigned_64 := State_Field (Word, Pub.State_Status_Shift);
      Degraded : constant Boolean :=
        Status = Unsigned_64 (Pub.IOMMU_Status'Pos (Pub.Degraded));
   begin
      if Status = Unsigned_64 (Pub.IOMMU_Status'Pos (Pub.Not_Reported)) then
         return;
      end if;
      IOMMU_Logged := True;
      Record_Line ("kernel: IOMMU " & Pub.IOMMU_Status_Name (Status) &
            (if Degraded then
               ": DMA unconfined (" &
               Pub.IOMMU_Reason_Name (State_Field (Word, Pub.State_Reason_Shift)) & ")"
             else "") &
            ", units " & Decimal (State_Field (Word, Pub.State_Translating_Shift)) & " of " &
            Decimal (State_Field (Word, Pub.State_Units_Shift)) & " translating" &
            (if State_Field (Word, Pub.State_Graphics_Shift) /= 0
             then ", integrated GPU untranslated" else ""),
            (if Degraded then Logs.Warning else Logs.Information));
   end Log_IOMMU;

   --  "kernel: DMA fault #N requester 00:03.0 read 0x100000 (RAM)
   --  reason=not-mapped code=6 unit=1 t=...ms" (Warning): a device
   --  reached for memory its domain does not map (IOMMU-001).
   procedure Log_Fault (Number : Unsigned_64; Item : Pub.Fault_Record) is
      RID : constant Unsigned_64 := Item.Requester and 16#FFFF#;
      function Two (V : Unsigned_64) return String is
        ([Digits_Of (Natural (Shift_Right (V, 4) and 16#F#) + 1),
          Digits_Of (Natural (V and 16#F#) + 1)]);
      Code : constant Unsigned_64 := Shift_Right (Item.Detail, Pub.Detail_Code_Shift) and 16#FF#;
   begin
      Record_Line ("kernel: DMA fault #" & Decimal (Number) &
            " requester " & Two (Shift_Right (RID, 8)) & ":" &
            Two (Shift_Right (RID, 3) and 16#1F#) & "." & Decimal (RID and 7) &
            (if (Shift_Right (Item.Detail, Pub.Detail_Write_Shift) and 1) /= 0
             then " write " else " read ") &
            Hex (Item.Address) & " (" &
            Pub.Class_Name (Shift_Right (Item.Detail, Pub.Detail_Class_Shift) and 16#FF#) &
            ") reason=" & Pub.Fault_Reason_Name (Code) & " code=" & Decimal (Code) &
            " unit=" & Decimal (Shift_Right (Item.Detail, Pub.Detail_Unit_Shift) and 16#FF#) &
            " t=" & Decimal (Item.Time_Ms) & "ms",
            Logs.Warning);
   end Log_Fault;

   procedure Poll_Faults is
      Total_Word : Unsigned_64 with
        Import, Atomic, Address => Page.Fault_Total'Address;
      Total : constant Unsigned_64 := Total_Word;
      Lost  : Unsigned_64 := 0;
      First : Unsigned_64;
   begin
      if Total < Faults_Seen then
         Faults_Seen := 0;
      end if;
      if Total = Faults_Seen then
         return;
      end if;
      First := Pub.First_Fault_Available (Faults_Seen, Total);
      Lost := First - Faults_Seen - 1;
      for Number in First .. Total loop
         declare
            Slot : constant Pub.Fault_Slot := Pub.Fault_Slot_Of (Number);
            Sequence : Unsigned_64 with
              Import, Atomic, Address => Page.Faults (Slot).Sequence'Address;
            Item : Pub.Fault_Record;
         begin
            if Sequence = Number then
               Item := Page.Faults (Slot);
               if Sequence = Number then
                  Log_Fault (Number, Item);
               else
                  Lost := Lost + 1;
               end if;
            else
               Lost := Lost + 1;
            end if;
         end;
      end loop;
      Faults_Seen := Total;
      if Lost /= 0 then
         Record_Line ("kernel: DMA fault records lost:" & Unsigned_64'Image (Lost),
                      Logs.Warning);
      end if;
   end Poll_Faults;

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
      if not IOMMU_Logged then
         Log_IOMMU;
      end if;
      Poll_Kills;
      Poll_Faults;
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
