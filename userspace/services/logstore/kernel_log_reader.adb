pragma Ada_2022;
with System;
with Fault_Reports;
with Kernel_Log_Publication;
with Kernel_Log_Publication.Drain;
with CuBit.Messages;

package body Kernel_Log_Reader is
   package KLP renames Kernel_Log_Publication;
   package FR renames Fault_Reports;
   package Logs renames CuBit.Log_Records;

   Area : KLP.Ring_Area with
     Import, Volatile, Address => System'To_Address (KLP.Page_Address);

   function Load_Header return KLP.Header is (Area.Head);

   function Load_Sequence (Where : KLP.Slot) return Unsigned_64 is
      Word : Unsigned_64 with Import, Atomic, Address => Area.Records (Where).Sequence'Address;
   begin
      return Word;
   end Load_Sequence;

   function Load_Record (Where : KLP.Slot) return KLP.Log_Record is (Area.Records (Where));

   Seen : Unsigned_64 := 0;           --  every record up to Seen accounted for
   Taken, Lost : Unsigned_64 := 0;    --  records published, records dropped
   Earliest : Unsigned_64 := Unsigned_64'Last;
   Started : Unsigned_64 := 0;
   Summarized : Boolean := False;
   Poll_Ms : Unsigned_64 := 0;

   --  A dropped run waiting for the record after it (it takes that time).
   Pending_Drop : Boolean := False;
   Drop_First, Drop_Last : Unsigned_64 := 0;

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   function Level_Of (L : KLP.Level) return Logs.Severity is
     (Logs.Severity'Val (KLP.Level'Pos (L)));

   procedure Flush_Drop (Time_Ms : Unsigned_64) is
   begin
      if Pending_Drop then
         Pending_Drop := False;
         Publish (KLP.Kernel_Source, Time_Ms, Logs.Warning,
                  "kernel log: " & Decimal (Drop_Last - Drop_First + 1) &
                  " records dropped (sequence " & Decimal (Drop_First) & ".." &
                  Decimal (Drop_Last) & ")");
      end if;
   end Flush_Drop;

   procedure Dropped (First, Last : KLP.Record_Number) is
   begin
      --  Runs come in order. One that continues the pending run (the
      --  overwritten records, then one overwritten while it was copied)
      --  joins it; otherwise the pending one is published first.
      if Pending_Drop and then Drop_Last < Unsigned_64'Last and then First = Drop_Last + 1 then
         Drop_Last := Last;
      else
         Flush_Drop (Poll_Ms);
         Pending_Drop := True;
         Drop_First := First;
         Drop_Last := Last;
      end if;
      Lost := Lost + (Last - First + 1);
   end Dropped;

   function Hex (Value : Unsigned_64) return String is
      Digits_Of : constant String := "0123456789abcdef";
      Text : String (1 .. 16);
      V : Unsigned_64 := Value;
      First : Natural := Text'Last;
   begin
      for I in reverse Text'Range loop
         Text (I) := Digits_Of (Natural (V and 16#F#) + 1);
         V := Shift_Right (V, 4);
         if Text (I) /= '0' then
            First := I;
         end if;
      end loop;
      return "0x" & Text (First .. Text'Last);
   end Hex;

   --  What this reader may see (Kernel_Log_Publication.Audience). The
   --  store has no reader that may see more: the log-observer badge lets
   --  its holder read every record, so a diagnostics-only record never
   --  enters it (LOG-004).
   Store_Audience : constant KLP.Audience := KLP.Everyone;

   --  The summary every reader of the process's logs sees: "stopped by a
   --  page fault (write); details restricted". No address, offset or
   --  register.
   function Summary_Text (R : FR.Report) return String is
      use type FR.Fault_Kind;
      use type FR.Access_Kind;
      Kind : constant FR.Fault_Kind := FR.Kind_Of (R);
   begin
      case Kind is
         when FR.Stopped_By_Process =>
            return "stopped by " &
              (if R.Stopper = 0 then "an unknown process"
               else Who (R.Stopper, "pid " & Hex (R.Stopper)));
         when FR.Out_Of_Memory =>
            return "stopped: out of memory; details restricted";
         when FR.Page_Fault =>
            return "stopped by a page fault" &
              (if FR.Operation_Of (R) /= FR.No_Access
               then " (" & FR.Access_Name (FR.Operation_Of (R)) & ")" else "") &
              "; details restricted";
         when others =>
            return "stopped by a CPU exception: " & FR.Kind_Name (Kind) & "; details restricted";
      end case;
   end Summary_Text;

   --  The summary as a record: its text, and its fields (kind, access,
   --  mode, who stopped it) for readers that render it themselves.
   procedure Publish_Stop (Source, Time_Ms : Unsigned_64; Name : String; R : FR.Report) is
      use type FR.Fault_Kind;
      Text : constant String := Who (Source, Name) & ": " & Summary_Text (R);
      Made : Logs.Decoded := Logs.Make (Text, Logs.Error);
      procedure Add (Field : String; Value : Unsigned_64) is
      begin
         if Made.Success then
            declare
               Next : constant Logs.Decoded :=
                 Logs.With_Field (Made.Value, Field, Logs.Unsigned_Integer, Value);
            begin
               if Next.Success then
                  Made := Next;
               end if;
            end;
         end if;
      end Add;
   begin
      Add ("fault.kind", Unsigned_64 (R.Kind));
      Add ("fault.access", Unsigned_64 (R.Operation));
      Add ("fault.mode", Unsigned_64 (R.Mode));
      if FR.Kind_Of (R) = FR.Stopped_By_Process then
         Add ("fault.stopper", R.Stopper);
      end if;
      if Made.Success then
         Publish_Record (Source, Time_Ms, Made.Value);
      end if;
   end Publish_Stop;

   Withheld : Unsigned_64 := 0;   --  restricted records kept out of the store

   procedure Deliver (Number : KLP.Record_Number; Item : KLP.Log_Record) is
      pragma Unreferenced (Number);
      Stopped : constant Boolean :=
        KLP."=" (KLP.Origin_Of (Item), KLP.Process_Stopped);
   begin
      Flush_Drop (Item.Time_Ms);
      if not KLP.Visible (Item, Store_Audience) then
         --  A fault's detail: it stays in the kernel ring. The console (the
         --  machine's own screen, where the kernel printed the raw fault)
         --  is told that it exists and what it is about, never its values.
         Withheld := Withheld + 1;
         CuBit.Messages.debugPrint
           ("logstore: kernel log: withheld the restricted fault detail of " &
            Who (Item.Source, KLP.Name_Of (Item)) & " (" &
            FR.Where_Name (FR.Where_Of (Item.Fault)) & ")" & ASCII.LF);
      elsif Stopped then
         --  Under the stopped process's own source.
         Learn (Item.Source, KLP.Name_Of (Item));
         Publish_Stop (Item.Source, Item.Time_Ms, KLP.Name_Of (Item), FR.Summary_Of (Item.Fault));
      else
         Publish (KLP.Kernel_Source, Item.Time_Ms, Level_Of (KLP.Level_Of (Item)),
                  KLP.Text_Of (Item) & (if KLP.Truncated (Item) then " [truncated]" else ""));
      end if;
      Taken := Taken + 1;
      Earliest := Unsigned_64'Min (Earliest, Item.Time_Ms);
   end Deliver;

   procedure Drain is new KLP.Drain
     (Load_Header, Load_Sequence, Load_Record, Deliver, Dropped);

   procedure Set_Started (Started_Ms : Unsigned_64) is
   begin
      Started := Started_Ms;
   end Set_Started;

   --  Records the kernel gave up unnumbered (Header.Reentered), reported.
   Reentered_Seen : Unsigned_64 := 0;

   procedure Poll (Now_Ms : Unsigned_64; More : out Boolean) is
      Accounted : Unsigned_64;
      Reentered : Unsigned_64 with
        Import, Atomic, Address => Area.Head.Reentered'Address;
      Given_Up : constant Unsigned_64 := Reentered;
   begin
      Poll_Ms := Now_Ms;
      Drain (Seen, Budget, Accounted, More);
      if Given_Up > Reentered_Seen then
         Publish (KLP.Kernel_Source, Now_Ms, Logs.Warning,
                  "kernel log: " & Decimal (Given_Up - Reentered_Seen) &
                  " records given up unnumbered (a CPU logged while it was writing a record)");
         Reentered_Seen := Given_Up;
      end if;
      if not More then
         Flush_Drop (Now_Ms);
      end if;
      if not More and then not Summarized and then Seen > 0 then
         Summarized := True;
         CuBit.Messages.debugPrint
           ("logstore: kernel log caught up: " & Decimal (Taken) & " records taken, " &
            Decimal (Lost) & " dropped, through sequence " & Decimal (Seen) &
            "; earliest t=" & Decimal (Earliest) & "ms, logstore started t=" &
            Decimal (Started) & "ms; " & Decimal (Withheld) & " restricted withheld" & ASCII.LF);
      end if;
   end Poll;
end Kernel_Log_Reader;
