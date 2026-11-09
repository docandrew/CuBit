------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

procedure Kernel_Log_Publication.Drain
  (Seen      : in out Unsigned_64;
   Budget    : Record_Capacity;
   Accounted : out Unsigned_64;
   More      : out Boolean)
with SPARK_Mode
is
   H : constant Header := Load_Header;
   Start : constant Unsigned_64 := Seen;
   Total, First, Last : Record_Number;
   --  Of the numbers First .. Last looked at so far: delivered, in dropped
   --  runs already reported, and in the run still open (the Run_Length
   --  numbers before the current one). At most Budget each.
   Delivered, Reported_Here, Run_Length : Unsigned_64 := 0;
   --  The numbers before First: overwritten before this call.
   Reported_Before : Unsigned_64;
   Item  : Log_Record;
   Taken : Boolean;
begin
   Accounted := 0;
   More := False;
   if not Valid_Header (H) or else H.Total <= Seen then
      return;
   end if;
   Total := H.Total;
   First := First_Available (Seen, Total, H.Capacity);
   if First > Seen + 1 then
      Dropped (Seen + 1, First - 1);
   end if;
   Reported_Before := First - 1 - Start;
   Last := (if Total - First >= Budget then First + (Budget - 1) else Total);
   More := Last < Total;
   pragma Assert (First <= Last and then Last - First < Budget);
   for Offset in 0 .. Last - First loop
      declare
         N     : constant Record_Number := First + Offset;
         Where : constant Slot := Slot_Of (N, H.Capacity);
      begin
         Taken := False;
         if Load_Sequence (Where) = N then
            Item := Load_Record (Where);
            Taken := Item.Sequence = N and then Load_Sequence (Where) = N;
         end if;
         if Taken then
            if Run_Length > 0 then
               Dropped (N - Run_Length, N - 1);
               Reported_Here := Reported_Here + Run_Length;
               Run_Length := 0;
            end if;
            Deliver (N, Item);
            Delivered := Delivered + 1;
         else
            Run_Length := Run_Length + 1;
         end if;
      end;
      --  Each number looked at is delivered, in a reported run, or in the
      --  open run; the open run ends at this number.
      pragma Loop_Invariant (Run_Length <= Offset + 1);
      pragma Loop_Invariant (Delivered <= Offset + 1 and then Reported_Here <= Offset + 1);
      pragma Loop_Invariant (Delivered + Reported_Here + Run_Length = Offset + 1);
   end loop;
   if Run_Length > 0 then
      Dropped (Last - (Run_Length - 1), Last);
      Reported_Here := Reported_Here + Run_Length;
   end if;
   pragma Assert (Delivered + Reported_Here = Last - First + 1);
   Seen := Last;
   Accounted := Reported_Before + Delivered + Reported_Here;
end Kernel_Log_Publication.Drain;
