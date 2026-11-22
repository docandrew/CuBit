with System;
with CuBit.Block_Requests; use CuBit.Block_Requests;
package body Test_Device is
   function Offset (Item : Transfer_Request) return Positive is
     (Natural (Item.First_Block) * Block_Bytes + 1);

   procedure Post (Item : Slot; Work : Work_Order) is
   begin
      pragma Assert (not Posted (Item));
      Posts := Posts + 1;
      Posted (Item) := True;
      Work_Of (Item) := Work;
      Done (Item) := Instant;
      Cancelled (Item) := False;
      if Work.Kind = Block_Transfer
        and then Work.Granted.Item.Direction = Device_Write
      then
         declare
            Count : constant Positive := Positive (Bytes (Work.Granted.Item));
            Source : Bytes_View (1 .. Count) with Import, Address => Work.Granted.Buffer;
         begin
            Disk (Offset (Work.Granted.Item) .. Offset (Work.Granted.Item) + Count - 1)
              := Source;
         end;
      end if;
   end Post;

   function Finished (Item : Slot) return Boolean is (Done (Item));

   procedure Collect (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome) is
   begin
      Collects := Collects + 1;
      Posted (Item) := False;
      if Work.Kind = Cache_Flush then
         Outcome := (Passed => not Fail (Item), Moved => 0);
         return;
      end if;
      declare
         Count : constant Positive := Positive (Bytes (Work.Granted.Item));
         Target : Bytes_View (1 .. Count) with Import, Address => Work.Granted.Buffer;
      begin
         --  A short read copies nothing; a short write reports its prefix;
         --  a failed command claims everything (the driver must not trust it).
         Outcome := (Passed => not Fail (Item),
                     Moved => Unsigned_64
                       (if not Short (Item) then Count
                        elsif Work.Granted.Item.Direction = Device_Read then 0
                        else Count - Block_Bytes));
         if Outcome.Passed and then not Short (Item)
           and then Work.Granted.Item.Direction = Device_Read
         then
            Target := Disk (Offset (Work.Granted.Item) .. Offset (Work.Granted.Item) + Count - 1);
         end if;
      end;
   end Collect;

   procedure Cancel (Item : Slot) is
   begin
      Cancelled (Item) := True;
      Posted (Item) := False;
   end Cancel;

   procedure Note (Event : Driver_Event; Count : Unsigned_64) is
   begin
      Notes (Event) := Notes (Event) + 1;
      Last_Count (Event) := Count;
   end Note;
end Test_Device;
