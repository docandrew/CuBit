with DMA_Retirement_Steps;
with Interfaces; use Interfaces;
with Ada.Text_IO;
procedure DMA_Retirement_Steps_Test is
   Released, Freed : Natural := 0;
   procedure Release_Owner (Physical_Page, Owner : Unsigned_64) is
   begin
      pragma Assert (Owner = 7);
      pragma Assert (Physical_Page = 16#200000# + Unsigned_64 (Released) * 4096);
      Released := Released + 1;
   end Release_Owner;
   procedure Free_Block (Physical : Unsigned_64; Order : Natural) is
   begin
      pragma Assert (Physical = 16#200000# and Order = 9 and Released = 512);
      Freed := Freed + 1;
   end Free_Block;
   package R is new DMA_Retirement_Steps (Release_Owner, Free_Block);
   Complete : Boolean;
begin
   for Retained in Boolean loop
      declare
         Position : R.Cursor;
         Item : R.Allocation := (16#200000#, 7, 42, 9, Retained);
      begin
         Released := 0; Freed := 0;
         R.Step (Item, Position, False, Complete);
         pragma Assert (not Complete and Released = 0 and Freed = 0);
         pragma Assert (not R.Rejected (Position));
         for I in 1 .. 8 loop
            R.Step (Item, Position, True, Complete);
            pragma Assert (Released = I * 64 and Complete = (I = 8));
         end loop;
         pragma Assert (Freed = (if Retained then 0 else 1));
         R.Step (Item, Position, True, Complete);
         pragma Assert (Complete and Released = 512);
         pragma Assert (Freed = (if Retained then 0 else 1));
         Item.Generation := 43;
         R.Step (Item, Position, True, Complete);
         pragma Assert (not Complete and Released = 512);
         pragma Assert (R.Rejected (Position));
         Item.Generation := 42;
         for Retry in 1 .. 128 loop
            R.Step (Item, Position, True, Complete);
            pragma Assert (not Complete and R.Rejected (Position) and Released = 512);
         end loop;
      end;
   end loop;
   declare
      Position : R.Cursor;
      Invalid : R.Allocation := (16#200000#, 7, 0, 9, True);
   begin
      Released := 0; Freed := 0;
      R.Step (Invalid, Position, True, Complete);
      pragma Assert (R.Rejected (Position) and not Complete and Released = 0 and Freed = 0);
      Invalid.Generation := 42;
      R.Step (Invalid, Position, True, Complete);
      pragma Assert (R.Rejected (Position) and not Complete and Released = 0 and Freed = 0);
   end;
   Ada.Text_IO.Put_Line ("PASS bounded DMA cleanup: grant/CPU gate,64pages/step, retained never freed, ordinary freed once, changed owner generation rejected");
end DMA_Retirement_Steps_Test;
