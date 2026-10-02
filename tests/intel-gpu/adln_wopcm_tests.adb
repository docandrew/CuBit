with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_WOPCM; use Intel_GPU_ADLN_WOPCM;
procedure ADLN_WOPCM_Tests is
   Item : Layout;
begin
   Item := Select_Layout (0, 0, 0, 0, 335_000);
   pragma Assert (Item.Valid and not Item.Locked);
   pragma Assert (Item.Capacity = 2_097_152 and Item.Base = 16_384);
   pragma Assert (Item.Bytes = 2_043_904 and Item.Pin_Bias = Item.Bytes);
   Item := Select_Layout (16#1F3001#, 16#4001#, 16#1F3001#, 16#4001#, 335_000);
   pragma Assert (Item.Valid and Item.Locked and Item.Bytes = 2_043_904);
   for Bad in 0 .. 7 loop
      case Bad is
         when 0 => Item := Select_Layout (1, 0, 1, 0, 335_000);
         when 1 => Item := Select_Layout (0, 1, 0, 1, 335_000);
         when 2 => Item := Select_Layout (0, 0, 1, 0, 335_000);
         when 3 => Item := Select_Layout (Unsigned_32'Last, 0, Unsigned_32'Last, 0, 335_000);
         when 4 => Item := Select_Layout (0, 0, 0, 0, 0);
         when 5 => Item := Select_Layout (0, 0, 0, 0, 2_097_152);
         when 6 => Item := Select_Layout (16#1F3001#, 16#4003#, 16#1F3001#, 16#4003#, 335_000);
         when 7 => Item := Select_Layout (16#7F3001#, 16#4001#, 16#7F3001#, 16#4001#, 335_000);
      end case;
      pragma Assert (not Item.Valid and Item.Capacity = 0 and Item.Pin_Bias = 0);
   end loop;
   Ada.Text_IO.Put_Line ("ADLN WOPCM layout PASS: default/locked, stability, lock agreement, capacity and firmware bounds");
end ADLN_WOPCM_Tests;
