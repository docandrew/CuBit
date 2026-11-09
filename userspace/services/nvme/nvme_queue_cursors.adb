------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body NVMe_Queue_Cursors with SPARK_Mode is

   function Advance (Position : Cursor) return Cursor is
   begin
      if Position.Head = Queue_Index'Last then
         return (Head => 0, Phase => Position.Phase + 1);
      else
         return (Head => Position.Head + 1, Phase => Position.Phase);
      end if;
   end Advance;

   procedure Lemma_Advance_Consumes_One (Consumed : Consumed_Count) is
   begin
      if Consumed mod Queue_Depth = Queue_Index'Last then
         pragma Assert ((Consumed + 1) mod Queue_Depth = 0);
         pragma Assert ((Consumed + 1) / Queue_Depth = Consumed / Queue_Depth + 1);
      else
         pragma Assert ((Consumed + 1) mod Queue_Depth = Consumed mod Queue_Depth + 1);
         pragma Assert ((Consumed + 1) / Queue_Depth = Consumed / Queue_Depth);
      end if;
   end Lemma_Advance_Consumes_One;

   procedure Lemma_Pass_Flips_Phase (Consumed : Consumed_Count) is
   begin
      pragma Assert ((Consumed + Queue_Depth) mod Queue_Depth = Consumed mod Queue_Depth);
      pragma Assert ((Consumed + Queue_Depth) / Queue_Depth = Consumed / Queue_Depth + 1);
   end Lemma_Pass_Flips_Phase;

   procedure Lemma_Room (Tail, Head : Queue_Index) is null;

end NVMe_Queue_Cursors;
