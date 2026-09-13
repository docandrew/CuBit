pragma Ada_2022;
package body Boot_Frame_Allocator with SPARK_Mode is
   procedure Initialize (This : out State) is
   begin
      This := (Available => [others => False], High_Water => 0);
   end Initialize;

   procedure Admit (This : in out State; Item : Payload_Frame) is
   begin
      This.Available (Item) := True;
   end Admit;

   procedure Reserve
     (This : in out State; Size : Request_Size; First : out Frame)
   is
      Run : Frame := 0;
   begin
      First := 0;
      for Item in Payload_Frame loop
         pragma Loop_Invariant (Run < Size and then Run < Item);
         pragma Loop_Invariant
           (for all Prior in Item - Run .. Item - 1 =>
              This.Available (Prior));
         if This.Available (Item) then
            Run := Run + 1;
            if Run = Size then
               First := Item - (Size - 1);
               for Claimed in First .. Item loop
                  This.Available (Claimed) := False;
                  pragma Loop_Invariant
                    (for all F in Frame =>
                       (if F in First .. Claimed then
                          not This.Available (F)
                        else This.Available (F) = This.Available'Loop_Entry (F)));
               end loop;
               This.High_Water := Frame'Max (This.High_Water, Item);
               return;
            end if;
         else
            Run := 0;
         end if;
      end loop;
   end Reserve;

   function Free_Count (This : State) return Frame is
      Count : Frame := 0;
   begin
      for Item in Payload_Frame loop
         pragma Loop_Invariant (Count < Item);
         if This.Available (Item) then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Free_Count;
end Boot_Frame_Allocator;
