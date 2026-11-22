pragma Ada_2022;

package body Debug_View_Walk with SPARK_Mode is
   function Walk (Root, Address, Physical_Last : Unsigned_64) return Result is
      Table_Frame : Unsigned_64 := Root;
      Word, Frame : Unsigned_64;
   begin
      if Address >= User_Limit or else not RAM_Page (Root, Physical_Last) then
         return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
      end if;
      for L in Level loop
         pragma Loop_Invariant (RAM_Page (Table_Frame, Physical_Last));
         declare
            Shift : constant Natural :=
              (case L is when P4_Level => 39, when P3_Level => 30,
                         when P2_Level => 21, when P1_Level => 12);
            Index : constant Table_Index := Natural (Shift_Right (Address, Shift) and 511);
         begin
            Read_Entry (Table_Frame, Index, Word);
         end;
         if (Word and Required) /= Required then
            return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
         end if;
         declare
            Large : constant Boolean :=
              L = P2_Level and then (Word and Large_Page_Bit) /= 0;
         begin
            if L in P4_Level | P3_Level and then (Word and Large_Page_Bit) /= 0 then
               -- No 1 GiB leaf, and never a large bit read as a table.
               return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
            elsif L = P1_Level or else Large then
               if not Write_Back (Word, Large) then
                  return (Frame => 0, Kind => Not_Write_Back, Leaf => 0, Large => False);
               end if;
               Frame := Leaf_Frame (Word, Address, Large);
               if not RAM_Page (Frame, Physical_Last) then
                  return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
               end if;
               return (Frame => Frame, Kind => Viewable, Leaf => Word, Large => Large);
            end if;
         end;
         Frame := Word and Frame_Mask;
         if not RAM_Page (Frame, Physical_Last) then
            return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
         end if;
         Table_Frame := Frame;
      end loop;
      return (Frame => 0, Kind => Unmapped, Leaf => 0, Large => False);
   end Walk;
end Debug_View_Walk;
