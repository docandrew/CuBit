with Ada.Text_IO;
with Buddy_Boot_Admission; use Buddy_Boot_Admission;
with Buddy_Geometry; use type Buddy_Geometry.Count;

procedure Main is
   subtype Small_Frame is Frame range 0 .. 63;
   Consults : Natural;
   function Expected_Admission (Item, Highest : Frame) return Boolean is
     (Item > Highest or else Item mod 3 = 0);
begin
   -- Exhaust every interval and inclusive high-water value in the small
   -- arena. Bitmap entries deliberately alternate allocated/free frames.
   for Highest in Small_Frame loop
      Prove_Boundary (Highest);
      for First in Small_Frame loop
         for Last in First .. Small_Frame'Last loop
            Consults := 0;
            if Source_Of (First, Highest) = Unallocated_Tail then
               Prove_Whole_Tail (First, Last, Highest);
               for Item in First .. Last loop
                  pragma Assert (Expected_Admission (Item, Highest));
               end loop;
            else
               for Item in First .. Last loop
                  declare
                     Accepted : Boolean;
                  begin
                     if Source_Of (Item, Highest) = Boot_Bitmap then
                        Prove_Bitmap_Bound (Item, Highest, Highest);
                        pragma Assert (Item <= Highest);
                        Consults := Consults + 1;
                        Accepted := Item mod 3 = 0;
                     else
                        Accepted := True;
                     end if;
                     pragma Assert (Accepted = Expected_Admission (Item, Highest));
                  end;
               end loop;
               pragma Assert (Consults > 0);
            end if;
         end loop;
      end loop;
   end loop;
   -- The old strict comparison incorrectly admitted frame 16 as a whole
   -- block when the last boot allocation was exactly frame 16.
   pragma Assert (Source_Of (16, 16) = Boot_Bitmap);
   pragma Assert (not Expected_Admission (16, 16));
   pragma Assert (Source_Of (17, 16) = Unallocated_Tail);
   Prove_Boundary (Frame'Last);
   Prove_Bitmap_Bound (Frame'Last, Frame'Last, Frame'Last);
   Prove_Whole_Tail (Frame'Last, Frame'Last, Frame'Last - 1);
   pragma Assert (Source_Of (Frame'Last, Frame'Last - 1) = Unallocated_Tail);
   for Words in Word_Count range 1 .. 256 loop
      pragma Assert (Last_Frame (Words) = Words * 64 - 1);
      for Item in 0 .. Last_Frame (Words) loop
         Prove_Word_Bound (Item, Words);
         pragma Assert (Word_Of (Item) < Words);
         pragma Assert (Word_Of (Item) * 64 + Frame (Bit_Of (Item)) = Item);
      end loop;
      -- The old limit admitted this first unrepresented frame.
      pragma Assert (Word_Of (Last_Frame (Words) + 1) = Words);
   end loop;
   pragma Assert (Last_Frame (256) = 16383);
   pragma Assert (Word_Of (16384) = 256);
   Prove_Word_Bound (Frame'Last, Word_Count'Last);
   Ada.Text_IO.Put_Line ("PASS boot admission: 133120 intervals, inclusive boundary and full-width edges");
   Ada.Text_IO.Put_Line ("PASS boot bitmap: 2105344 indices and first-unrepresented-frame regression");
end Main;
