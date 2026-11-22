with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Offscreen_Batch; use Intel_GPU_ADLN_Offscreen_Batch;
procedure Offscreen_Batch_Tests is
   I : Image;
   -- Packet order oracle, separate from builder concatenation. Exact low
   -- length fields are decoded below, so an interior word cannot hide a gap.
   Headers : constant Words :=
     [16#7A000204#, 16#69041310#, 16#7A000204#, 16#61010014#, 16#7A000004#,
      16#78220000#,
      16#780E0000#,
      16#79120000#, 16#79130000#, 16#79140000#, 16#79150000#, 16#79160000#,
      16#786D1F00#, 16#79190002#,
      16#78260000#, 16#78270000#, 16#78280000#, 16#78290000#,
      16#782A0000#, 16#782B0000#, 16#782C0000#, 16#782D0000#,
      16#782E0000#, 16#782F0000#,
      16#78300000#, 16#78310000#, 16#78320000#, 16#78330000#,
      16#781E0003#, 16#781B0007#, 16#781C0003#, 16#781D0009#,
      16#78110008#, 16#786C0004#, 16#78100007#, 16#78210000#,
      16#78230000#, 16#78120002#, 16#78500003#, 16#78130002#,
      16#78140000#, 16#781F0004#, 16#780D0000#, 16#78180000#,
      16#791C0007#,
      16#784D0000#, 16#78240000#, 16#784E0002#, 16#78710002#,
      16#78050006#, 16#78060006#, 16#7A000004#, 16#78070003#, 16#78040001#,
      16#79000002#, 16#7820000A#, 16#784F0000#, 16#78080003#,
      16#78090001#, 16#780C0000#, 16#78490001#, 16#784A0000#,
      16#78560001#, 16#784B0000#, 16#7B000005#, 16#05000000#];
   Position : Natural;
begin
   for Policy in Unsigned_32 range 0 .. 127 loop
      I := Build (Policy, 512, 546, 64);
      pragma Assert (I.Valid = (Policy in 2 .. 126 and Policy mod 2 = 0));
      if I.Valid then
         Position := 0;
         for H of Headers loop
            if I.Data (Position) /= H then
               Put_Line ("header mismatch at" & Position'Image &
                 " got" & I.Data (Position)'Image & " expected" & H'Image);
            end if;
            pragma Assert (I.Data (Position) = H);
            Position := Position +
              (if H in 16#69041310# | 16#05000000# then 1
               else Natural (H and 255) + 2);
         end loop;
         pragma Assert (Position = I.Count);
         pragma Assert (I.Count < 1024);
         pragma Assert (I.Data (I.Count - 8 .. I.Count - 1) =
           Words'[16#7B000005#, 0, 3, 0, 1, 0, 0, 16#05000000#]);
         pragma Assert (for all J in I.Count .. 1023 => I.Data (J) = 0);
      else
         pragma Assert (I.Count = 0 and (for all W of I.Data => W = 0));
      end if;
   end loop;
   for Limit in 0 .. 548 loop
      I := Build (6, 512, Limit, 64);
      pragma Assert (I.Valid = (Limit in 1 .. 546));
      I := Build (6, 512, 546, Limit);
      pragma Assert (I.Valid = (Limit in 1 .. 64));
      I := Build (6, Limit, 546, 64);
      pragma Assert (I.Valid = (Limit in 40 .. 512));
   end loop;
   I := Build (Unsigned_32'Last, Natural'Last, Natural'Last, Natural'Last);
   pragma Assert (not I.Valid and I.Count = 0 and (for all W of I.Data => W = 0));
   I := Build (6, 512, 546, 64);
   Put_Line ("offscreen batch assembly PASS: words=" & I.Count'Image &
     "; packet order, bounds, rejection; NOT submitted to GPU");
end Offscreen_Batch_Tests;
