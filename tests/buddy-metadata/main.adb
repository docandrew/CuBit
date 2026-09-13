with Ada.Text_IO;
with Buddy_Metadata; use Buddy_Metadata;
with Buddy_Geometry; use type Buddy_Geometry.Count;
with Buddy_Blocks;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

procedure Main is
   type Granules is array (Positive range <>) of Page_Size;
   type Frames is array (Positive range <>) of Frame;
   type Addresses is array (Positive range <>) of Unsigned_64;
   Checks : Natural := 0;
   procedure Check (Highest, Item : Frame; Kind : Table_Kind) is
      Width : constant Unsigned_64 := (if Kind = Block_State then 2 else 1);
      Expected_Bytes : constant Unsigned_64 := (Unsigned_64 (Highest) + 1) * Width;
   begin
      pragma Assert (Unsigned_64 (Bytes (Highest, Kind)) = Expected_Bytes);
      pragma Assert (Unsigned_64 (Offset (Item, Kind)) = Unsigned_64 (Item) * Width);
      Prove_Slot (Item, Highest, Kind);
      if Item < Highest then
         Prove_Separate (Item, Item + 1, Kind);
      end if;
      for Base of Addresses'[0, 16#123000#, 16#FFFF_8000_0000_0000#,
                             Unsigned_64'Last - Expected_Bytes + 1] loop
         pragma Assert (Fits_At (Base, Highest, Kind));
         pragma Assert (Address_Of (Base, Item, Kind) = Base + Unsigned_64 (Item) * Width);
         if Item < Highest then
            Prove_Address_Separation (Base, Item, Item + 1, Highest, Kind);
         end if;
      end loop;
      -- Exact-fit placement succeeds, one byte further wraps the span.
      if Expected_Bytes > 1 then
         pragma Assert (not Fits_At (Unsigned_64'Last - Expected_Bytes + 2, Highest, Kind));
      end if;
      Checks := Checks + 1;
   end Check;
begin
   for Kind in Table_Kind loop
      for Highest in Frame range 0 .. 1023 loop
         for Item in 0 .. Highest loop
            Check (Highest, Item, Kind);
         end loop;
         for Granule of Granules'[1, 3, 64, 4096, 2 ** 21, Page_Size'Last] loop
            Prove_Page_Coverage (Highest, Kind, Granule);
            pragma Assert (Unsigned_64 (Pages (Highest, Kind, Granule)) =
              (Unsigned_64 (Bytes (Highest, Kind)) + Unsigned_64 (Granule) - 1) /
                Unsigned_64 (Granule));
         end loop;
      end loop;
      for Highest of Frames'[0, 1, 2047, 2048, 4095, 4096, 2 ** 20 - 1,
                              Frame'Last - 1, Frame'Last] loop
         Check (Highest, 0, Kind);
         Check (Highest, Highest, Kind);
         for Granule of Granules'[1, 3, 64, 4096, 2 ** 21, Page_Size'Last] loop
            Prove_Page_Coverage (Highest, Kind, Granule);
         end loop;
      end loop;
      for Order in Buddy_Blocks.Order loop
         declare
            Length : constant Buddy_Geometry.Frame_Count := 2 ** Natural (Order);
         begin
            Prove_Block (0, Length, Frame'Last, Kind);
            Prove_Block (Frame'Last - Length + 1, Length, Frame'Last, Kind);
         end;
      end loop;
      for Budget of Granules'[1, 7, 16383] loop
         declare
            Last_Fit : constant Frame := Frame
              (Budget * 4096 / Entry_Bytes (Kind) - 1);
         begin
            pragma Assert (Pages (Last_Fit, Kind, 4096) = Budget);
            pragma Assert (Pages (Last_Fit + 1, Kind, 4096) = Budget + 1);
         end;
      end loop;
   end loop;
   declare
      type Records is array (Natural range <>) of Buddy_Blocks.Descriptor
        with Component_Size => Buddy_Blocks.Descriptor_Bits;
      Slots : Records (0 .. 3);
   begin
      pragma Assert (Descriptor_Bytes = 2);
      pragma Assert (To_Integer (Slots (1)'Address) - To_Integer (Slots (0)'Address) =
                     Integer_Address (Descriptor_Bytes));
   end;
   Ada.Text_IO.Put_Line ("PASS metadata slots:" & Checks'Image &
     " cases; page coverage, address wrap edges, descriptor stride, all block orders");
end Main;
