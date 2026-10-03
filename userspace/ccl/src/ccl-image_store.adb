package body CCL.Image_Store is
   use Interfaces;

   subtype Pixel_Index is Natural range 0 .. Maximum_Pixels - 1;
   type Pixel_Buffer is array (Pixel_Index) of Pixel;
   subtype Pool_Index is Natural range 0 .. Pool_Pixels - 1;
   type Pool_Buffer is array (Pool_Index) of Pixel;
   type Slot is record
      Id : Image_Id := No_Image;
      Width, Height : Natural := 0;
      Offset : Natural := 0;       --  its first pixel in the pool
      Used : Unsigned_64 := 0;     --  last use, for replacement
   end record;
   type Slot_Array is array (1 .. Maximum_Images) of Slot;

   Slots : Slot_Array;
   Pool : Pool_Buffer := [others => 0];
   Clock : Unsigned_64 := 0;
   Draft : Pixel_Buffer := [others => 0];
   Draft_W, Draft_H : Natural := 0;
   Drafting : Boolean := False;

   function Size (S : Slot) return Natural is (S.Width * S.Height);

   function Building return Boolean is (Drafting);
   function Draft_Width return Natural is (if Drafting then Draft_W else 0);
   function Draft_Height return Natural is (if Drafting then Draft_H else 0);

   procedure Start (Width, Height : Side; Fill : Pixel) is
   begin
      Draft_W := Width;
      Draft_H := Height;
      Draft (0 .. Width * Height - 1) := [others => Fill];
      Drafting := True;
   end Start;

   procedure Set (X, Y : Natural; Value : Pixel) is
   begin
      if Drafting and then X < Draft_W and then Y < Draft_H then
         Draft (Y * Draft_W + X) := Value;
      end if;
   end Set;

   function Draft_Pixel (X, Y : Natural) return Pixel is
     (if Drafting and then X < Draft_W and then Y < Draft_H
      then Draft (Y * Draft_W + X) else 0);

   --  FNV-1a over the size and the pixels, as a positive 63-bit id.
   function Digest return Image_Id is
      PRIME : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
      Hash : Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
      procedure Mix (Word : Unsigned_32) is
      begin
         for Shift in 0 .. 3 loop
            Hash := (Hash xor (Shift_Right (Unsigned_64 (Word), Shift * 8) and 16#FF#)) * PRIME;
         end loop;
      end Mix;
   begin
      Mix (Unsigned_32 (Draft_W));
      Mix (Unsigned_32 (Draft_H));
      for I in 0 .. Draft_W * Draft_H - 1 loop
         Mix (Draft (I));
      end loop;
      Hash := Hash and 16#7FFF_FFFF_FFFF_FFFF#;
      return (if Hash = 0 then 1 else Image_Id (Hash));
   end Digest;

   function Find (Id : Image_Id) return Natural is
   begin
      if Id /= No_Image then
         for I in Slots'Range loop
            if Slots (I).Id = Id then return I; end if;
         end loop;
      end if;
      return 0;
   end Find;

   function Pool_Used return Natural is
      Total : Natural := 0;
   begin
      for S of Slots loop
         if S.Id /= No_Image then Total := Total + Size (S); end if;
      end loop;
      return Total;
   end Pool_Used;

   --  Slide every image to the front of the pool, in offset order, so the
   --  free pixels are one run at the end. Returns where that run starts.
   function Compact return Natural is
      Next : Natural := 0;
      Moved : array (Slot_Array'Range) of Boolean := [others => False];
   begin
      loop
         declare
            Lowest : Natural := 0;
         begin
            for I in Slots'Range loop
               if Slots (I).Id /= No_Image and then not Moved (I) and then
                 (Lowest = 0 or else Slots (I).Offset < Slots (Lowest).Offset)
               then
                  Lowest := I;
               end if;
            end loop;
            exit when Lowest = 0;
            declare
               S : Slot renames Slots (Lowest);
               N : constant Natural := Size (S);
            begin
               if S.Offset /= Next and then N > 0 then
                  --  Moving down never overlaps a later image, and copying
                  --  forward is safe when the regions overlap.
                  for K in 0 .. N - 1 loop
                     Pool (Next + K) := Pool (S.Offset + K);
                  end loop;
               end if;
               S.Offset := Next;
               Next := Next + N;
               Moved (Lowest) := True;
            end;
         end;
      end loop;
      return Next;
   end Compact;

   procedure Finish (Id : out Image_Id) is
      Count : constant Natural := Draft_W * Draft_H;
   begin
      Id := No_Image;
      if not Drafting then return; end if;
      Drafting := False;
      Id := Digest;
      Clock := Clock + 1;
      declare
         Existing : constant Natural := Find (Id);
      begin
         if Existing > 0 then
            if Slots (Existing).Width = Draft_W and then Slots (Existing).Height = Draft_H and then
              (for all K in 0 .. Count - 1 => Pool (Slots (Existing).Offset + K) = Draft (K))
            then
               Slots (Existing).Used := Clock;
               return;
            end if;
            Slots (Existing).Id := No_Image;   --  a digest collision: the newer content wins
         end if;
      end;
      --  Room: drop the least recently used images until the pixels and a
      --  slot are free.
      loop
         declare
            Free_Slot : Natural := 0;
            Oldest : Natural := 0;
         begin
            for I in Slots'Range loop
               if Slots (I).Id = No_Image then
                  if Free_Slot = 0 then Free_Slot := I; end if;
               elsif Oldest = 0 or else Slots (I).Used < Slots (Oldest).Used then
                  Oldest := I;
               end if;
            end loop;
            exit when Free_Slot /= 0 and then Pool_Used + Count <= Pool_Pixels;
            exit when Oldest = 0;
            Slots (Oldest).Id := No_Image;
         end;
      end loop;
      declare
         Start_At : constant Natural := Compact;
         Free_Slot : Natural := 0;
      begin
         for I in Slots'Range loop
            if Slots (I).Id = No_Image then Free_Slot := I; exit; end if;
         end loop;
         if Free_Slot = 0 or else Start_At + Count > Pool_Pixels then
            Id := No_Image;
            return;
         end if;
         for K in 0 .. Count - 1 loop
            Pool (Start_At + K) := Draft (K);
         end loop;
         Slots (Free_Slot) := (Id => Id, Width => Draft_W, Height => Draft_H,
                               Offset => Start_At, Used => Clock);
      end;
   end Finish;

   procedure Discard is
   begin
      Drafting := False;
   end Discard;

   function Known (Id : Image_Id) return Boolean is (Find (Id) > 0);
   function Width (Id : Image_Id) return Natural is
     (if Find (Id) > 0 then Slots (Find (Id)).Width else 0);
   function Height (Id : Image_Id) return Natural is
     (if Find (Id) > 0 then Slots (Find (Id)).Height else 0);
   function Pixel_At (Id : Image_Id; X, Y : Natural) return Pixel is
      Position : constant Natural := Find (Id);
   begin
      if Position = 0 or else X >= Slots (Position).Width or else Y >= Slots (Position).Height then
         return 0;
      end if;
      return Pool (Slots (Position).Offset + Y * Slots (Position).Width + X);
   end Pixel_At;
end CCL.Image_Store;
