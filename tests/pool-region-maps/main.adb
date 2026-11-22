--  Hosted test of Pool_Region_Maps (KERN-014 slice 4): random takes and
--  gives of aligned runs against a reference array, with contracts on.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Pool_Region_Maps; use Pool_Region_Maps;

procedure Main is
   M : Region_Map := Joined;
   type Run is record First : Frame_Offset; Order : Run_Order; end record;
   Taken : array (1 .. 1024) of Run := [others => (0, 0)];
   Count : Natural := 0;
   Seed : Unsigned_64 := 16#DEAD_BEEF_1234_5678#;
   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
   Found : Boolean;
   First : Frame_Offset;
   Takes, Misses : Natural := 0;
begin
   pragma Assert (All_Free (M));
   for Step in 1 .. 200_000 loop
      if Count > 0 and then (Next mod 2 = 0 or else Count = Taken'Last) then
         declare
            I : constant Positive := Natural (Next mod Unsigned_64 (Count)) + 1;
         begin
            Give (M, Taken (I).First, Taken (I).Order);
            Taken (I) := Taken (Count);
            Count := Count - 1;
         end;
      else
         declare
            O : constant Run_Order := Run_Order (Next mod 7);
         begin
            Find (M.Free, O, Frame_Offset (Next mod 512), First, Found);
            if Found then
               Take (M, First, O);
               Count := Count + 1;
               Taken (Count) := (First, O);
               Takes := Takes + 1;
            else
               Misses := Misses + 1;
            end if;
         end;
      end if;
      pragma Assert (Valid (M));
   end loop;
   for I in 1 .. Count loop
      Give (M, Taken (I).First, Taken (I).Order);
   end loop;
   pragma Assert (All_Free (M));
   Find (M.Free, 9, 0, First, Found);
   pragma Assert (Found and then First = 0);
   for F in Frame_Offset loop
      Mark_Clean (M, F);
   end loop;
   Find (M.Clean, 9, 0, First, Found);
   pragma Assert (Found and then First = 0);
   Put_Line ("pool-region-maps: takes" & Takes'Image & " misses" & Misses'Image);
   Put_Line ("pool-region-maps: PASS");
end Main;
