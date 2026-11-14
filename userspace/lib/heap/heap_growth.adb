pragma Ada_2022;
procedure Heap_Growth
  (B : in out Blocks.Block; Used : Natural; Initial, Ceiling : Positive; Wanted : Natural;
   Grown : out Boolean)
with SPARK_Mode => Off
is
   Size : Natural := (if Blocks.Capacity (B) = 0 then Initial else Blocks.Capacity (B));
   Fresh : Blocks.Block;
begin
   Grown := Blocks.Capacity (B) >= Wanted;
   if Grown then
      return;
   end if;
   while Size < Wanted and then Size < Ceiling loop
      Size := (if Size > Ceiling / 2 then Ceiling else 2 * Size);
   end loop;
   if Size < Wanted then
      return;
   end if;
   Blocks.Allocate (Fresh, Size);
   if Blocks.Capacity (Fresh) = 0 then
      return;
   end if;
   for I in 0 .. Used - 1 loop
      Blocks.Set (Fresh, I, Blocks.Get (B, I));
   end loop;
   Blocks.Release (B);
   B := Fresh;
   Grown := True;
end Heap_Growth;
