with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Metadata_Initialize;
package body Update_Test_Backend is
   type RAM is array (Natural range <>) of Unsigned_8;
   type RAM_Access is access RAM;
   Memory : constant RAM_Access := new RAM (0 .. 32 * 1024 * 1024 + 4095);
   Raw : constant Unsigned_64 := Unsigned_64 (To_Integer (Memory.all'Address));
   First : constant Unsigned_64 := (Raw + 4095) / 4096 * 4096;
   Next : Unsigned_64 := 0;
   type Region is record Base, Limit, Committed : Unsigned_64 := 0; end record;
   Regions : array (1 .. 128) of Region;
   function Ready return Boolean is (Owner);
   procedure Reset is
   begin
      Owner := True; Lose_On_Reserve := False; Lose_On_Commit := False;
      Fail_Commit := False; Fail_Clear := False;
      Reservations := 0; Commits := 0; Clears := 0; Committed := 0; Next := 0;
      Regions := [others => (others => 0)];
   end Reset;
   function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
      Result : constant Unsigned_64 := First + Next;
   begin
      pragma Assert (Bytes > 0 and Bytes mod 4096 = 0 and Bytes <= 32 * 1024 * 1024 - Next);
      pragma Assert (Reservations < Regions'Length);
      Reservations := Reservations + 1;
      Regions (Reservations) := (Result, Bytes, 0); Next := Next + Bytes;
      if Lose_On_Reserve then Owner := False; end if;
      return Result;
   end Reserve;
   function Commit (Base, Offset, Bytes : Unsigned_64) return Boolean is
   begin
      Commits := Commits + 1;
      pragma Assert (Bytes in 4096 .. 65536 and Bytes mod 4096 = 0);
      if Lose_On_Commit then Owner := False; end if;
      if Fail_Commit then return False; end if;
      for I in 1 .. Reservations loop
         if Regions (I).Base = Base then
            pragma Assert (Offset = Regions (I).Committed and
                           Bytes <= Regions (I).Limit - Offset);
            Regions (I).Committed := Offset + Bytes;
            Committed := Committed + Bytes; return True;
         end if;
      end loop;
      raise Program_Error with "commit outside reserved metadata";
   end Commit;
   function Clear (Base, Bytes : Unsigned_64) return Boolean is
   begin
      Clears := Clears + 1;
      if Fail_Clear then return False; end if;
      for I in 1 .. Reservations loop
         if Base >= Regions (I).Base and then Base - Regions (I).Base <= Regions (I).Committed and then
           Bytes <= Regions (I).Committed - (Base - Regions (I).Base)
         then return Intel_GPU_Metadata_Initialize.Clear (Base, Bytes); end if;
      end loop;
      raise Program_Error with "clear outside committed metadata";
   end Clear;
end Update_Test_Backend;
