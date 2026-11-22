--  Hosted test of User_Access_Ranges (KERN-014 slice 2): ranges at the user
--  limit, wrapping lengths, and every overlap with an aperture, against a
--  byte-wise reference.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with User_Access_Ranges; use User_Access_Ranges;

procedure Main is
   Checks : Natural := 0;
   First : constant Unsigned_64 := 16#4000_0000_0000#;
   Last_Excl : constant Unsigned_64 := First + 16#1000_0000#;
   function Reference (Address, Bytes : Unsigned_64) return Boolean is
   begin
      if Address >= User_Limit or else Bytes > User_Limit - Address then
         return False;
      end if;
      -- Disjoint from [First, Last_Excl): the range ends at or before
      -- First, or starts at or after Last_Excl.
      return Bytes = 0 or else Address + Bytes <= First or else Address >= Last_Excl;
   end Reference;
   type U64_List is array (Positive range <>) of Unsigned_64;
   Addresses : constant U64_List :=
     [0, 1, 4095, 4096, First - 4096, First - 1, First, First + 1, Last_Excl - 1,
      Last_Excl, Last_Excl + 1, User_Limit - 4096, User_Limit - 1, User_Limit,
      User_Limit + 1, 16#FFFF_8000_0000_0000#, Unsigned_64'Last];
   Lengths : constant U64_List :=
     [0, 1, 4, 64, 4096, 4097, 16#1000_0000#, User_Limit, Unsigned_64'Last];
begin
   for A of Addresses loop
      for B of Lengths loop
         pragma Assert (Admitted (A, B, First, Last_Excl) = Reference (A, B));
         if Admitted (A, B, First, Last_Excl) and then B > 0 then
            Prove_Outside_Aperture (A, B, First, Last_Excl, A);
            Prove_Outside_Aperture (A, B, First, Last_Excl, A + B - 1);
         end if;
         Checks := Checks + 1;
      end loop;
   end loop;
   pragma Assert (not Within_User (User_Limit - 4, 5));
   pragma Assert (Within_User (User_Limit - 4, 4));
   pragma Assert (not Admitted (First - 4, 8, First, Last_Excl));
   pragma Assert (Admitted (First - 4, 4, First, Last_Excl));
   Put_Line ("user-access-ranges: checks" & Checks'Image);
   Put_Line ("user-access-ranges: PASS");
end Main;
