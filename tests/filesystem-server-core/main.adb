--  Hosted tests of the filesystem service's pure core: Fs_Handle_Codes and
--  Fs_Path_Checks against the formulas main.adb used before they were
--  extracted (kept here as references), exhaustively where the domain is
--  small and over pseudo-random samples elsewhere.
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Volume_List;
with Fs_Handle_Codes; use Fs_Handle_Codes;
with Fs_Path_Checks;

procedure Main is
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         raise Program_Error with "FAILED: " & What;
      end if;
   end Check;

   --  xorshift64*, a fixed seed: the same samples every run.
   State : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next return Unsigned_64 is
   begin
      State := State xor Shift_Right (State, 12);
      State := State xor Shift_Left (State, 25);
      State := State xor Shift_Right (State, 27);
      return State * 16#2545_F491_4F6C_DD1D#;
   end Next;

   ---------------------------------------------------------------------------
   --  References: the code main.adb had before the extraction.
   ---------------------------------------------------------------------------
   function Ref_Encode (Slot : Natural; Gen : Unsigned_32) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Gen), 32) or Unsigned_64 (Slot + 1));

   function Ref_Tag (Slot : Natural; Gen : Unsigned_32) return Unsigned_32 is
     (Shift_Left (Gen and (2 ** 21 - 1), 11) or Unsigned_32 (Slot));

   function Ref_Identity_Hash (Volume : Natural; Number : Unsigned_32) return Unsigned_32 is
     (Number * 16#9E37_79B9# xor Unsigned_32 (Volume) * 16#85EB_CA6B#);

   function Ref_Version_Home (Volume : Natural; Number : Unsigned_32) return Natural is
     (Natural (Shift_Right ((Number xor Unsigned_32 (Volume)) * 16#9E37_79B9#, 32 - 10)));

   function Ref_Check (Number : Unsigned_32; Volume : Natural; Ext2_Gen : Unsigned_32;
                       Is_Ext2 : Boolean) return Unsigned_32 is
      Mixed : Unsigned_32 := Number * 16#9E37_79B1# xor
        Shift_Left (Unsigned_32 (Volume), 24);
   begin
      if Is_Ext2 then
         Mixed := Mixed xor Ext2_Gen;
      end if;
      return (if Mixed = 0 then 16#9E37_79B1# else Mixed);
   end Ref_Check;

   function Ref_Has_Traversal (path : String) return Boolean is
   begin
      if path'Length = 2 and then
         path (path'First) = '.' and then path (path'First + 1) = '.'
      then
         return True;
      end if;
      for i in path'First .. path'Last - 1 loop
         if path (i) = '.' and then path (i + 1) = '.' then
            declare
               atStart : constant Boolean :=
                 (i = path'First) or else path (i - 1) = '/';
               atEnd   : constant Boolean :=
                 (i + 1 = path'Last) or else path (i + 2) = '/';
            begin
               if atStart and atEnd then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Ref_Has_Traversal;

   function Ref_Canonical (path : String) return String is
      result : String (1 .. path'Length) := [others => ' '];
      used : Natural := 0;
   begin
      for c of path loop
         if c /= '/' or else (used > 0 and then result (used) /= '/') then
            used := used + 1;
            result (used) := c;
         end if;
      end loop;
      if used > 0 and then result (used) = '/' then
         used := used - 1;
      end if;
      return result (1 .. used);
   end Ref_Canonical;

   Generations : constant array (Positive range <>) of Unsigned_32 :=
     [1, 2, 3, 16#1F_FFFF#, 16#20_0000#, 16#7FFF_FFFF#, 16#8000_0000#,
      16#FFFF_FFFE#, 16#FFFF_FFFF#];

   Refused_Codes : constant array (Positive range <>) of Unsigned_64 :=
     [0, Maximum_Handles + 1, 16#FFFF_FFFF#];

   procedure Test_Handles is
      Gen : Unsigned_32;
   begin
      for Slot in Handle_Slot loop
         for G of Generations loop
            declare
               Word : constant Handle_Word := Encode (Slot, Generation (G));
            begin
               Check (Unsigned_64 (Word) = Ref_Encode (Natural (Slot), G), "encode formula");
               Check (Valid_Slot_Code (Word), "encoded slot code valid");
               Check (Slot_Of (Word) = Slot, "slot round trip");
               Check (Generation_Of (Word) = Generation (G), "generation round trip");
               Check (Unsigned_32 (Tag_Of (Slot, Generation (G))) = Ref_Tag (Natural (Slot), G),
                      "tag formula");
               Check (Tag_Slot (Tag_Of (Slot, Generation (G))) = Slot, "tag slot");
            end;
         end loop;
         for K in 1 .. 16 loop
            Gen := Unsigned_32 (Next and 16#FFFF_FFFF#);
            Check (Generation_Of (Encode (Slot, Generation (Gen))) = Generation (Gen),
                   "random generation round trip");
         end loop;
      end loop;
      --  Refused slot codes: zero and past the table, whatever the generation.
      for G of Generations loop
         for Code of Refused_Codes loop
            Check (not Valid_Slot_Code (Handle_Word (Shift_Left (Unsigned_64 (G), 32) or Code)),
                   "invalid slot code refused");
         end loop;
      end loop;
      Check (Valid_Slot_Code (Handle_Word (Maximum_Handles)), "last slot code accepted");
      --  Any 64-bit word: valid exactly when its low word is 1 .. table size,
      --  and then it re-encodes to itself.
      for K in 1 .. 200_000 loop
         declare
            Word : constant Handle_Word :=
              Handle_Word (if K mod 2 = 0 then Next
                           else (Next and 16#FFFF_FFFF_0000_0000#) or (Next mod 2_100));
            Low : constant Unsigned_64 := Unsigned_64 (Word) and 16#FFFF_FFFF#;
         begin
            Check (Valid_Slot_Code (Word) = (Low in 1 .. Maximum_Handles), "validity rule");
            if Valid_Slot_Code (Word) then
               Check (Encode (Slot_Of (Word), Generation_Of (Word)) = Word, "re-encode");
            end if;
         end;
      end loop;
      Check (Retires (Generation'Last) and then not Retires (First_Generation), "retire rule");
      Check (Next (First_Generation) = 2, "next generation");
   end Test_Handles;

   procedure Test_Hashes is
      Number, Gen : Unsigned_32;
   begin
      for Volume in Volume_List.Volume_Index loop
         for K in 1 .. 20_000 loop
            Number := Unsigned_32 (Next and 16#FFFF_FFFF#);
            Gen := Unsigned_32 (Next and 16#FFFF_FFFF#);
            Check (Identity_Hash (Volume, Number) = Ref_Identity_Hash (Volume, Number),
                   "identity hash formula");
            Check (Natural (Version_Home (Volume, Number)) = Ref_Version_Home (Volume, Number),
                   "version home formula");
            Check (Object_Id (Volume, Number) =
                     (Shift_Left (Unsigned_64 (Volume), 32) or Unsigned_64 (Number)),
                   "object id formula");
            Check (Unsigned_32 (Check_Of (Number, Volume, Gen)) =
                     Ref_Check (Number, Volume, Gen, True), "ext2 directory check");
            Check (Unsigned_32 (Check_Of (Number, Volume, 0)) =
                     Ref_Check (Number, Volume, Gen, False), "archive directory check");
            declare
               Position : constant Unsigned_64 := Next and Token_Position_Mask;
               Token : constant Resume_Token := Token_Of (Check_Of (Number, Volume, Gen), Position);
            begin
               Check (Unsigned_64 (Token) =
                        (Shift_Left (Unsigned_64 (Ref_Check (Number, Volume, Gen, True)), 32) or
                         Position), "token formula");
               Check (Check_In (Token) = Check_Of (Number, Volume, Gen), "token check");
               Check (Position_In (Token) = Position, "token position");
            end;
         end loop;
         --  The one generation that zeroes the mix: the check is then the
         --  golden constant, never zero.
         Number := Unsigned_32 (Next and 16#FFFF_FFFF#);
         Gen := Number * 16#9E37_79B1# xor Shift_Left (Unsigned_32 (Volume), 24);
         Check (Unsigned_32 (Check_Of (Number, Volume, Gen)) = 16#9E37_79B1#, "zero mix");
      end loop;
      --  Probing wraps at the table's end and visits Version_Probes entries.
      for Home in Version_Slot loop
         for Step in Version_Probe loop
            Check (Natural (Probe (Home, Step)) =
                     (Natural (Home) + Natural (Step)) mod Version_Entries, "probe");
         end loop;
      end loop;
      Check (Next_Probe (Version_Probe'Last) = Version_Probe'First, "probe cycle");
   end Test_Hashes;

   --  Every string over {'.', '/', 'a'} up to Max_Length, at two lower bounds.
   procedure Test_Paths is
      Alphabet : constant String := "./a";
      Max_Length : constant := 8;
      Text : String (1 .. Max_Length);

      procedure Compare (Path : String) is
         Canonical : constant String := Fs_Path_Checks.Canonical (Path);
      begin
         Check (Fs_Path_Checks.Has_Traversal (Path) = Ref_Has_Traversal (Path),
                "traversal: """ & Path & """");
         Check (Canonical = Ref_Canonical (Path), "canonical: """ & Path & """");
         Check (Canonical'First = 1, "canonical lower bound");
      end Compare;

      procedure Walk (Length, Filled : Natural) is
      begin
         if Filled = Length then
            Compare (Text (1 .. Length));
            declare
               Shifted : constant String (100 .. 99 + Length) := Text (1 .. Length);
            begin
               Compare (Shifted);
            end;
            return;
         end if;
         for C of Alphabet loop
            Text (Filled + 1) := C;
            Walk (Length, Filled + 1);
         end loop;
      end Walk;
   begin
      for Length in 0 .. Max_Length loop
         Walk (Length, 0);
      end loop;
      Check (Fs_Path_Checks.Has_Traversal (".."), "dotdot");
      Check (Fs_Path_Checks.Has_Traversal ("a/../b"), "inner");
      Check (not Fs_Path_Checks.Has_Traversal ("a/..b/c"), "dotted name");
      Check (not Fs_Path_Checks.Has_Traversal ("..."), "three dots");
      Check (Fs_Path_Checks.Canonical ("//a//b/") = "a/b", "canonical sample");
   end Test_Paths;
begin
   Test_Handles;
   Test_Hashes;
   Test_Paths;
   Put_Line ("SERVER-CORE-CHECK: PASS (" & Checks'Image & " checks)");
end Main;
