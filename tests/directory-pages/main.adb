--  Hosted tests for CuBit.Directory_Pages (Directory.Page.V2,
--  docs/filesystem-protocol-v2.md step 3): every page the writer makes is
--  accepted and reads back the same entries; damaged pages never fault and
--  the damage the format can see is refused.
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Interfaces; use Interfaces;
with CuBit.Directory_Pages; use CuBit.Directory_Pages;

procedure Main is
   Failures, Checks : Natural := 0;

   procedure Check_That (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then
            Ada.Text_IO.Put_Line ("FAIL: " & What);
         end if;
      end if;
   end Check_That;

   subtype Byte_Range is Natural range 0 .. 255;
   package Random_Bytes is new Ada.Numerics.Discrete_Random (Byte_Range);
   G : Random_Bytes.Generator;

   function Random_Name_Byte return Unsigned_8 is
      B : Unsigned_8;
   begin
      loop
         B := Unsigned_8 (Random_Bytes.Random (G));
         exit when B /= 0 and then B /= Character'Pos ('/') and then B /= Character'Pos ('.');
      end loop;
      return B;
   end Random_Name_Byte;

   function Random_64 return Unsigned_64 is
      V : Unsigned_64 := 0;
   begin
      for I in 1 .. 8 loop
         V := Shift_Left (V, 8) or Unsigned_64 (Random_Bytes.Random (G));
      end loop;
      return V;
   end Random_64;

   type Entry_Record is record
      Item : Facts;
      Name : Name_Bytes;
      Length : Name_Length;
   end record;
   type Entry_Array is array (1 .. Maximum_Entries) of Entry_Record;

   P : CuBit.Directory_Pages.Page;
   W : Writer;
   Written : Entry_Array;
   Valid, Ended, OK : Boolean;
   Count : Entry_Count;
   Used : Used_Bytes;
   Resume, Stamp : Unsigned_64;
   Total_Entries : Natural := 0;
   Pages_Made : Natural := 0;
begin
   Random_Bytes.Reset (G, 2026_10_08);

   --  Record sizes: 8-byte multiples, at least the fixed part and the name.
   for L in 1 .. Maximum_Name_Bytes loop
      Check_That (Record_Bytes (L) mod Record_Alignment = 0 and then
                  Record_Bytes (L) >= Fixed_Entry_Bytes + L and then
                  Record_Bytes (L) < Fixed_Entry_Bytes + L + Record_Alignment,
                  "record size" & L'Image);
   end loop;

   --  An empty page that ends the directory.
   Start (P, W);
   Finish (P, W, Ended => True, Resume => 7, Stamp => 9);
   Check (P, Valid, Count, Used, Ended, Resume, Stamp);
   Check_That (Valid and then Count = 0 and then Used = Header_Bytes and then Ended
               and then Resume = 7 and then Stamp = 9, "empty page");

   --  Random pages: fill until a name no longer fits.
   for Round in 1 .. 5_000 loop
      declare
         N : Natural := 0;
         Max_Length : constant Name_Length :=
           (case Round mod 4 is when 0 => 8, when 1 => 24, when 2 => 255, when others => 64);
      begin
         Start (P, W);
         loop
            declare
               E : Entry_Record;
            begin
               E.Length := 1 + Random_Bytes.Random (G) mod Max_Length;
               E.Name := [others => 0];
               for I in 1 .. E.Length loop
                  E.Name (I) := Random_Name_Byte;
               end loop;
               E.Item := (Kind => Unsigned_8 (Random_Bytes.Random (G) mod (Last_Kind + 1)),
                          Valid => Unsigned_32 (Random_Bytes.Random (G) mod 64),
                          Object => Random_64, Size => Random_64, Modified => Random_64,
                          Changed => Random_64, Accessed => Random_64,
                          Mode => Unsigned_32 (Random_64 mod 2 ** 32),
                          Links => Unsigned_32 (Random_64 mod 2 ** 32),
                          Owner => Unsigned_32 (Random_64 mod 2 ** 32),
                          Group => Unsigned_32 (Random_64 mod 2 ** 32));
               exit when not Fits (W, E.Length);
               Append (P, W, E.Item, E.Name, E.Length);
               N := N + 1;
               Written (N) := E;
            end;
         end loop;
         Finish (P, W, Ended => Round mod 3 = 0, Resume => Unsigned_64 (Round), Stamp => 42);
         Pages_Made := Pages_Made + 1;
         Total_Entries := Total_Entries + N;
         Check (P, Valid, Count, Used, Ended, Resume, Stamp);
         Check_That (Valid and then Count = N and then Used = W.Used and then
                     Ended = (Round mod 3 = 0) and then Resume = Unsigned_64 (Round),
                     "written page accepted");
         --  Read every entry back.
         declare
            At_Entry : Natural := Header_Bytes;
            Item : Facts;
            Name : Name_Bytes;
            Length : Name_Length;
            Next : Natural;
         begin
            for I in 1 .. N loop
               Get (P, At_Entry, Used, Item, Name, Length, Next, OK);
               Check_That (OK and then Item = Written (I).Item and then Length = Written (I).Length
                           and then Name (1 .. Length) = Written (I).Name (1 .. Length),
                           "entry reads back");
               exit when not OK;
               At_Entry := Next;
            end loop;
         end;

         --  Damage: flip one random byte of the used part; Check never
         --  faults (-gnata, -gnato), and damage to the structure is seen.
         declare
            D : CuBit.Directory_Pages.Page := P;
            Where : constant Natural := Random_Bytes.Random (G) * 16 mod Used;
            Structural : constant Boolean :=
              Where in Version_At .. Used_At + 1 or else Where in Flags_At .. Reserved_At + 3;
         begin
            D (Where) := D (Where) xor 16#FF#;
            Check (D, Valid, Count, Used, Ended, Resume, Stamp);
            if Structural and then Where /= Flags_At then
               Check_That (not Valid, "damaged header refused at" & Where'Image);
            end if;
         end;
         --  Random garbage pages: never fault.
         if Round mod 10 = 0 then
            declare
               D : CuBit.Directory_Pages.Page;
               Item : Facts;
               Name : Name_Bytes;
               Length : Name_Length;
               Next : Natural;
            begin
               for I in D'Range loop
                  D (I) := Unsigned_8 (Random_Bytes.Random (G));
               end loop;
               Check (D, Valid, Count, Used, Ended, Resume, Stamp);
               for Offset in 0 .. 50 loop
                  Get (D, Offset * 83, Page_Bytes + Offset, Item, Name, Length, Next, OK);
               end loop;
            end;
         end if;
      end;
   end loop;

   --  Names a listing must not hold.
   declare
      Bad : Name_Bytes := [others => 0];
   begin
      Bad (1) := Character'Pos ('.');
      Check_That (not Valid_Name (Bad, 1), "dot");
      Bad (2) := Character'Pos ('.');
      Check_That (not Valid_Name (Bad, 2), "dot dot");
      Check_That (Valid_Name (Bad, 1) = False and then not Valid_Name (Bad, 3), "NUL inside");
      Bad (3) := Character'Pos ('/');
      Check_That (not Valid_Name (Bad, 3), "slash");
      Bad (3) := Character'Pos ('x');
      Check_That (Valid_Name (Bad, 3), "...x is a name");
   end;

   --  A page with a bad name is refused.
   declare
      N : Name_Bytes := [others => 0];
   begin
      Start (P, W);
      N (1) := Character'Pos ('a');
      N (2) := Character'Pos ('/');
      Append (P, W, (others => <>), N, 2);
      Finish (P, W, True, 0, 0);
      Check (P, Valid, Count, Used, Ended, Resume, Stamp);
      Check_That (not Valid, "slash in a name refused");
   end;

   Ada.Text_IO.Put_Line ("DIRECTORY-PAGES:" & Pages_Made'Image & " pages," & Total_Entries'Image
             & " entries (" & Natural (Total_Entries / Pages_Made)'Image & " per page),"
             & (if Failures = 0 then Checks'Image & " checks PASS"
                else Failures'Image & " of" & Checks'Image & " checks FAIL"));
end Main;
