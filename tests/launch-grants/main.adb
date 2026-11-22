--  Delegated launch grants: the builder's regions are valid, Next walks
--  them back exactly, and malformed regions (every one-byte corruption of
--  a valid one that breaks the rules) are refused.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Launch_Grants; use CuBit.Launch_Grants;

procedure Main is
   Failures, Checks : Natural := 0;
   B : Builder;
   Region : Bytes (1 .. Maximum_Bytes);
   Length : Byte_Count;
   Added : Boolean;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   type Name_Access is access constant String;
   Names : constant array (1 .. 3) of Name_Access :=
     [new String'("@nvme:0/cubit/build"), new String'("@nvme:0/tmp"),
      new String'("@nvme:0/src/a.c")];
   Rights : constant array (1 .. 3) of Unsigned_8 :=
     [Read_Right or Write_Right or Create_Right, Read_Right or Write_Right,
      Read_Right];
begin
   Start (B);
   Finish (B, Region, Length);
   Check (Length = 0, "no grants, no region");
   for I in Names'Range loop
      Add (B, Rights (I), Names (I).all, Added);
      Check (Added, "add " & Names (I).all);
   end loop;
   Add (B, Read_Right, "relative/name", Added);
   Check (not Added, "unqualified name refused");
   Add (B, 4, "@nvme:0/x", Added);
   Check (not Added, "unknown right refused");
   Add (B, 0, "@nvme:0/x", Added);
   Check (not Added, "no rights refused");
   Add (B, Read_Right, "@" & (1 .. 256 => 'x'), Added);
   Check (not Added, "257-byte name refused");
   Finish (B, Region, Length);
   Check (Length > Header_Bytes and then Valid (Region (1 .. Length)), "valid region");

   --  Walk it back.
   declare
      Position : Positive := Header_Bytes + 1;
      R : Unsigned_8;
      First, Last : Positive;
   begin
      for I in Names'Range loop
         Next (Region (1 .. Length), Position, R, First, Last);
         Check (R = Rights (I), "rights" & I'Image);
         Check (String'([for K in First .. Last => Character'Val (Region (K))]) =
                  Names (I).all, "name" & I'Image);
      end loop;
      Check (Position = Length + 1, "walk ends at the end");
   end;

   --  Corruptions.
   declare
      Good : constant Bytes := Region (1 .. Length);
      Bad : Bytes (1 .. Length);
   begin
      Bad := Good; Bad (1) := 2;
      Check (not Valid (Bad), "wrong version");
      Bad := Good; Bad (3) := 4;
      Check (not Valid (Bad), "count beyond the entries");
      Bad := Good; Bad (3) := 2;
      Check (not Valid (Bad), "count short of the entries");
      Bad := Good; Bad (3) := 0;
      Check (not Valid (Bad), "zero count");
      Bad := Good; Bad (5) := 16;
      Check (not Valid (Bad), "unknown right bit");
      Bad := Good; Bad (6) := 200;
      Check (not Valid (Bad), "name past the end");
      Bad := Good; Bad (7) := 2;
      Check (not Valid (Bad), "name longer than 256 bytes");
      Bad := Good; Bad (8) := Character'Pos ('n');
      Check (not Valid (Bad), "unqualified name");
      Bad := Good; Bad (9) := 0;
      Check (not Valid (Bad), "NUL in a name");
      Check (not Valid (Good (1 .. Length - 1)), "truncated");
      declare
         Longer : constant Bytes := Good & [1 => 0];
      begin
         Check (not Valid (Longer), "trailing byte");
      end;
   end;

   --  A name as long as any path the filesystem takes.
   Start (B);
   Add (B, Read_Right, "@nvme:0/" & (1 .. 248 => 'd'), Added);
   Check (Added, "256-byte name accepted");
   Finish (B, Region, Length);
   Check (Valid (Region (1 .. Length)), "256-byte name region valid");

   --  At most sixteen.
   Start (B);
   for I in 1 .. Maximum_Grants loop
      Add (B, Read_Right, "@nvme:0/g", Added);
   end loop;
   Add (B, Read_Right, "@nvme:0/one-too-many", Added);
   Check (not Added and CuBit.Launch_Grants.Count (B) = Maximum_Grants, "seventeenth refused");
   Finish (B, Region, Length);
   Check (Valid (Region (1 .. Length)), "full region valid");

   if Failures = 0 then
      Put_Line ("LAUNCH-GRANTS: PASS" & Checks'Image & " checks");
   else
      Put_Line ("LAUNCH-GRANTS: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Main;
