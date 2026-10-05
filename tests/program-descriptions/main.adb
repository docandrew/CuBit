--  Program descriptions: an ld-shaped description decodes (parameters,
--  pieces, connectors, descriptor map), values render into the expected argv and
--  places, bad calls are refused with the right result, malformed connectors and
--  maps are refused, and every one-byte corruption of the description
--  either decodes to a well-formed signature or is refused.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Launch_Arguments;
with CuBit.Launch_Grants;
with CuBit.Program_Descriptions; use CuBit.Program_Descriptions;

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package LG renames CuBit.Launch_Grants;

   Failures, Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Text (S : String) return Bytes is
      R : Bytes (1 .. S'Length);
   begin
      for K in S'Range loop
         R (K - S'First + 1) := Character'Pos (S (K));
      end loop;
      return R;
   end Text;

   --  ld: output (Output_File), inputs (Input_File, many), verbose (Flag,
   --  optional); argv: [--verbose] -o <output> <inputs...>; connectors
   --  unix.stderr (descriptor 2) and org.gnu.ld.progress (a level).
   Header : constant Bytes := Text ("PDSC") & [Version, 0, 3, 4, 2, 1, 0, 0];
   Ports : constant Bytes :=
     [Connector_Direction'Enum_Rep (Outlet), Element_Kind'Enum_Rep (Text_Lines),
      Signal_Kind'Enum_Rep (Stream), 4, 11] & Text ("unix.stderr")
     & [Connector_Direction'Enum_Rep (Outlet), Element_Kind'Enum_Rep (Integers),
        Signal_Kind'Enum_Rep (Level), 1, 19] & Text ("org.gnu.ld.progress");
   Map : constant Bytes := [2, 0];
   Ld : constant Bytes :=
     Header
     & [Kind'Enum_Rep (Output_File), 0, 6] & Text ("output")
     & [Kind'Enum_Rep (Input_File), Many_Flag, 6] & Text ("inputs")
     & [Kind'Enum_Rep (Flag), Optional_Flag, 7] & Text ("verbose")
     & [Piece_Kind'Enum_Rep (When_Set), 2, 9] & Text ("--verbose")
     & [Piece_Kind'Enum_Rep (Literal), 0, 2] & Text ("-o")
     & [Piece_Kind'Enum_Rep (Value), 0, 0]
     & [Piece_Kind'Enum_Rep (Value), 1, 0]
     & Ports & Map;
   Parameters_Part : constant Bytes := Ld (Header'Length + 1 .. Ld'Last - Ports'Length - Map'Length);

   S : Signature;
   V : Values;
   Accepted, Added : Boolean;
   Block : LA.Builder;
   Grants : LG.Builder;
   Result : Check_Result;

   function Argument (Index : Positive) return String is
      First : Positive;
      Last : Natural;
      Found : Boolean;
      Length : constant Natural := Block.Used;
   begin
      LA.Locate (Block.Data (1 .. Length), Index, First, Last, Found);
      if not Found then
         return "<missing>";
      end if;
      declare
         R : String (1 .. Last - First + 1);
      begin
         for K in R'Range loop
            R (K) := Character'Val (Block.Data (First + K - 1));
         end loop;
         return R;
      end;
   end Argument;

   procedure Expect (Expected : Check_Result; What : String) is
   begin
      Render (S, V, "@cd:0/apps/ld", Block, Grants, Result);
      Check (Result = Expected,
             What & ": " & Result'Image & " (expected " & Expected'Image & ")");
   end Expect;

   Corrupt : Bytes (Ld'Range);
   Survived : Natural := 0;
begin
   Decode (Ld, S, Accepted);
   Check (Accepted, "ld descriptor decodes");
   Check (S.Parameter_Total = 3 and then S.Piece_Total = 4, "ld counts");
   Check (S.Parameters (1).Many and then not S.Parameters (0).Many, "flags");
   Check (S.Parameters (2).Name (1 .. 7) = "verbose", "names");

   declare
      Index : Parameter_Index;
      Found : Boolean;
   begin
      Find (S, "inputs", Index, Found);
      Check (Found and then Index = 1, "find inputs");
      Find (S, "input", Index, Found);
      Check (not Found, "no parameter input");
   end;

   --  The good call.
   Clear (V);
   Add (V, 1, "@nvme:0/work/hello.o", Added);
   Add (V, 1, "@nvme:0/work/crt0.o", Added);
   Add (V, 0, "@nvme:0/work/hello", Added);
   Expect (Matches, "ld call");
   Check (Block.Data (LA.Argument_Count_Offset + 1) = 5, "argc 5");
   Check (Argument (1) = "@cd:0/apps/ld", "argv0");
   Check (Argument (2) = "-o", "argv1");
   Check (Argument (3) = "@nvme:0/work/hello", "argv2 output");
   Check (Argument (4) = "@nvme:0/work/hello.o", "argv3 first input");
   Check (Argument (5) = "@nvme:0/work/crt0.o", "argv4 second input");
   Check (LG.Count (Grants) = 3, "three places");
   declare
      Region : LG.Bytes (1 .. LG.Maximum_Bytes);
      Length : LG.Byte_Count;
      Position : Positive := LG.Header_Bytes + 1;
      Rights : Unsigned_8;
      First, Last : Positive;
   begin
      LG.Finish (Grants, Region, Length);
      Check (LG.Valid (Region (1 .. Length)), "places region valid");
      LG.Next (Region (1 .. Length), Position, Rights, First, Last);
      Check (Rights = LG.Read_Right, "input is read-only");
      LG.Next (Region (1 .. Length), Position, Rights, First, Last);
      LG.Next (Region (1 .. Length), Position, Rights, First, Last);
      Check (Rights = Rights_For (Output_File)
             and then (Rights and LG.Create_Right) /= 0, "output may be created");
   end;

   --  The flag adds its literal.
   Add (V, 2, "", Added);
   Expect (Matches, "verbose call");
   Check (Argument (2) = "--verbose" and then Argument (3) = "-o", "flag literal first");
   Check (LG.Count (Grants) = 3, "a flag is no place");

   --  Refusals.
   Clear (V);
   Add (V, 1, "@nvme:0/work/hello.o", Added);
   Expect (Missing_Parameter, "no output");
   Add (V, 0, "@nvme:0/work/a", Added);
   Add (V, 0, "@nvme:0/work/b", Added);
   Expect (Too_Many_Values, "two outputs");
   Clear (V);
   Add (V, 0, "work/hello", Added);
   Add (V, 1, "@nvme:0/work/hello.o", Added);
   Expect (Bad_File_Name, "relative output");
   Clear (V);
   Add (V, 0, "@nvme:0/work/hello", Added);
   Add (V, 3, "@nvme:0/x", Added);
   Expect (Unknown_Parameter, "no fourth parameter");
   Clear (V);
   Add (V, 0, "@nvme:0/work/hello", Added);
   for I in 1 .. 16 loop
      Add (V, 1, "@nvme:0/work/f" & I'Image (2 .. I'Image'Last) & ".o", Added);
   end loop;
   Expect (Too_Many_Places, "seventeen places");
   Clear (V);
   Add (V, 0, "@nvme:0/work/hello", Added);
   Add (V, 1, "@nvme:0/work/a" & Character'Val (0), Added);
   Expect (Bad_Text, "NUL in a file name");

   --  The Values buffers refuse overflow instead of growing.
   Clear (V);
   for I in 1 .. Maximum_Values loop
      Add (V, 1, "@x", Added);
   end loop;
   Add (V, 1, "@x", Added);
   Check (not Added, "value count bounded");

   --  Malformed descriptors.
   Decode (Ld (1 .. Ld'Last - 1), S, Accepted);
   Check (not Accepted, "truncated refused");
   Decode (Ld & [0], S, Accepted);
   Check (not Accepted, "trailing byte refused");
   Decode (Text ("PDSC") & [Version, 0, 0, 0, 0, 0, 0, 0], S, Accepted);
   Check (Accepted and then S.Parameter_Total = 0 and then S.Connector_Total = 0,
          "empty description decodes");

   --  Connectors and the descriptor map.
   Decode (Ld, S, Accepted);
   Check (Accepted and then S.Connector_Total = 2 and then S.Descriptor_Total = 1, "connectors decode");
   Check (S.Connectors (1).Signal = Level and then S.Connectors (1).Element = Integers, "signal and element");
   Check (S.Connectors (0).Pages = 4, "outlet ring pages");
   Check (S.Descriptors (1).Number = 2 and then S.Descriptors (1).Target = 0, "descriptor 2 maps");
   declare
      type Name_Access is access constant String;
      Bad_Names : constant array (1 .. 6) of Name_Access :=
        [new String'("stderr"), new String'("unix..err"), new String'(".unix"),
         new String'("unix."), new String'("Unix.err"), new String'("unix.std err")];
      Index : Connector_Index;
      Found : Boolean;
      function With_Connectors (P : Bytes; Count : Unsigned_8; M : Bytes; Maps : Unsigned_8)
        return Bytes is
        (Text ("PDSC") & [Version, 0, 3, 4, Count, Maps, 0, 0] & Parameters_Part & P & M);
      function One_Connector (Way : Connector_Direction; Name : String) return Bytes is
        ([Connector_Direction'Enum_Rep (Way), Element_Kind'Enum_Rep (Text_Lines),
          Signal_Kind'Enum_Rep (Stream), 1, Unsigned_8 (Name'Length)] & Text (Name));
   begin
      Find_Connector (S, "org.gnu.ld.progress", Index, Found);
      Check (Found and then Index = 1, "find a connector");
      Find_Connector (S, "stderr", Index, Found);
      Check (not Found, "an unqualified name is no connector");
      for Bad of Bad_Names loop
         Decode (With_Connectors (One_Connector (Outlet, Bad.all), 1, [], 0), S, Accepted);
         Check (not Accepted, "connector name refused: " & Bad.all);
      end loop;
      Decode (With_Connectors (One_Connector (Outlet, "unix.stdout"), 1, [], 0), S, Accepted);
      Check (Accepted, "a qualified connector decodes");
      Decode (With_Connectors (One_Connector (Outlet, "unix.stdout") & One_Connector (Outlet, "unix.stdout"),
                          2, [], 0), S, Accepted);
      Check (not Accepted, "a connector declared twice is refused");
      Decode (With_Connectors (One_Connector (Outlet, "unix.stdout"), 1, [0, 0], 1), S, Accepted);
      Check (not Accepted, "descriptor 0 cannot write an outlet");
      Decode (With_Connectors (One_Connector (Inlet, "unix.stdin"), 1, [1, 0], 1), S, Accepted);
      Check (not Accepted, "descriptor 1 cannot read an inlet");
      Decode (With_Connectors (One_Connector (Inlet, "unix.stdin"), 1, [0, 0], 1), S, Accepted);
      Check (Accepted, "descriptor 0 reads an inlet");
      Decode (With_Connectors (One_Connector (Outlet, "unix.stdout"), 1, [1, 1], 1), S, Accepted);
      Check (not Accepted, "a descriptor names a missing connector");
      Decode (With_Connectors (One_Connector (Outlet, "unix.stdout"), 1, [1, 0, 1, 0], 2), S, Accepted);
      Check (not Accepted, "a descriptor mapped twice is refused");
   end;
   for I in Ld'Range loop
      for B in Unsigned_8 loop
         Corrupt := Ld;
         Corrupt (I) := B;
         Decode (Corrupt, S, Accepted);
         if Accepted then
            Survived := Survived + 1;
            Check (Well_Formed (S), "corrupt decode well formed");
         else
            Check (S.Parameter_Total = 0 and then S.Piece_Total = 0, "refusal empty");
         end if;
      end loop;
   end loop;
   Check (Survived > 0, "some corruptions are still valid");

   Put_Line ("program-descriptions:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Main;
