with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with Namespace_Instance;
procedure Load_Tests is
   package NS renames Namespace_Instance;
   use NS;
   use type Integer_Value;
   Tree : State := Empty;
   Before : State;
   Result : Load_Status;
   Node : Node_ID;
   Added : Insert_Status;
   Checks : Natural := 0;
   Data : constant Bytes :=
     [16#08#, 16#54#, 16#45#, 16#53#, 16#54#, 16#0B#, 16#34#, 16#12#,
      16#08#, 16#5C#, 16#4F#, 16#4E#, 16#45#, 16#53#, 16#FF#];
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for W in Integer_Width loop
      Tree := Empty;
      Load_Names (Tree, Data, W, Result);
      Check (Result = Loaded and then Count (Tree) = 2);
      Check (Has_Integer (Tree, 1) and then Integer_Data (Tree, 1) = 16#1234#);
      Check (Integer_Data (Tree, 2) =
        (if W = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      Before := Tree;
      Load_Names (Tree, Data, W, Result);
      Check (Result = Duplicate_Name and then Tree = Before);
   end loop;
   for N in 1 .. Data'Length - 1 loop
      Tree := Empty;
      Before := Tree;
      Load_Names (Tree, Data (1 .. N), Bits_64, Result);
      if N = 8 then
         Check (Result = Loaded and then Count (Tree) = 1);
      else
         Check (Result /= Loaded and then Tree = Before);
      end if;
   end loop;
   Tree := Empty;
   Load_Names (Tree, Data & [16#5B#,16#80#], Bits_64, Result);
   Check (Result = Unsupported_Opcode and then Count (Tree) = 0);
   Load_Names (Tree, [16#08#,0,0], Bits_64, Result);
   Check (Result = Bad_Name and then Count (Tree) = 0);
   Insert (Tree, 0, "_SB_", Node, Added);
   Check (Added = Inserted);
   Load_Names (Tree,
     [16#08#,16#2E#,16#5F#,16#53#,16#42#,16#5F#,
      16#54#,16#45#,16#53#,16#54#,1], Bits_64, Result);
   Check (Result = Loaded and then Parent (Tree, 2) = 1
          and then Integer_Data (Tree, 2) = 1);
   for I in 3 .. 128 loop
      Insert (Tree, I - 1, "FILL", Node, Added);
   end loop;
   Before := Tree;
   Load_Names (Tree, Data, Bits_64, Result);
   Check (Result = Storage_Full and then Tree = Before);
   Tree := Empty;
   Load_Names (Tree, [Positive'Last - 5 => 16#08#,
     Positive'Last - 4 => 16#54#, Positive'Last - 3 => 16#45#,
     Positive'Last - 2 => 16#53#, Positive'Last - 1 => 16#54#,
     Positive'Last => 1], Bits_64, Result);
   Check (Result = Loaded and then Integer_Data (Tree, 1) = 1);
   --  Mutate every byte position of a two-definition block. Successful
   --  alternative encodings are legal; every rejection must be atomic.
   for I in Data'Range loop
      for B in Byte loop
         declare
            Mutant : Bytes := Data;
         begin
            Mutant (I) := B;
            Tree := Empty;
            Before := Tree;
            Load_Names (Tree, Mutant, Bits_64, Result);
            Check (Result = Loaded or else Tree = Before);
         end;
      end loop;
   end loop;
   declare
      Device_Data : constant Bytes :=
        [16#5B#,16#82#,11,16#44#,16#45#,16#56#,16#30#,
         8,16#56#,16#41#,16#4C#,16#30#,1];
      Scope_Data : constant Bytes :=
        [16#10#,11,16#44#,16#45#,16#56#,16#30#,
         8,16#56#,16#41#,16#4C#,16#31#,0];
      Nested : Bytes (1 .. 13) := Device_Data;
   begin
      Tree := Empty;
      Load_Names (Tree, Device_Data & Scope_Data, Bits_64, Result);
      Check (Result = Loaded and then Count (Tree) = 3
             and then Kind (Tree, 0) = Scope_Object
             and then Kind (Tree, 1) = Device_Object
             and then Kind (Tree, 2) = Integer_Object
             and then Parent (Tree, 2) = 1 and then Parent (Tree, 3) = 1
             and then Integer_Data (Tree, 2) = 1 and then Integer_Data (Tree, 3) = 0);
      for N in 1 .. Device_Data'Length - 1 loop
         Tree := Empty;
         Load_Names (Tree, Device_Data (1 .. N), Bits_64, Result);
         Check (Result /= Loaded and then Count (Tree) = 0);
      end loop;
      --  End package before the integer operand, despite a valid trailing
      --  byte in the enclosing table. Must not read through the package end.
      Nested (3) := 10;
      Tree := Empty;
      Load_Names (Tree, Nested, Bits_64, Result);
      Check (Result = Bad_Integer and then Count (Tree) = 0);
      Tree := Empty;
      Load_Names (Tree, Scope_Data, Bits_64, Result);
      Check (Result = Missing_Scope and then Count (Tree) = 0);
   end;
   --  Nested Scope(root) packages need no namespace objects. Build proper
   --  two-byte lengths to exercise the stack limit separately from capacity.
   declare
      Buffer_Data : Bytes (1 .. 1024) := [others => 0];
      Used : Natural := 0;
      Extent : Natural;
   begin
      for Depth in 1 .. 65 loop
         for I in reverse 1 .. Used loop
            Buffer_Data (I + 5) := Buffer_Data (I);
         end loop;
         Extent := Used + 4;
         Buffer_Data (1) := 16#10#;
         Buffer_Data (2) := Byte (16#40# + Extent mod 16);
         Buffer_Data (3) := Byte (Extent / 16);
         Buffer_Data (4) := 16#5C#;
         Buffer_Data (5) := 0;
         Used := Used + 5;
         Tree := Empty;
         Load_Names (Tree, Buffer_Data (1 .. Used), Bits_64, Result);
         Check ((if Depth <= 64 then Result = Loaded else Result = Nesting_Limit)
                and then Count (Tree) = 0);
      end loop;
   end;
   Ada.Text_IO.Put_Line ("AML-LOAD-CHECK: PASS" & Checks'Image);
end Load_Tests;
