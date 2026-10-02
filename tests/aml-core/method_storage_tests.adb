pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with Namespace_Instance;
procedure Method_Storage_Tests is
   package NS renames Namespace_Instance;
   use NS;
   use type Byte;
   use type Integer_Value;
   Checks : Natural := 0;
   Tree : State := Empty;
   Loaded : Load_Status;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Method (Name : String; Code : Bytes) return Bytes is
      -- Always use the legal three-byte PkgLength form, including small bodies.
      Extent : constant Natural := 3 + 4 + 1 + Code'Length;
      Header : Bytes (1 .. 9) :=
        [16#14#, Byte (16#80# + Extent mod 16), Byte ((Extent / 16) mod 256),
         Byte (Extent / 4096), 0, 0, 0, 0, 0];
   begin
      for I in 1 .. 4 loop Header (I + 4) := Character'Pos (Name (Name'First + I - 1)); end loop;
      return Header & Code;
   end Method;
   type Sizes is array (Positive range <>) of Natural;
begin
   for W in Integer_Width loop
      for Size of Sizes'(0, 1, 2, 1023, 1024, 1025, 1170, 4096, 16384, 65535, 65536) loop
         declare
            Code : Bytes (1 .. Size) := [others => 16#A3#];
            R : Execution_Result;
         begin
            if Size >= 2 then Code (Size - 1 .. Size) := [16#A4#, 1]; end if;
            Tree := Empty;
            Load_Names (Tree, Method ("LONG", Code), W, Loaded);
            Check (Loaded = NS.Loaded and Count (Tree) = 1);
            Check (Method_Usage (Tree) = Size and Method_Data (Tree, 1) = Code);
            R := Invoke (Tree, 1, [others => 0], 0, Size + 1);
            Check (R.Charged = Size);
            Check ((if Size >= 2 then R.Status = Returned and then R.Value = 1
                    else R.Status = No_Return));
            if Size > 0 then
               R := Invoke (Tree, 1, [others => 0], 0, Size - 1);
               Check (R.Status = Budget_Exceeded);
            end if;
            -- The namespace owns the bytes independently of caller storage.
            if Size > 0 then
               Code (1) := 16#FF#;
               Check (Method_Data (Tree, 1) (Method_Data (Tree, 1)'First) /= 16#FF#);
            end if;
         end;
      end loop;
   end loop;
   Tree := Empty;
   declare
      A : constant Bytes (1 .. 32000) := [others => 16#A3#];
      B : constant Bytes (1 .. 32000) := [others => 16#00#];
      C : constant Bytes (1 .. 1536) := [others => 16#FF#];
   begin
      Load_Names (Tree, Method ("AAAA", A), Bits_64, Loaded);
      Check (Loaded = NS.Loaded);
      Load_Names (Tree, Method ("BBBB", B), Bits_32, Loaded);
      Check (Loaded = NS.Loaded);
      Load_Names (Tree, Method ("CCCC", C), Bits_64, Loaded);
      Check (Loaded = NS.Loaded and Method_Usage (Tree) = Max_Method_Bytes);
      Check (Method_Data (Tree, 1) = A and Method_Data (Tree, 2) = B and Method_Data (Tree, 3) = C);
      declare
         Before : constant State := Tree;
      begin
         Load_Names (Tree, Method ("FAIL", [16#A3#]), Bits_64, Loaded);
         Check (Loaded = Value_Limit and Tree = Before);
         Load_Names (Tree, Method ("AAAA", []), Bits_64, Loaded);
         Check (Loaded = Duplicate_Name and Tree = Before);
      end;
      Load_Names (Tree, Method ("ZERO", []), Bits_64, Loaded);
      Check (Loaded = NS.Loaded and Method_Data (Tree, 4)'Length = 0);
   end;
   Tree := Empty;
   declare
      Before : constant State := Tree;
      Huge : constant Bytes (1 .. Max_Method_Bytes + 1) := [others => 16#A3#];
   begin
      Load_Names (Tree, Method ("HUGE", Huge), Bits_64, Loaded);
      Check (Loaded = Value_Limit and Tree = Before);
      Load_Names (Tree, Method ("GOOD", [16#A4#,1]) & [16#FF#], Bits_64, Loaded);
      Check (Loaded = Unsupported_Opcode and Tree = Before and Method_Usage (Tree) = 0);
   end;
   declare
      Encoded : constant Bytes := Method ("HIGH", [16#A4#,1]);
      High : Bytes (Positive'Last - Encoded'Length + 1 .. Positive'Last) := Encoded;
   begin
      Load_Names (Tree, High, Bits_64, Loaded);
      Check (Loaded = NS.Loaded and Method_Data (Tree, 1) = [16#A4#,1]);
      High (High'Last) := 0;
      Check (Method_Data (Tree, 1) = [16#A4#,1]);
   end;
   -- An empty method whose flags byte is at Positive'Last must not form
   -- Data'Last + 1 when copying its zero-byte body.
   declare
      Encoded : constant Bytes := Method ("EMPT", []);
      High : constant Bytes (Positive'Last - Encoded'Length + 1 .. Positive'Last) := Encoded;
   begin
      Tree := Empty;
      Load_Names (Tree, High, Bits_64, Loaded);
      Check (Loaded = NS.Loaded and Method_Data (Tree, 1)'Length = 0);
      Check (Method_Usage (Tree) = 0);
   end;
   Ada.Text_IO.Put_Line ("AML-METHOD-STORAGE-CHECK: PASS" & Checks'Image);
end Method_Storage_Tests;
