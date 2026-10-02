pragma Ada_2022;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Objects;
with Namespace_Instance;
procedure Package_Count_Tests is
   package NS renames Namespace_Instance;
   use NS;
   use type Byte;
   use type Integer_Value;
   Tree : State;
   Loaded : Load_Status;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   function Enc (Text : String) return Bytes is
      Value : Bytes (1 .. Text'Length);
   begin
      for I in Value'Range loop Value (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Value;
   end Enc;
   function Count_Name (Size : Natural) return Bytes is
     ([8] & Enc ("CNT0") & [16#0C#, Byte (Size mod 256), Byte ((Size / 256) mod 256), 0, 0]);
   function Package_Name (Count : Bytes; Elements : Bytes := []) return Bytes is
      Extent : constant Natural := 2 + Count'Length + Elements'Length;
   begin
      return [8] & Enc ("PKG0") & [16#13#, Byte (16#40# + Extent mod 16), Byte (Extent / 16)] & Count & Elements;
   end Package_Name;
   type Sizes is array (Positive range <>) of Natural;
begin
   for W in Integer_Width loop
      for Size of Sizes'(0, 1, 2, 255, 4096, 4097,
                        AML_Objects.Max_Elements, AML_Objects.Max_Elements + 1) loop
         Tree := Empty;
         Load_Names (Tree, Count_Name (Size), W, Loaded);
         Check (Loaded = NS.Loaded);
         declare
            Before : constant State := Tree;
         begin
            Load_Names (Tree, Package_Name (Enc ("CNT0"), (if Size > 0 then [1] else [])), W, Loaded);
            if Size > AML_Objects.Max_Elements then
               Check (Loaded = Value_Limit and Tree = Before);
            else
               Check (Loaded = NS.Loaded);
               declare
                  Store : constant AML_Objects.State := Value_Store (Tree);
                  ID : constant AML_Objects.Object_ID := Data_Object (Tree, 2);
               begin
                  Check (AML_Objects.Length (Store, ID) = Size);
                  if Size > 0 then
                     Check (AML_Objects.Integer_Data (Store, AML_Objects.Element (Store, ID, 0)) = 1);
                  end if;
                  for I in 2 .. Size loop Check (AML_Objects.Element (Store, ID, I - 1) = 0); end loop;
               end;
            end if;
         end;
      end loop;
   end loop;
   Tree := Empty;
   Load_Names (Tree, Count_Name (3), Bits_64, Loaded);
   declare
      Before : constant State := Tree;
      P : constant Bytes := Package_Name (Enc ("CNT0"), [1]);
   begin
      for Cut in 1 .. P'Length - 1 loop
         Load_Names (Tree, P (1 .. Cut), Bits_64, Loaded);
         Check (Loaded /= NS.Loaded and Tree = Before);
      end loop;
      Load_Names (Tree, Package_Name (Enc ("MISS")), Bits_64, Loaded);
      Check (Loaded = Bad_Package and Tree = Before);
      Load_Names (Tree, Package_Name ([16#5E#] & Enc ("CNT0")), Bits_64, Loaded);
      Check (Loaded = Bad_Package and Tree = Before);
      Load_Names (Tree, Package_Name ([16#5C#] & Enc ("CNT0")), Bits_64, Loaded);
      Check (Loaded = NS.Loaded);
   end;
   Tree := Empty;
   Load_Names (Tree, [8] & Enc ("CNT0") & [16#0D#, 51, 0], Bits_64, Loaded);
   declare
      Before : constant State := Tree;
   begin
      Load_Names (Tree, Package_Name (Enc ("CNT0")), Bits_64, Loaded);
      Check (Loaded = Bad_Package and Tree = Before);
   end;
   Ada.Text_IO.Put_Line ("AML-PACKAGE-COUNT-CHECK: PASS" & Checks'Image);
end Package_Count_Tests;
