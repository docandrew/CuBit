with Ada.Text_IO;
with AML_Data;
with AML_Decode; use AML_Decode;
with AML_Objects; use AML_Objects;
with Namespace_Instance;
procedure Data_Tests is
   use type Integer_Value;
   Store : State := Empty;
   Before : State;
   ID : Object_ID;
   Used : Natural;
   Result : Status;
   Checks : Natural := 0;
   function Nested (Depth : Natural) return Bytes is
   begin
      if Depth = 0 then return [1 => 1]; end if;
      declare
         Inner_Data : constant Bytes := Nested (Depth - 1);
         Small : constant Boolean := Inner_Data'Length + 2 <= 63;
         Extent : constant Natural := Inner_Data'Length + (if Small then 2 else 3);
      begin
         if Small then return Bytes'[16#12#, Byte (Extent), 1] & Inner_Data; end if;
         return Bytes'[16#12#, Byte (16#40# + Extent mod 16), Byte (Extent / 16), 1] & Inner_Data;
      end;
   end Nested;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Checks'Image; end if;
   end Check;
   --  Four elements: integer, string, nested package and uninitialized tail.
   Data : constant Bytes := [16#12#, 12, 4, 16#0A#, 42, 16#0D#, 65, 0,
                             16#12#, 4, 2, 1, 0];
begin
   AML_Data.Load (Store, Data, Bits_64, ID, Used, Result);
   Check (Result = Accepted and Used = Data'Length and Kind (Store, ID) = Package_Object);
   Check (Length (Store, ID) = 4 and Element (Store, ID, 3) = 0);
   Check (Integer_Data (Store, Element (Store, ID, 0)) = 42);
   Check (Byte_Data (Store, Element (Store, ID, 1)) = [1 => 65]);
   declare
      Nested : constant Object_ID := Element (Store, ID, 2);
   begin
      Check (Length (Store, Nested) = 2);
      Check (Integer_Data (Store, Element (Store, Nested, 0)) = 1);
      Check (Integer_Data (Store, Element (Store, Nested, 1)) = 0);
   end;
   Before := Store;
   for Last in 0 .. Data'Length - 1 loop
      AML_Data.Load (Store, Data (1 .. Last), Bits_64, ID, Used, Result);
      Check (Result /= Accepted and Store = Before and ID = 0 and Used = 0);
   end loop;
   AML_Data.Load (Store, [16#12#, 3, 0, 1], Bits_64, ID, Used, Result);
   Check (Result = Malformed and Store = Before);
   AML_Data.Load (Store, [16#13#, 4, 16#0A#, 3, 1], Bits_64, ID, Used, Result);
   Check (Result = Accepted and Length (Store, ID) = 3 and Element (Store, ID, 2) = 0);
   AML_Data.Load (Store, [16#12#, 2, 0, 16#FF#], Bits_64, ID, Used, Result);
   Check (Result = Accepted and Used = 3 and Length (Store, ID) = 0);
   AML_Data.Load (Store, [Positive'Last - 2 => 16#12#, Positive'Last - 1 => 2,
                         Positive'Last => 0], Bits_64, ID, Used, Result);
   Check (Result = Accepted and Used = 3);
   for Depth in 1 .. 65 loop
      Store := Empty;
      Before := Store;
      AML_Data.Load (Store, Nested (Depth), Bits_64, ID, Used, Result);
      if Depth <= 64 then
         Check (Result = Accepted and Live_Count (Store) = Depth + 1);
         for J in 1 .. Depth loop
            Check (Kind (Store, ID) = Package_Object and Length (Store, ID) = 1);
            ID := Element (Store, ID, 0);
         end loop;
         Check (Integer_Data (Store, ID) = 1);
      else
         Check (Result = Limit_Exceeded and Store = Before);
      end if;
   end loop;
   Store := Empty;
   AML_Data.Load (Store, [16#12#, 8, 2, 16#11#, 4, 16#0A#, 2, 42, 1], Bits_64, ID, Used, Result);
   Check (Result = Accepted and Byte_Data (Store, Element (Store, ID, 0)) = [42, 0]);
   Before := Store;
   AML_Data.Load (Store, [16#12#, 6, 2, 1, 65, 66, 67], Bits_64, ID, Used, Result);
   Check (Result /= Accepted and Store = Before);
   Store := Empty;
   AML_Data.Load (Store, [16#13#, 4, 16#0B#,
                         Byte (Max_Elements mod 256), Byte (Max_Elements / 256)],
                  Bits_64, ID, Used, Result);
   Check (Result = Accepted and Length (Store, ID) = Max_Elements);
   Before := Store;
   AML_Data.Load (Store, [16#12#, 2, 1], Bits_64, ID, Used, Result);
   Check (Result = Limit_Exceeded and Store = Before);
   Store := Empty;
   declare
      Allocation : Allocation_Status;
   begin
      for J in 1 .. Max_Objects - 1 loop
         New_Integer (Store, Integer_Value (J), ID, Allocation);
         Check (Allocation = Allocated);
      end loop;
   end;
   Before := Store;
   AML_Data.Load (Store, [16#12#, 3, 1, 1], Bits_64, ID, Used, Result);
   Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
   declare
      package NS renames Namespace_Instance;
      use type NS.Load_Status;
      Tree : NS.State := NS.Empty;
      Loaded : NS.Load_Status;
   begin
      NS.Load_Names (Tree, Bytes'[16#08#, 80, 75, 71, 48] & Data, Bits_64, Loaded);
      Check (Loaded = NS.Loaded);
      Store := NS.Value_Store (Tree);
      ID := NS.Data_Object (Tree, 1);
      Check (Kind (Store, ID) = Package_Object and Length (Store, ID) = 4);
   end;
   Ada.Text_IO.Put_Line ("AML-DATA-CHECK: PASS" & Checks'Image);
end Data_Tests;
