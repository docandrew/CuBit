pragma Ada_2022;
with Ada.Text_IO;
with AML_Data;
with AML_Decode;
with AML_Objects;
with AML_Objects.Construction_Test_Fixture;
procedure Parser_Tests is
   package O renames AML_Objects;
   use AML_Decode;
   use type Bytes, Integer_Value, Byte;
   use type O.State, O.Allocation_Status, O.Object_ID, O.Object_Kind;
   type Context is record
      Calls : Natural := 0;
      Borrowed : O.Object_ID := 0;
   end record;
   function Count (Environment : Context; Data : Bytes; Width : Integer_Width)
     return AML_Data.Count_Result is
      pragma Unreferenced (Environment);
      N : constant Integer_Result := Read_Integer (Data, Width);
   begin
      if N.Kind = Accepted then return (Accepted, N.Value, N.Consumed); end if;
      return (Kind => N.Kind, others => <>);
   end Count;
   procedure Member (Environment : in out Context; Package_ID : O.Object_ID;
                     Element : Natural; Data : Bytes; Result : out AML_Data.Member_Result) is
      pragma Unreferenced (Package_ID, Element);
   begin
      Environment.Calls := Environment.Calls + 1;
      if Data'Length >= 4 then
         Result := (Accepted, AML_Data.Resolved_Member, Environment.Borrowed, 4);
      else Result := (Kind => Truncated, others => <>); end if;
   end Member;
   procedure Load is new AML_Data.Load_Bound (Context, Count, Member);
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   Package_Data : constant Bytes :=
     [16#12#, 12, 4, 16#0A#, 42, 16#0D#, 65, 0, 16#12#, 4, 2, 1, 0];
   Ref_Data : constant Bytes := [16#12#, 6, 1, 82, 69, 70, 48];
   Bad_After_Ref : constant Bytes := [16#12#, 7, 2, 82, 69, 70, 48, 16#FE#];
   Bad_After_String : constant Bytes := [16#12#, 6, 2, 16#0D#, 65, 0, 16#FE#];
   Buffer_Data : constant Bytes := [16#11#, 5, 16#0A#, 4, 1, 2];
   function Nested (Depth : Natural) return Bytes is
   begin
      if Depth = 0 then return [1 => 1]; end if;
      declare Inner : constant Bytes := Nested (Depth - 1);
         Short : constant Boolean := Inner'Length + 2 <= 63;
         Extent : constant Natural := Inner'Length + (if Short then 2 else 3);
      begin
         if Short then return Bytes'[16#12#,Byte(Extent),1] & Inner; end if;
         return Bytes'[16#12#,Byte(16#40# + Extent mod 16),Byte(Extent / 16),1] & Inner;
      end;
   end Nested;
begin
   for Width in Integer_Width loop
      for Repeat in 1 .. 16 loop
         declare
            Store : O.State := O.Empty (Object_Capacity => 80, Byte_Capacity => 64, Element_Capacity => 80);
            Env : Context;
            ID, Old : O.Object_ID;
            Alloc : O.Allocation_Status;
            Used : Natural;
            Result : Status;
         begin
            O.New_Integer (Store, 99, Old, Alloc);
            O.Construction_Test_Fixture.Dirty_Unused (Store, Old);
            Env.Borrowed := Old;
            declare Before : constant O.State := Store; Prior : constant Context := Env; begin
               Load (Store, Bad_After_String, Width, Env, ID, Used, Result);
               Check (Result /= Accepted and ID = 0 and Used = 0);
               Check (Store = Before and Env = Prior);
               Load (Store, Bad_After_Ref, Width, Env, ID, Used, Result);
               Check (Result /= Accepted and ID = 0 and Used = 0);
               Check (Store = Before and Env = Prior);
               Env.Borrowed := 2;
               Load (Store, Ref_Data, Width, Env, ID, Used, Result);
               Check (Result = Malformed and ID = 0 and Used = 0);
               Check (Store = Before and Env.Calls = 0 and Env.Borrowed = 2);
            end;
            Env.Borrowed := Old;
            Load (Store, Ref_Data, Width, Env, ID, Used, Result);
            Check (Result = Accepted and Used = Ref_Data'Length and Env.Calls = 1);
            Check (O.Element (Store, ID, 0) = Old);
            Load (Store, Package_Data, Width, Env, ID, Used, Result);
            Check (Result = Accepted and Used = Package_Data'Length);
            Check (O.Kind (Store, ID) = O.Package_Object and O.Length (Store, ID) = 4);
            Check (O.Integer_Data (Store, O.Element (Store, ID, 0)) = 42);
            Check (O.Kind (Store, O.Element (Store, ID, 1)) = O.String_Object);
            Check (O.Element (Store, ID, 3) = 0);
            Load (Store, Buffer_Data, Width, Env, ID, Used, Result);
            Check (Result = Accepted and O.Length (Store, ID) = 4);
            Check (O.Stored_Byte (Store, ID, 0) = 1 and O.Stored_Byte (Store, ID, 3) = 0);
         end;
      end loop;
      declare
         Store : O.State := O.Empty (Object_Capacity => 80, Byte_Capacity => 3, Element_Capacity => 80);
         Before : constant O.State := Store;
         Env : Context;
         ID : O.Object_ID;
         Used : Natural;
         Result : Status;
      begin
         Load (Store, Buffer_Data, Width, Env, ID, Used, Result);
         Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
         Load (Store, Nested (65), Width, Env, ID, Used, Result);
         Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
         Load (Store, Nested (64), Width, Env, ID, Used, Result);
         Check (Result = Accepted and O.Valid (Store));
      end;
      declare
         Store : O.State := O.Empty (Object_Capacity => 8, Byte_Capacity => 8, Element_Capacity => 8);
         Env : Context;
         ID, Old : O.Object_ID;
         Alloc : O.Allocation_Status;
         Used : Natural;
         Result : Status;
      begin
         for I in 1 .. 3 loop O.New_Integer (Store, Integer_Value (I), Old, Alloc); end loop;
         O.Construction_Test_Fixture.Retire_Integer (Store, 2);
         O.Construction_Test_Fixture.Dirty_Unused (Store, 3);
         declare Before : constant O.State := Store; begin
            Env.Borrowed := 2;
            Load (Store, Ref_Data, Width, Env, ID, Used, Result);
            Check (Result = Malformed and Store = Before and Env.Calls = 0);
            Env.Borrowed := 3;
            Load (Store, Ref_Data, Width, Env, ID, Used, Result);
            Check (Result = Accepted and ID = 2 and O.Element (Store, ID, 0) = 3 and Env.Calls = 1);
         end;
         Load (Store, Bytes'[16#0D#,0], Width, Env, ID, Used, Result);
         Check (Result = Accepted and O.Kind (Store, ID) = O.String_Object and O.Length (Store, ID) = 0);
         Load (Store, Bytes'[16#11#,2,0], Width, Env, ID, Used, Result);
         Check (Result = Accepted and O.Kind (Store, ID) = O.Buffer_Object and O.Length (Store, ID) = 0);
         Load (Store, Bytes'[16#12#,2,0], Width, Env, ID, Used, Result);
         Check (Result = Accepted and O.Kind (Store, ID) = O.Package_Object and O.Length (Store, ID) = 0);
         Load (Store, Bytes'[16#13#,2,0], Width, Env, ID, Used, Result);
         Check (Result = Accepted and O.Kind (Store, ID) = O.Package_Object and O.Length (Store, ID) = 0);
      end;
      declare
         Store : O.State := O.Empty (Object_Capacity => 2, Byte_Capacity => 1, Element_Capacity => 1);
         ID, Old : O.Object_ID;
         Alloc : O.Allocation_Status;
         Env : Context;
         Used : Natural;
         Result : Status;
      begin
         O.New_Integer (Store, 1, Old, Alloc);
         O.Construction_Test_Fixture.Dirty_Unused (Store, Old);
         declare Before : constant O.State := Store; begin
            Load (Store, Bytes'[16#12#,3,1,1], Width, Env, ID, Used, Result);
            Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
            Load (Store, Bytes'[16#12#,6,1,16#0D#,65,66,0], Width, Env, ID, Used, Result);
            Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
            Load (Store, Bytes'[16#12#,5,1,16#12#,2,2], Width, Env, ID, Used, Result);
            Check (Result = Limit_Exceeded and Store = Before and ID = 0 and Used = 0);
         end;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Parser undo checks" & Checks'Image);
end Parser_Tests;
