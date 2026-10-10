with AML_Delays;
with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_References;
with AML_Table_Backing;
procedure Tointeger_Owned_Tests is
   package NS is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
   use NS; use NS.Owned;
   use type Integer_Value;
   A : Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Result : Execution_Result;
   Loaded_Status : Load_Status;
   OK : Boolean;
   Checks : Natural := 0;

   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image & Result.Status'Image; end if;
   end Check;
   procedure Run (Code : Bytes; Width : Integer_Width) is
   begin
      Reset (A, OK); Check (OK);
      Load (A, Bytes'(16#14#, Byte (6 + Code'Length),84,69,83,84,0) & Code,
            Width, Loaded_Status);
      Check (Loaded_Status = Loaded);
      Invoke (A, Input, 1, [others => <>], 0, 100, Result);
   end Run;
begin
   for Width in Integer_Width loop
      Run (Bytes'(16#A4#,16#99#,16#0D#,49,48,0,0), Width);
      Check (Result.Status = Returned and then Result.Value = 10
        and then Result.Origin = Ordinary_Integer);
      Run (Bytes'(16#A4#,16#99#,1,0), Width);
      Check (Result.Status = Returned and then Result.Value = 1
        and then Result.Origin = AML_Constant);
      Run (Bytes'(16#A4#,16#99#,16#11#,4,1,16#AA#,16#BB#,0), Width);
      Check (Result.Status = Returned and then Result.Value = 16#BBAA#);
      Run (Bytes'(16#A4#,16#99#,16#11#,2,0,0), Width);
      Check (Result.Status = Empty_Buffer);
      Run (Bytes'(16#A4#,16#99#,16#60#,0), Width);
      Check (Result.Status = Uninitialized);
      Run (Bytes'(16#A4#,16#0D#,49,48,0), Width);
      Check (Result.Status = Object_Returned);
      declare
         Item : Datum := (Value_Kind => Object_Datum, Object => Result.Object);
         Before : constant Datum := Item;
         Status : Execution_Status;
      begin
         Reset (A, OK); Check (OK);
         Convert_To_Integer (A, Width, Item, Status);
         Check (Status = Unsupported_Value and then Item = Before);
         Item := (Value_Kind => Reference_Datum, Ref => AML_References.No_Reference);
         declare Saved : constant Datum := Item; begin
            Convert_To_Integer (A, Width, Item, Status);
            Check (Status = Unsupported_Value and then Item = Saved);
         end;
      end;
      declare
         Item : Datum := (Value_Kind => Integer_Datum, Number => 1, Origin => AML_Constant);
         Status : Execution_Status;
      begin
         Convert_To_Integer (A, Width, Item, Status);
         Check (Status = Returned and then Item.Number = 1 and then Item.Origin = AML_Constant);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ToInteger owned checks" & Checks'Image);
end Tointeger_Owned_Tests;
