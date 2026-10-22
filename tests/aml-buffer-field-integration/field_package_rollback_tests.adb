with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute; use AML_Execute;
with AML_Namespace;
with AML_Objects;
with AML_References;
with AML_Table_Backing;
procedure Field_Package_Rollback_Tests is
   use type AML_Decode.Bytes;
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace (16, AML_Delays.Unavailable_Provider, Clock);
   use N; use N.Owned;
   use type AML_Objects.State;
   use type AML_Objects.Allocation_Status;
   use type AML_References.Node_Incarnation;
   A : Arena;
   Before : State;
   Input : aliased AML_Table_Backing.State (1, 1);
   Remaining : Natural := 0;
   Running : Boolean := False;
   Captured : Boolean := False;
   Checks : Natural := 0;
   R : Execution_Result;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & R.Status'Image; end if;
   end Check;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      ID : AML_Objects.Object_ID;
      Allocated : AML_Objects.Allocation_Status;
   begin
      Value := 0; Available := True;
      if not Running then return; end if;
      if Remaining > 0 then
         while Values_Used (A).Objects < AML_Objects.Max_Objects - Remaining loop
            Append (A, Bytes'(1 .. 0 => 0), ID, Allocated);
            Check (Allocated = AML_Objects.Allocated);
         end loop;
      end if;
      Before := Snapshot (A); Captured := True;
   end Clock;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Data;
   end Name;
   function Method (Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Name ("TEST") & Bytes'(1 => 0) & Code);
   Data : constant Bytes := [16#11#,5,16#0A#,2,65,66];
   Loaded_Status : Load_Status;
   OK : Boolean;
begin
   for Width in Integer_Width loop
      for Named in Boolean loop
         for Generic_Field in Boolean loop
            for Failure in Natural range 0 .. 2 loop
               declare
                  Field_Code : constant Bytes :=
                    (if Generic_Field then [16#5B#,16#13#] & Data & [0,16#0A#,8]
                     else [16#8C#] & Data & [0]) & Name ("FLD0");
                  -- Failure0: first capture succeeds, second name is missing.
                  -- Failure1: no slot for first captured leaf after package.
                  -- Failure2: first capture succeeds, second leaf hits quota.
                  Package_Value : constant Bytes := [16#12#,10,2] & Name ("FLD0") &
                    Name ((if Failure = 0 then "MISS" else "FLD0"));
                  Package_Code : constant Bytes :=
                    (if Named then [16#08#] & Name ("PKG0") & Package_Value
                     else [16#70#] & Package_Value & [16#60#]);
                  Code : constant Bytes := Field_Code & [16#5B#,16#33#] & Package_Code & [16#A4#,0];
               begin
                  Reset (A, OK); Check (OK);
                  Load (A, Method (Code), Width, Loaded_Status); Check (Loaded_Status = Loaded);
                  Remaining := Failure;
                  Running := True; Captured := False;
                  Invoke (A, Input, 1, [others => <>], 0, 200, R);
                  Running := False;
                  Check (Captured);
                  Check (R.Status = (if Failure = 0 then Unsupported_Value else Value_Limit));
                  Check (Value_Store (Snapshot (A)) = Value_Store (Before));
                  Check (Last_Incarnation (Snapshot (A)) = Last_Incarnation (Before));
                  Check (Count (Snapshot (A)) = 1);
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("field package rollback checks" & Checks'Image);
end Field_Package_Rollback_Tests;
