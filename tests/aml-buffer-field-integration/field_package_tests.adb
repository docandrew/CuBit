with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Field_Package_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (32, Max_Retained_Roots => 2,
     Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type Byte;
   use type Integer_Value;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant C.Arguments := [others => (C.Immediate_Argument, 0)];
   R : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Data;
   end Name;
   function Method (Code : Bytes) return Bytes is
      Size : constant Natural := Code'Length + 5;
   begin
      if Size + 1 < 64 then
         return [16#14#, Byte (Size + 1)] & Name ("TEST") & Bytes'(1 => 0) & Code;
      end if;
      return [16#14#, 16#40# + Byte ((Size + 2) mod 16), Byte ((Size + 2) / 16)] &
        Name ("TEST") & Bytes'(1 => 0) & Code;
   end Method;
   Data : constant Bytes := [16#11#,5,16#0A#,2,65,66];
   Package_Value : constant Bytes := [16#12#,6,1] & Name ("FLD0");
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & R.Status'Image; end if;
   end Check;
begin
   for Width in Integer_Width loop
      for Named in Boolean loop
         for Generic_Field in Boolean loop
            declare
               Field_Code : constant Bytes :=
                 (if Generic_Field then [16#5B#,16#13#] & Data & [0,16#0A#,8]
                  else [16#8C#] & Data & [0]) & Name ("FLD0");
               Package_Code : constant Bytes :=
                 (if Named then [16#08#] & Name ("PKG0") & Package_Value
                  else [16#70#] & Package_Value & [16#60#]);
               Package_Source : constant Bytes := (if Named then Name ("PKG0") else [16#60#]);
               -- Snapshot the field into a package, overwrite its backing, and
               -- convert the captured element to Integer after further GC.
               Code : constant Bytes := Field_Code & Package_Code &
                 [16#70#,16#0A#,66] & Name ("FLD0") &
                 [16#70#,16#0D#,88,0,16#61#,16#A4#,16#99#,16#83#,16#88#] &
                 Package_Source & [0,0,0];
            begin
               C.Reset (A, Status); Check (Status = C.Available);
               C.Load (A, Method (Code), Width, Loaded, Status);
               Check (Status = C.Available and then Loaded = N.Loaded);
               C.Seal (A, Report, Status); Check (Status = C.Available);
               declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
                  C.Invoke (A, Input, 1, Args, 0, 200, R, Status);
                  Check (Status = C.Available);
                  Check (R.Status = AML_Execute.Returned and then R.Number = 65);
                  Check (C.Reclamation_Metrics (A).Completed > Before and then
                    C.Reclamation_Metrics (A).Rejected = 0);
                  Check (C.Observe_Usage (A).Nodes = 1);
               end;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("field package checks" & Checks'Image);
end Field_Package_Tests;
