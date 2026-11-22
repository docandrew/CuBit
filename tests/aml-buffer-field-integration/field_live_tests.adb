with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Field_Live_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (32, Max_Retained_Roots => 2,
     Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type Integer_Value;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type C.Value_Handle;
   use type AML_Execute.Execution_Status;
   A : C.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Args : constant C.Arguments := [others => (C.Immediate_Argument, 0)];
   R : C.Result;
   Status : C.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Keep : C.Value_Handle := C.No_Value;
   Checks : Natural := 0;
   function Name (Text : String) return Bytes is
      Data : Bytes (1 .. Text'Length);
   begin
      for I in Data'Range loop Data (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return Data;
   end Name;
   function Method (Text : String; Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Name (Text) & Bytes'(1 => 0) & Code);
   Buffer_2 : constant Bytes := [16#11#,5,16#0A#,2,16#81#,16#42#];
   Collect : constant Bytes := [16#70#,16#0D#,88,0,16#61#];
   Fixed : constant Bytes := [16#8C#] & Buffer_2 & [0] & Name ("FLD0");
   Generic_Field : constant Bytes := [16#5B#,16#13#] & Buffer_2 & [1,16#0A#,9] & Name ("FLD0");
   Fixture : constant Bytes :=
     Method ("FIXD", Fixed & Collect & [16#70#,16#0A#,90] & Name ("FLD0") & [16#A4#] & Name ("FLD0")) &
     Method ("GENR", Generic_Field & Collect & [16#A4#] & Name ("FLD0")) &
     Method ("REFR", Generic_Field & Collect & [16#A4#,16#83#,16#71#] & Name ("FLD0")) &
     Method ("COPY", Fixed & [16#9D#,16#0A#,42] & Name ("FLD0") & Collect & [16#A4#] & Name ("FLD0")) &
     Method ("ALIS", [16#70#] & Buffer_2 & [16#60#,16#8C#,16#60#,0] & Name ("FLD0") &
       [16#70#,1,16#60#] & Collect & [16#A4#] & Name ("FLD0")) &
     Method ("INTR", [16#8C#,16#0A#,65,0] & Name ("FLD0") & Collect & [16#A4#] & Name ("FLD0")) &
     Method ("STRR", [16#8C#,16#0D#,65,66,0,1] & Name ("FLD0") & Collect & [16#A4#] & Name ("FLD0")) &
     Method ("TYPR", Fixed & [16#A4#,16#8E#] & Name ("FLD0"));
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & R.Status'Image &
        (if R.Status = AML_Execute.Returned then R.Number'Image else ""); end if;
   end Check;
   procedure Bytes_Are (Handle : C.Value_Handle) is
      Data : Bytes (17 .. 20);
      Copied : Natural;
   begin
      C.Read_Bytes (A, Handle, 0, Data, Copied, Status);
      Check (Status = C.Available and then Copied = 2 and then Data = Bytes'[64,1,0,0]);
   end Bytes_Are;
begin
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available);
      C.Load (A, Fixture, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded);
      C.Seal (A, Report, Status); Check (Status = C.Available);
      for Node in N.Node_ID range 1 .. 8 loop
         declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
            C.Invoke (A, Input, Node, Args, 0, 100, R, Status);
            Check (Status = C.Available);
            Check (C.Reclamation_Metrics (A).Completed > Before and then C.Reclamation_Metrics (A).Rejected = 0);
            Check (C.Observe_Usage (A).Nodes = 8);
            if Node in 2 | 3 then
               Check (R.Status = AML_Execute.Object_Returned);
               Bytes_Are (R.Handle);
               if Node = 2 then Keep := R.Handle;
               else declare H : C.Value_Handle := R.Handle; begin
                  C.Release (A, H, Status); Check (Status = C.Available and then H = C.No_Value);
               end; end if;
            else
               Check (R.Status = AML_Execute.Returned and then R.Number =
                 (case Node is when 1 => 90, when 4 => 42, when 5 => 129,
                   when 6 => 65, when 7 => 66, when others => 14));
            end if;
            if Keep /= C.No_Value then Bytes_Are (Keep); end if;
         end;
      end loop;
      -- Retained field result survives field cleanup and subsequent GC; then
      -- another result occupies the final pin and a third is denied up front.
      C.Invoke (A, Input, 2, Args, 0, 100, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Object_Returned);
      declare Second : C.Value_Handle := R.Handle; begin
         C.Invoke (A, Input, 2, Args, 0, 100, R, Status);
         Check (Status = C.Root_Limit);
         Bytes_Are (Keep); Bytes_Are (Second);
         C.Release (A, Second, Status); Check (Status = C.Available);
      end;
      C.Release (A, Keep, Status); Check (Status = C.Available);
      Check (C.Retained_Count (A) = 0);
   end loop;
   Ada.Text_IO.Put_Line ("field live collecting checks" & Checks'Image);
end Field_Live_Tests;
