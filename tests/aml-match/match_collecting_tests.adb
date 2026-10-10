with Ada.Text_IO;
with AML_Delays;
with AML_Decode; use AML_Decode;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Match_Collecting_Tests is
   package N is new AML_Namespace (24, Max_Retained_Roots => 8,
     Perform_Delay => AML_Delays.Unavailable_Provider);
   package C is new N.Owned.Collecting;
   use type N.Load_Status;
   use type C.Access_Status;
   use type C.Collection_Count;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
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
   function Method (Text : String; Code : Bytes) return Bytes is
     ([16#14#, Byte (Code'Length + 6)] & Name (Text) & Bytes'(1 => 0) & Code);
   Fixture : constant Bytes :=
     Method ("SRC0", [16#A4#,16#12#,4,2,1,1]) &
     Method ("ONE0", [16#A4#,16#11#,3,1,1]) &
     Method ("TWO0", [16#A4#,16#11#,3,1,2]) &
     Method ("STRT", [16#70#,16#0D#,88,0,16#60#,16#A4#,0]) &
     Method ("TEST", [16#A4#,16#89#] & Name ("SRC0") & Bytes'(1 => 4) &
       Name ("ONE0") & Bytes'(1 => 2) & Name ("TWO0") & Name ("STRT")) &
     Method ("BND0", [16#A4#,16#89#,16#12#,3,1,1,0,0,0,0,1]) &
     Method ("BAD0", [16#A4#,16#89#,16#12#,3,1,1,6,0,0,0,0]) &
     Method ("ZERO", [16#A4#,16#89#,16#12#,3,1,1,0,0,0,0,0]) &
     Method ("NONE", [16#A4#,16#89#,16#12#,3,1,1,1,0,0,0,0]) &
     Method ("TWOX", [16#A4#,16#89#,16#12#,4,2,1,1,1,0,0,0,0]) &
     Method ("SELM", [16#08#] & Name ("PKG0") & [16#12#,6,1] & Name ("PKG0") &
       [16#A4#,16#89#] & Name ("PKG0") & [0,0,0,0,0]) &
     Method ("SELE", [16#08#] & Name ("PKG0") & [16#12#,6,1] & Name ("PKG0") &
       [16#A4#,16#89#] & Name ("PKG0") & [1,1,0,0,0]);
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image & R.Status'Image; end if;
   end Check;
begin
   for Width in Integer_Width loop
      C.Reset (A, Status); Check (Status = C.Available);
      C.Load (A, Fixture, Width, Loaded, Status); Check (Status = C.Available and then Loaded = N.Loaded);
      C.Seal (A, Report, Status); Check (Status = C.Available);
      declare Before : constant C.Collection_Count := C.Reclamation_Metrics (A).Completed; begin
         C.Invoke (A, Input, 5, Args, 0, 100, R, Status);
         Check (Status = C.Available and then R.Status = AML_Execute.Returned and then R.Number = 0);
         Check (C.Reclamation_Metrics (A).Completed > Before and then C.Reclamation_Metrics (A).Rejected = 0);
      end;
      C.Invoke (A, Input, 6, Args, 0, 100, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Package_Limit);
      C.Invoke (A, Input, 7, Args, 0, 100, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Invalid_Match_Operation);
      -- Return, Match, Package, two Zero matchobjects, Zero start, one visit.
      C.Invoke (A, Input, 8, Args, 0, 7, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Returned and then R.Number = 0 and then R.Charged = 7);
      C.Invoke (A, Input, 8, Args, 0, 6, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Budget_Exceeded and then R.Charged = 6);
      C.Invoke (A, Input, 9, Args, 0, 7, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Returned
        and then R.Number = (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last)
        and then R.Charged = 7);
      C.Invoke (A, Input, 10, Args, 0, 8, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Returned and then R.Charged = 8);
      C.Invoke (A, Input, 10, Args, 0, 7, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Budget_Exceeded and then R.Charged = 7);
      C.Invoke (A, Input, 11, Args, 0, 100, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Returned and then R.Number = 0);
      C.Invoke (A, Input, 12, Args, 0, 100, R, Status);
      Check (Status = C.Available and then R.Status = AML_Execute.Returned
        and then R.Number = (if Width = Bits_32 then 16#FFFF_FFFF# else Integer_Value'Last));
      declare Code : constant Bytes := [16#A4#,16#89#,16#12#,3,1,1,0,0,0,0,0]; begin
         for Last in 2 .. Code'Last - 1 loop
            C.Reset (A, Status); Check (Status = C.Available);
            C.Load (A, Method ("TRNC", Code (Code'First .. Last)), Width, Loaded, Status);
            Check (Status = C.Available and then Loaded = N.Loaded);
            C.Seal (A, Report, Status); Check (Status = C.Available);
            C.Invoke (A, Input, 1, Args, 0, 100, R, Status);
            -- A present Package length extending beyond the enclosing bytes is
            -- Bad_Package; absent length or later Match operands are Truncated.
            Check (Status = C.Available and then R.Status =
              (if Last in 4 .. 5 then AML_Execute.Bad_Package else AML_Execute.Truncated));
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Match collecting checks" & Checks'Image);
end Match_Collecting_Tests;
