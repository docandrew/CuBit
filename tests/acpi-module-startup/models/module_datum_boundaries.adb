pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Interfaces; use Interfaces;
with ACPI_Service_Core; with ACPI_FADT.Transactions; with AML_Delays;
with AML_Decode; with AML_Execute; with AML_Field_Access; with AML_Field_Layout;
with AML_Identity; with AML_Identity.Issuer; with Root_Test_Support; use Root_Test_Support;
procedure Module_Datum_Boundaries is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package A renames AML_Field_Access; package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes; use type Bytes;
   use type AML_Identity.Identity, A.Register_ID, A.Endpoint_ID, A.Observation;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Access_Status;
   use type C.Values.Module_Outcome, C.Values.Value_Handle, E.Execution_Status;
   Cell_Count : constant Positive := 9;
   subtype Cell_Index is Positive range 1 .. Cell_Count;
   type Cells is array (Cell_Index) of Unsigned_64;
   First_ID : constant A.Register_ID := 16#E000_0000_0000_0001#;
   function ID (Cell : Cell_Index) return A.Register_ID is (First_ID + A.Register_ID (Cell - 1));
   Input : constant Bytes := [16#88#,16#77#,16#66#,16#55#,16#44#,16#33#,16#22#,16#11#];
   Expected : constant Cells := [16#85#,16#78#,16#67#,16#56#,16#45#,16#34#,16#23#,16#12#,16#A1#];
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Data : Cells := [others => 16#A5#];
      Calls : Natural := 0;
      Deny_Last_Write, Deny_Last_Read, Unknown_Last : Boolean := False;
      Receipt : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 3, Base => 0, Length => Unsigned_64 (Cell_Count)),
        Byte_Offset => 0, Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner and then Target.Endpoint = 7
        and then Target.Epoch = 1 and then Target.Register in First_ID .. ID (Cell_Index'Last);
      if Accepted then Geometry.Byte_Offset := Unsigned_64 (Target.Register - First_ID); end if;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; Writing : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then (Target.Register /= ID (Cell_Index'Last)
        or else (if Writing then not IO.Deny_Last_Write else not IO.Deny_Last_Read));
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry; Allowed : Boolean;
   begin
      Reply := (others => <>); Describe (IO, Item.Destination, Geometry, Allowed);
      if not Allowed then return; end if;
      declare Cell : constant Cell_Index := Cell_Index (Item.Destination.Register - First_ID + 1); begin
         IO.Calls := IO.Calls + 1;
         if Item.For_Write then IO.Data (Cell) := Item.Value; end if;
         if Item.For_Write and IO.Unknown_Last and Cell = Cell_Index'Last then
            IO.Receipt := (True, IO.Owner, Item, 1);
            Reply := (A.Indeterminate, IO.Owner, 0, 1);
         else Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Data (Cell)), A.No_Operation);
         end if;
      end;
   end Dispatch;
   procedure Confirmed (IO : Context; Item : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Item);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize, Maximum_Bindings => Cell_Count + 1);
   Mode : constant String := Ada.Command_Line.Argument (1);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Table (Revision : Unsigned_8) return Bytes is
      Payload : constant Bytes := [16#08#] & Enc ("INP0") & Block ([16#11#], [16#0A#,8] & Input)
        & [16#08#] & Enc ("OUT0") & Block ([16#11#], [16#0A#,8])
        & [16#5B#,16#80#] & Enc ("REG0") & [3,0,16#0A#,AML_Decode.Byte (Cell_Count)]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1,0,4] & Enc ("PAIR") & [16#40#,4])
        & [16#70#] & Enc ("INP0") & Enc ("PAIR")
        & [16#70#] & Enc ("PAIR") & Enc ("OUT0");
      Header_Bytes : constant Positive := 36;
      Data : Bytes (1 .. Header_Bytes + Payload'Length) := [others => 0]; Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (Header_Bytes + 1 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         Service : aliased C.State (1,4096,4096, Load_Capacity => 1); Data : constant Bytes := Table (Revision);
         IO : Context; Targets : F.Module_Target_Set := [others => A.No_Target];
         Count : F.Module_Target_Count := F.Module_Target_Count (Cell_Count);
         Stage : C.Stage_Status; Frozen : C.Freeze_Status; Report : C.Execution_Report;
         Status : C.Values.Access_Status; Accepted : Boolean; Before : Natural;
         Initialized : C.Namespace.Initialization_Report; Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor; Sample : C.Values.Result; Handle : C.Values.Value_Handle;
         Copied : Bytes (1 .. Input'Length);
         Copied_Count : Natural;
      begin
         C.Stage_Table (Service,41,C.DSDT,Data,Stage); Check (Stage = C.Staged, "stage");
         C.Freeze_Catalog (Service,Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
         AML_Identity.Issuer.Issue (IO.Owner,Accepted); Check (Accepted, "provider identity");
         for Cell in Cell_Index loop Targets (Cell) :=
           (Provider => IO.Owner, Endpoint => 7, Register => ID (Cell), Epoch => 1); end loop;
         if Mode = "missing-last" then Targets (Cell_Count) := A.No_Target; Count := F.Module_Target_Count (Cell_Count - 1);
         elsif Mode = "ambiguous-last" then Targets (Cell_Count + 1) := Targets (Cell_Count); Count := F.Module_Target_Count (Cell_Count + 1);
         elsif Mode = "denied-last" then IO.Deny_Last_Write := True;
         elsif Mode = "read-denied-last" then IO.Deny_Last_Read := True;
         elsif Mode = "unknown-last" then IO.Unknown_Last := True;
         end if;
         F.Execute_Frozen (Service,IO,Targets,Count,10000,Report,Status);
         Check (Status = C.Values.Available and Report.Modules.Charged <= 10000, "bounded module report");
         if Mode = "normal" then
            Check (Report.Modules.Outcome = C.Values.Modules_Completed, "maximum datum plan completed");
            Check (IO.Data = Expected and IO.Calls = 20, "nine-cell write/read and edge preservation");
            C.Initialize_Members (Service,Initialized,Status); Check (Status = C.Values.Available, "seal");
            C.Find_Child (Service,C.Namespace.Root,"OUT0",Node,Status); Check (Status = C.Values.Available, "result node");
            C.Describe_Node (Service,Node,Item,Status); Check (Status = C.Values.Available, "result identity");
            C.Evaluate_Named (Service,Item,1000,Sample,Status);
            Check (Status = C.Values.Available and then Sample.Status = E.Object_Returned, "buffer destination returned");
            Handle := Sample.Handle; C.Read_Result_Bytes (Service,Handle,0,Copied,Copied_Count,Status);
            Check (Status = C.Values.Available and Copied_Count = Input'Length and Copied = Input, "all64bits preserved at both AML widths");
            C.Release_Result (Service,Handle,Status);
            Check (Status = C.Values.Available and Handle = C.Values.No_Value and C.Retained_Results (Service) = 0,
              "readback result released");
         elsif Mode = "unknown-last" then
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and Report.Modules.Failure = E.Field_Indeterminate,
              "last datum uncertainty stops module");
            Check (IO.Data = Expected and IO.Calls = 11 and Pending (IO) and F.Uncertain (Service) = IO.Receipt,
              "all possible writes and last receipt retained before readback");
         else
            Check (Report.Modules.Outcome = C.Values.Module_Rejected and IO.Calls = 0,
              "last-cell geometry and rights preflight before first effect");
            Check (IO.Data = Cells'[others => 16#A5#], "refusal leaves every cell untouched");
         end if;
         Before := IO.Calls; F.Execute_Frozen (Service,IO,Targets,Count,10000,Report,Status);
         Check (Status /= C.Values.Available and IO.Calls = Before, "maximum plan cannot replay");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Module datum boundary checks" & Checks'Image);
end Module_Datum_Boundaries;
