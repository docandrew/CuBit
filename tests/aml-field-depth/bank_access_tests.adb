pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with AML_Execution_Limits;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Bank_Access_Oracle;
procedure Bank_Access_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 512, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   type Slot is (Selector, Data);
   type Registers is array (Slot) of AML_Decode.Integer_Value;
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [16#8000_0000_0000_0011#, 16#9000_0000_0000_0022#];
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Values : Registers := [others => 0];
      Calls : Natural := 0;
   end record;
   function Pending (IO : Context) return Boolean is
      pragma Unreferenced (IO);
   begin return False; end Pending;
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 2),
        Byte_Offset => (if Target.Register = IDs (Selector) then 0 else 1),
        Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := IO.Owner /= AML_Identity.No_Identity and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then (Target.Register = IDs (Selector) or else Target.Register = IDs (Data));
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      Geometry : AML_Field_Layout.Register_Geometry;
   begin Describe (IO, Target, Geometry, Accepted); end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Allowed : Boolean;
      S : constant Slot := (if Item.Destination.Register = IDs (Selector) then Selector else Data);
   begin
      Reply := (others => <>);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Allowed or else (Item.For_Write and then Item.Value > 255) then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then IO.Values (S) := Item.Value; end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Values (S)), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Maximum_Bindings => 2, Authorize_Target => Authorize);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Run (Deep : Boolean; Expanded : Boolean := False) is
      Depth : constant Positive := (if Deep then 256 else 128);
      Service : aliased C.State (1, Bank_Access_Oracle.Table256'Length, Bank_Access_Oracle.Table256'Length);
      IO : Context;
      Items : F.Binding_Set;
      Stage : C.Stage_Status;
      Freeze : C.Freeze_Status;
      Status : C.Values.Access_Status;
      Modules : C.Execution_Report;
      Init : C.Namespace.Initialization_Report;
      Issued : Boolean;
      Node : C.Namespace.Node_ID;
      Region, Method : C.Node_Descriptor;
      Result : C.Values.Result;
   begin
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "depth provider identity");
      if Deep then C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Table256, Stage);
      else C.Stage_Table (Service, 1, C.DSDT, Bank_Access_Oracle.Table128, Stage); end if;
      Check (Stage = C.Staged, "depth stage");
      C.Freeze_Catalog (Service, Freeze); Check (Freeze = C.Catalog_Frozen_OK, "depth freeze");
      C.Execute_Frozen (Service, 100_000, Modules, Status);
      Check (Status = C.Values.Available and then Modules.Modules.Outcome = C.Values.Modules_Completed, "depth module");
      C.Initialize_Members (Service, Init, Status); Check (Status = C.Values.Available, "depth members");
      F.Clear (Items);
      for S in Slot loop
         declare
            Description : C.Values.Field_Description;
            Key : C.Values.Field_Key;
            Name : constant AML_Names.Segment := (if S = Selector then "SEL0" else "DAT0");
         begin
            C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
            Check (Status = C.Values.Available and then Node /= C.Namespace.Root, "depth leaf lookup");
            C.Read_Field_Descriptor (Service, Node, Description, Key, Status); Check (Status = C.Values.Available, "depth key");
            F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
            Check (Status = C.Values.Available, "depth binding");
         end;
      end loop;
      C.Find_Child (Service, C.Namespace.Root, "REG0", Node, Status);
      C.Describe_Node (Service, Node, Region, Status); Check (Status = C.Values.Available, "depth region");
      F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data)), True, True, Items, Status);
      Check (Status = C.Values.Available, "depth named catalog");
      C.Find_Child (Service, C.Namespace.Root, "READ", Node, Status);
      C.Describe_Node (Service, Node, Method, Status); Check (Status = C.Values.Available, "depth method");
      Ada.Text_IO.Put_Line ("Depth" & Depth'Image & " setup complete");
      if not Expanded then
      for Invocation in 1 .. 2 loop
         IO.Calls := 0; IO.Values := [others => 0];
         F.Evaluate_Named (Service, IO, Items, Method, 100_000, Result, Status);
         Ada.Text_IO.Put_Line ("Depth" & Depth'Image & " invocation" & Invocation'Image &
           " outcome " & Result.Status'Image & " requests" & IO.Calls'Image);
         Check (Status = C.Values.Available, "depth evaluation admission");
         if Deep then
            Check (Result.Status = AML_Execute.Value_Limit, "deep graph work limit");
            Check (IO.Calls = 0 and then IO.Values = Registers'[others => 0], "deep work exhaustion precedes all effects");
         else
            Check (Result.Status = AML_Execute.Returned and then Result.Number = 3, "admitted depth value");
            Check (IO.Calls = Depth + 1 and then IO.Values = Registers'[others => 3], "admitted depth effects");
         end if;
         Check (not F.Uncertain (Service).Present and then not Pending (IO), "depth result has no uncertain completion");
         Check (C.Retained_Results (Service) = 0, "depth roots released");
      end loop;
      end if;
      if Expanded then
         declare
            Expanded_Work : constant AML_Execution_Limits.Field_Work_Limit := 100_000;
            package Expanded_IO is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
              Maximum_Bindings => 2, Authorize_Target => Authorize, Maximum_Work => Expanded_Work);
            Bindings : Expanded_IO.Binding_Set;
         begin
            Expanded_IO.Clear (Bindings);
            for S in Slot loop
               declare
                  Description : C.Values.Field_Description;
                  Key : C.Values.Field_Key;
                  Name : constant AML_Names.Segment := (if S = Selector then "SEL0" else "DAT0");
               begin
                  C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
                  C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
                  Check (Status = C.Values.Available, "expanded depth leaf");
                  Expanded_IO.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Bindings, Status);
                  Check (Status = C.Values.Available, "expanded depth binding");
               end;
            end loop;
            Expanded_IO.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (Data)), True, True, Bindings, Status);
            Check (Status = C.Values.Available, "expanded depth catalog");
            for Invocation in 1 .. 2 loop
               IO.Calls := 0; IO.Values := [others => 0];
               Expanded_IO.Evaluate_Named (Service, IO, Bindings, Method, 100_000, Result, Status);
               Ada.Text_IO.Put_Line ("Expanded depth" & Depth'Image & " invocation" & Invocation'Image &
                 " outcome " & Result.Status'Image & " requests" & IO.Calls'Image);
               Check (Status = C.Values.Available and then Result.Status = AML_Execute.Returned
                 and then Result.Number = 3, "explicitly admitted deep graph value");
               Check (IO.Calls = Depth + 1 and then IO.Values = Registers'[others => 3], "explicit deep effects");
               Check (not Expanded_IO.Uncertain (Service).Present and then C.Retained_Results (Service) = 0,
                 "explicit deep invocation completion and roots");
            end loop;
         end;
      end if;
   end Run;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "one depth case argument");
   declare Choice : constant String := Ada.Command_Line.Argument (1); begin
      if Choice = "shallow" then Run (False);
      elsif Choice = "exhausted" then Run (True);
      elsif Choice = "admitted" then Run (True, Expanded => True);
      else raise Program_Error with "unknown depth case"; end if;
   end;
   Ada.Text_IO.Put_Line ("Bank access checks" & Checks'Image);
end Bank_Access_Tests;
