pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with Root_Test_Support; use Root_Test_Support;
procedure Field_Binding_Set_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package E renames AML_Execute;
   package A renames AML_Field_Access;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type E.Execution_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Observation;
   First_ID : constant A.Register_ID := 16#8000_0000_0000_0041#;
   Second_ID : constant A.Register_ID := 16#FFFF_FFFF_FFFF_FF97#;
   -- Explicit synthetic provider, not the real backend catalog fixture.
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      First, Second : Unsigned_64 := 0;
      Calls : Natural := 0;
      Arm_Unknown_Second_Write : Boolean := False;
      Receipt, Confirmed : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 2),
         Byte_Offset => (if Target.Register = First_ID then 0 else 1),
         Width => ACPI_FADT.Transactions.Byte_Access);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = 1
        and then Target.Register in First_ID | Second_ID;
   end Describe;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      OK : Boolean;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, OK);
      if not OK then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.For_Write then
         if Item.Destination.Register = First_ID then IO.First := Item.Value;
         else IO.Second := Item.Value; end if;
      end if;
      if Item.For_Write and then IO.Arm_Unknown_Second_Write
        and then Item.Destination.Register = Second_ID then
         IO.Arm_Unknown_Second_Write := False;
         IO.Receipt := (True, IO.Owner, Item, 1);
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
      else
         Reply := (A.Completed, IO.Owner,
           (if Item.For_Write then 0 elsif Item.Destination.Register = First_ID
            then IO.First else IO.Second), A.No_Operation);
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected; end Confirmed;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
   end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed, Authorize_Target => Authorize,
      Maximum_Bindings => 2);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   function Table (Revision : Unsigned_8) return Bytes is
      Payload : constant Bytes := [16#5B#,16#80#] & Enc ("REG0")
        & [0,16#0C#,0,16#10#,0,0,16#0A#,2]
        & Block ([16#5B#,16#81#], Enc ("REG0") & [1] & Enc ("FLD1") & [8] & Enc ("FLD2") & [8])
        & Method ("PURE", [16#A4#,1])
        & Method ("READ", [16#A4#] & Enc ("FLD1"))
        & Method ("PAIR", [16#70#,16#0A#,16#11#] & Enc ("FLD1")
          & [16#70#,16#0A#,16#22#] & Enc ("FLD2")
          & [16#A4#,16#72#] & Enc ("FLD1") & Enc ("FLD2") & [0])
        & Method ("UNKN", [16#70#,16#0A#,16#33#] & Enc ("FLD1")
          & [16#70#,16#0A#,16#E1#] & Enc ("FLD2")
          & [16#70#,16#0A#,16#44#] & Enc ("FLD1") & [16#A4#,0]);
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255); end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      declare
         S, Other : aliased C.State (1, 4096, 4096, 1);
         IO, Foreign : Context;
         Items, Empty, Partial : F.Binding_Set;
         One : F.Binding;
         K1, K2, Other_Key : C.Values.Field_Key;
         Description : C.Values.Field_Description;
         Status : C.Values.Access_Status;
         Found : C.Namespace.Lookup_Result;
         Result : E.Execution_Result;
         OK : Boolean;
         Saved : A.Observation;
         Before : Natural;
         T1, T2 : A.Target;
         procedure Load (Service : in out C.State) is
            Staged : C.Stage_Status;
            Frozen : C.Freeze_Status;
            Report : C.Execution_Report;
            Members : C.Namespace.Initialization_Report;
         begin
            C.Stage_Table (Service, 41, C.DSDT, Table (Revision), Staged);
            Check (Staged = C.Staged, "stage");
            C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
            C.Execute_Frozen (Service, 10000, Report, Status);
            Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
            C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "initialize");
         end Load;
         procedure Key (Service : C.State; Name : String; Value : out C.Values.Field_Key) is
         begin
            C.Find_Object (Service, C.Namespace.Root, [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)], Found, Status);
            Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "find field");
            C.Read_Field_Descriptor (Service, Found.Node, Description, Value, Status);
            Check (Status = C.Values.Available, "field descriptor");
         end Key;
         procedure Run (Name : String; Set : F.Binding_Set; Expected : E.Execution_Status) is
         begin
            C.Find_Object (S, C.Namespace.Root, [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)], Found, Status);
            Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "find method");
            F.Invoke_Scalar (S, IO, Set, Found.Node, [others => 0], 0, 1000, Result);
            Check (Result.Status = Expected, Name & Result.Status'Image);
            Check (C.Retained_Results (S) = 0, "retained cleanup");
         end Run;
      begin
         AML_Identity.Issuer.Issue (IO.Owner, OK); Check (OK, "owner");
         AML_Identity.Issuer.Issue (Foreign.Owner, OK); Check (OK, "foreign owner");
         Load (S); Load (Other);
         Key (S, "FLD1", K1); Key (S, "FLD2", K2); Key (Other, "FLD2", Other_Key);
         T1 := (IO.Owner, 7, 1, First_ID); T2 := (IO.Owner, 7, 1, Second_ID);
         Run ("PURE", Empty, E.Returned); Check (Result.Value = 1 and then IO.Calls = 0, "empty pure");
         Run ("READ", Empty, E.Unsupported_Value); Check (IO.Calls = 0, "empty denies");
         F.Add_Binding (S, IO, K1, T1, Items, Status); Check (Status = C.Values.Available, "first binding");
         Partial := Items;
         F.Add_Binding (S, IO, K1, T1, Items, Status); Check (Status /= C.Values.Available, "duplicate");
         F.Add_Binding (Other, IO, Other_Key, T2, Items, Status); Check (Status /= C.Values.Available, "mixed owner");
         F.Add_Binding (S, Foreign, K2, (Foreign.Owner, 7, 1, Second_ID), Items, Status);
         Check (Status /= C.Values.Available, "mixed provider");
         F.Add_Binding (S, IO, K2, (IO.Owner, 7, 2, Second_ID), Items, Status);
         Check (Status /= C.Values.Available, "bad target");
         F.Add_Binding (S, IO, K2, T2, Items, Status); Check (Status = C.Values.Available, "second binding after failures");
         F.Add_Binding (S, IO, K2, T2, Items, Status); Check (Status /= C.Values.Available, "full");
         Run ("PAIR", Items, E.Returned);
         Check (Result.Value = 16#33# and then IO.First = 16#11# and then IO.Second = 16#22# and then IO.Calls = 4, "shared state");
         Before := IO.Calls;
         Run ("PAIR", Partial, E.Unsupported);
         Check (IO.Calls = Before + 1 and then IO.First = 16#11#, "prior write not rolled back");
         F.Bind (S, IO, K1, T1, One, Status); Check (Status = C.Values.Available, "singleton bind");
         C.Find_Object (S, C.Namespace.Root, "READ", Found, Status);
         F.Invoke_Scalar (S, IO, One, Found.Node, [others => 0], 0, 1000, Result);
         Check (Result.Status = E.Returned and then Result.Value = 16#11#, "singleton delegates");
         Before := IO.Calls;
         C.Invoke_Scalar (S, Found.Node, [others => 0], 0, 1000, Result);
         Check (Result.Status = E.Unsupported_Value and then IO.Calls = Before, "default no IO");
         IO.Arm_Unknown_Second_Write := True;
         Run ("UNKN", Items, E.Field_Indeterminate);
         Check (IO.Calls = Before + 2 and then IO.First = 16#33# and then IO.Second = 16#E1#, "second unknown suppresses third");
         Saved := F.Uncertain (S);
         Check (Saved = IO.Receipt and then Saved.Item.Destination.Register = Second_ID, "selected receipt");
         F.Clear (Items); Run ("PURE", Items, E.Field_Indeterminate);
         -- Trusted modeled terminal confirmation; no replay and no provider-to-arena callback.
         IO.Confirmed := IO.Receipt; IO.Receipt := A.No_Observation;
         Run ("PURE", Items, E.Field_Indeterminate);
         F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "own arena reconcile");
         Run ("PURE", Items, E.Returned);
         C.Find_Object (Other, C.Namespace.Root, "PURE", Found, Status);
         F.Invoke_Scalar (Other, IO, Partial, Found.Node, [others => 0], 0, 1000, Result);
         Check (Result.Status = E.Returned and then Result.Value = 1, "unused foreign binding allowed");
         Before := IO.Calls;
         C.Find_Object (Other, C.Namespace.Root, "READ", Found, Status);
         F.Invoke_Scalar (Other, IO, Partial, Found.Node, [others => 0], 0, 1000, Result);
         Check (Result.Status = E.Unsupported_Value and then IO.Calls = Before, "selected foreign binding denies");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Field binding set checks" & Checks'Image);
end Field_Binding_Set_Tests;
