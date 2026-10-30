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
with AML_Field_Plans;
with AML_Field_Targets;
with AML_Identity;
with AML_Identity.Issuer;
with Root_Test_Support; use Root_Test_Support;
procedure Field_Any_Space_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Retained_Result_Capacity => 32);
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   package E renames AML_Execute;
   subtype Bytes is AML_Decode.Bytes;
   use type Bytes;
   use type C.Values.Access_Status;
   use type C.Values.Value_Kind;
   use type C.Values.Module_Outcome;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Namespace.Lookup_Status;
   use type E.Execution_Status;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Observation;
   use type AML_Field_Plans.Datum_Count;
   type Slot is range 1 .. 9;
   type IDs is array (Slot) of A.Register_ID;
   Registers : constant IDs := [16#8000_0000_0000_0041#,
      16#FFFF_FFFF_FFFF_FF97#, 16#71#, 16#3001#, 16#4007#,
      16#500B#, 16#600D#, 16#7011#, 16#FEDC_BA98_7654_3210#];
   Pattern : constant Unsigned_64 := 16#D3_C6_B9_AC_8F_72_65_58#;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Width : T.Access_Width := T.Byte_Access;
      Space : Unsigned_8 := 0;
      Calls : Natural := 0;
      Epoch : A.Counter := 1;
      Fault, Unknown : Boolean := False;
      Revoke_After_First, Wrong_Last_Geometry : Boolean := False;
      Receipt, Confirmed : A.Observation;
   end record;
   function Pending (IO : Context) return Boolean is (IO.Receipt.Present);
   procedure Describe (IO : Context; Target : A.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      Position : Natural := 0;
   begin
      for I in Slot loop
         if Target.Register = Registers (I) then Position := Natural (I); end if;
      end loop;
      Geometry := (Region => (Space => IO.Space, Base => 16#1000#, Length => 12),
         Byte_Offset => (if Position = 9 and then IO.Wrong_Last_Geometry then 11
            elsif Position = 0 then 0 else
            Unsigned_64 ((Position - 1) * T.Octets (IO.Width))), Width => IO.Width);
      Accepted := not Pending (IO) and then Target.Provider = IO.Owner
        and then Target.Endpoint = 7 and then Target.Epoch = IO.Epoch
        and then Position /= 0;
   end Describe;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Geometry : AML_Field_Layout.Register_Geometry;
      Allowed : Boolean;
   begin
      Reply := (others => <>);
      Describe (IO, Item.Destination, Geometry, Allowed);
      if not Allowed or else Item.For_Write then return; end if;
      IO.Calls := IO.Calls + 1;
      if Item.Destination.Register = Registers (2) and then IO.Unknown then
         IO.Receipt := (True, IO.Owner, Item, 1);
         Reply := (A.Indeterminate, IO.Owner, 0, 1);
      elsif Item.Destination.Register = Registers (2) and then IO.Fault then
         Reply := (A.Failed_Completed, IO.Owner, 0, A.No_Operation);
      else
         Reply := (A.Completed, IO.Owner,
            (if Geometry.Byte_Offset = 8 then 16#A7# else
            Shift_Right (Pattern, Natural (Geometry.Byte_Offset) * 8))
              and (2 ** (T.Octets (IO.Width) * 8) - 1), A.No_Operation);
      end if;
      if Item.Destination.Register = Registers (1) and then IO.Revoke_After_First then
         IO.Epoch := IO.Epoch + 1;
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
   begin
      Accepted := Expected.Present and then not Pending (IO) and then IO.Confirmed = Expected;
   end Confirmed;
   procedure Authorize (IO : Context; Target : A.Target;
      For_Write : Boolean; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
   end Authorize;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed, Authorize_Target => Authorize);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why & Checks'Image; end if;
   end Check;
   function Table (Revision, Space : Unsigned_8) return Bytes is
      Payload : constant Bytes := [16#5B#,16#80#] & Enc ("REG0")
        & [Space,16#0C#,0,16#10#,0,0,16#0A#,12]
        & Block ([16#5B#,16#81#], Enc ("REG0")
           & [0,0,7] & Enc ("FLD0") & [16#40#,4])
        & Method ("READ", [16#A4#] & Enc ("FLD0"))
        -- Arithmetic consumes the dereferenced Field value. A bare returned
        -- RegionField cannot be exported by ACPICA's debugger (AE_TYPE).
        & Method ("DREF", [16#A4#,16#72#,16#83#,16#71#] & Enc ("FLD0") & [0,0])
        & Method ("FILL", [16#A4#] & Block ([16#11#], [16#0C#,0,0,1,0]))
        & Method ("WRIT", [16#70#,1] & Enc ("FLD0") & [16#A4#,0]);
      Data : Bytes (1 .. 36 + Payload'Length) := [others => 0];
      Sum : Unsigned_8 := 0;
   begin
      Data (1 .. 4) := Enc ("DSDT"); Data (9) := Revision;
      for I in 0 .. 3 loop
         Data (5 + I) := Unsigned_8 (Shift_Right (Unsigned_32 (Data'Length), I * 8) and 255);
      end loop;
      Data (37 .. Data'Last) := Payload;
      for B of Data loop Sum := Sum + B; end loop;
      Data (10) := 0 - Sum; return Data;
   end Table;
begin
   for Revision in Unsigned_8 range 1 .. 2 loop
      for Space in Unsigned_8 range 0 .. 1 loop
         declare
            S : aliased C.State (1, 4096, 4096);
            IO : Context;
            Items, Bad : F.Binding_Set;
            Targets, Reversed : AML_Field_Targets.Vector := [others => A.No_Target];
            Count : constant AML_Field_Plans.Datum_Count := 9;
            Expected : constant Unsigned_64 := 16#4FA7_8D73_591E_E4CA#;
            Buffer_Result : constant Boolean := Revision = 1;
            Key : C.Values.Field_Key;
            Description : C.Values.Field_Description;
            Status : C.Values.Access_Status;
            Found : C.Namespace.Lookup_Result;
            Staged : C.Stage_Status;
            Frozen : C.Freeze_Status;
            Report : C.Execution_Report;
            Members : C.Namespace.Initialization_Report;
            Result : E.Execution_Result;
            OK : Boolean;
            Before : Natural;
            Saved : A.Observation;
            Node : C.Node_Descriptor;
            Named : C.Values.Result;
            type Held_Values is array (Positive range 1 .. 32) of C.Values.Value_Handle;
            Held : Held_Values := [others => C.Values.No_Value];
            Large : C.Values.Value_Handle := C.Values.No_Value;
            View : C.Values.Value_Description;
            Data : Bytes (1 .. 8);
            Copied : Natural;
            procedure Check_Buffer is
            begin
               Check (Status = C.Values.Available and then Named.Status = E.Object_Returned, "owned buffer result");
               C.Describe_Result (S, Named.Handle, View, Status);
               Check (Status = C.Values.Available and then View.Kind = C.Values.Buffer_Description
                  and then View.Length = 8, "buffer type and byte length");
               C.Read_Result_Bytes (S, Named.Handle, 0, Data, Copied, Status);
               Check (Status = C.Values.Available and then Copied = 8
                  and then Data = Bytes'[16#CA#,16#E4#,16#1E#,16#59#,16#73#,16#8D#,16#A7#,16#4F#],
                  "nine datum Buffer result including last byte");
               Check (C.Retained_Results (S) = 1, "single retained result");
               Large := Named.Handle;
               C.Release_Result (S, Large, Status);
               Check (Status = C.Values.Available and then C.Retained_Results (S) = 0, "release buffer");
            end Check_Buffer;
            procedure Run (Name : String; Set : F.Binding_Set; Want : E.Execution_Status;
               Budget : Natural := 1000) is
            begin
               C.Find_Object (S, C.Namespace.Root,
                  [Name (Name'First), Name (Name'First + 1), Name (Name'First + 2), Name (Name'First + 3)],
                  Found, Status);
               Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "lookup");
               F.Invoke_Scalar (S, IO, Set, Found.Node, [others => 0], 0, Budget, Result);
               Check (Result.Status = Want, Name & Result.Status'Image);
               Check (C.Retained_Results (S) = 0, "no retained value");
            end Run;
         begin
            IO.Space := Space;
            AML_Identity.Issuer.Issue (IO.Owner, OK); Check (OK, "issue provider");
            C.Stage_Table (S, 1, C.DSDT, Table (Revision, Space), Staged);
            Check (Staged = C.Staged, "stage");
            C.Freeze_Catalog (S, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
            C.Execute_Frozen (S, 10000, Report, Status);
            Check (Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed, "module");
            C.Initialize_Members (S, Members, Status); Check (Status = C.Values.Available, "members");
            C.Find_Object (S, C.Namespace.Root, "FLD0", Found, Status);
            Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "field");
            C.Read_Field_Descriptor (S, Found.Node, Description, Key, Status);
            Check (Status = C.Values.Available, "descriptor");
            for I in AML_Field_Plans.Datum_Position range 1 .. Count loop
               Targets (I) := (IO.Owner, 7, 1, Registers (Slot (I)));
            end loop;
            Reversed := Targets; Reversed (1) := Targets (2); Reversed (2) := Targets (1);
            F.Add_Read_Binding (S, IO, Key, Reversed, Count, Bad, Status);
            Check (Status /= C.Values.Available and then IO.Calls = 0, "ordered geometry");
            F.Add_Read_Binding (S, IO, Key, Targets, Count - 1, Bad, Status);
            Check (Status /= C.Values.Available and then IO.Calls = 0, "incomplete target list");
            Reversed := Targets; Reversed (Count) := A.No_Target;
            F.Add_Read_Binding (S, IO, Key, Reversed, Count, Bad, Status);
            Check (Status /= C.Values.Available and then IO.Calls = 0, "missing target");
            F.Add_Read_Binding (S, IO, Key, Targets, Count, Bad, Status);
            Check (Status = C.Values.Available, "failed bind leaves set unchanged");
            F.Add_Read_Binding (S, IO, Key, Targets, Count, Items, Status);
            Check (Status = C.Values.Available and then IO.Calls = 0, "bind without effects");
            F.Add_Read_Binding (S, IO, Key, Targets, Count, Items, Status);
            Check (Status /= C.Values.Available, "duplicate field");
            -- Named Store projects Write_Unsupported to Unsupported.
            Run ("WRIT", Items, E.Unsupported); Check (IO.Calls = 0, "read binding denies write");
            Run ("READ", Items, E.Budget_Exceeded, Budget => 0);
            Check (IO.Calls = 0, "method budget before hardware");
            if Buffer_Result then
               -- Scalar-only callers reject a returned Buffer after the read,
               -- releasing the result; they must not pretend no read happened.
               Run ("READ", Items, E.Unsupported_Value);
               Check (IO.Calls = Natural (Count), "scalar API releases Buffer after reads");
               C.Find_Object (S, C.Namespace.Root, "READ", Found, Status);
               Before := IO.Calls;
               F.Invoke_Retained (S, IO, Items, Found.Node, [others => 0], 0, 1000, Named, Status);
               Check (IO.Calls = Before + Natural (Count), "method Buffer read effects");
               Check_Buffer;
               Before := IO.Calls;
               Run ("DREF", Items, E.Returned);
               Check (Result.Value = 16#591E_E4CA# and then IO.Calls = Before + Natural (Count),
                  "ACPICA arithmetic consumes Buffer with32bit conversion");
               C.Find_Object (S, C.Namespace.Root, "FLD0", Found, Status);
               C.Describe_Node (S, Found.Node, Node, Status);
               Check (Status = C.Values.Available, "Buffer named descriptor"); Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 0, Named, Status);
               Check (Named.Status = E.Budget_Exceeded and then IO.Calls = Before
                  and then C.Retained_Results (S) = 0, "Buffer budget before effects");
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (IO.Calls = Before + Natural (Count), "direct Buffer read effects");
               Check_Buffer;
               Before := IO.Calls; IO.Fault := True;
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Named.Status = E.Unsupported_Value and then IO.Calls = Before + 2
                  and then C.Retained_Results (S) = 0, "failed Buffer read releases reservation");
               IO.Fault := False; IO.Unknown := True; Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Named.Status = E.Field_Indeterminate and then IO.Calls = Before + 2
                  and then C.Retained_Results (S) = 0, "unknown Buffer read releases reservation");
               Saved := F.Uncertain (S);
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Named.Status = E.Field_Indeterminate and then IO.Calls = Before + 2, "Buffer no replay");
               IO.Confirmed := Saved; IO.Receipt := A.No_Observation; IO.Unknown := False;
               F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "Buffer reconcile");
               for I in Held'Range loop
                  F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
                  Check (Status = C.Values.Available and then Named.Status = E.Object_Returned, "fill external roots");
                  Held (I) := Named.Handle;
               end loop;
               Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Status = C.Values.Root_Limit and then IO.Calls = Before
                  and then C.Retained_Results (S) = Held'Length, "root limit before read");
               C.Find_Object (S, C.Namespace.Root, "READ", Found, Status);
               F.Invoke_Retained (S, IO, Items, Found.Node, [others => 0], 0, 1000, Named, Status);
               Check (Status = C.Values.Root_Limit and then IO.Calls = Before
                  and then C.Retained_Results (S) = Held'Length, "method root limit before read");
               for I in Held'Range loop
                  C.Read_Result_Bytes (S, Held (I), 0, Data, Copied, Status);
                  Check (Status = C.Values.Available and then Copied = 8
                     and then Data = Bytes'[16#CA#,16#E4#,16#1E#,16#59#,16#73#,16#8D#,16#A7#,16#4F#], "held Buffer survives later collection");
                  C.Release_Result (S, Held (I), Status); Check (Status = C.Values.Available, "release held root");
               end loop;
               -- A retained full-capacity Buffer prevents a new eight-byte
               -- allocation, even after collection. Neither path may read.
               C.Find_Object (S, C.Namespace.Root, "FILL", Found, Status);
               C.Invoke_Retained (S, Found.Node, [others => 0], 0, 1000, Named, Status);
               Check (Status = C.Values.Available and then Named.Status = E.Object_Returned, "retain full byte pool");
               Large := Named.Handle; Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Named.Status = E.Value_Limit and then IO.Calls = Before
                  and then C.Retained_Results (S) = 1, "byte quota before direct read");
               C.Find_Object (S, C.Namespace.Root, "READ", Found, Status);
               F.Invoke_Retained (S, IO, Items, Found.Node, [others => 0], 0, 1000, Named, Status);
               Check (Named.Status = E.Value_Limit and then IO.Calls = Before
                  and then C.Retained_Results (S) = 1, "byte quota before method read");
               C.Release_Result (S, Large, Status); Check (Status = C.Values.Available, "release full byte pool");
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (IO.Calls = Before + Natural (Count), "collection restores byte capacity");
               Check_Buffer;
            else
               Run ("READ", Items, E.Returned);
               Check (Result.Value = Expected and then IO.Calls = Natural (Count), "cross datum value");
               Before := IO.Calls;
               Run ("DREF", Items, E.Returned);
               Check (Result.Value = Expected and then IO.Calls = Before + Natural (Count), "reference read");
               C.Find_Object (S, C.Namespace.Root, "FLD0", Found, Status);
               Check (Status = C.Values.Available and then Found.Status = C.Namespace.Found, "named field");
               C.Describe_Node (S, Found.Node, Node, Status);
               Check (Status = C.Values.Available, "named descriptor"); Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 1000, Named, Status);
               Check (Status = C.Values.Available and then Named.Status = E.Returned, "direct named read");
               Check (Named.Number = Expected and then IO.Calls = Before + Natural (Count), "direct named value");
               Before := IO.Calls;
               F.Evaluate_Named (S, IO, Items, Node, 0, Named, Status);
               Check (Named.Status = E.Budget_Exceeded and then IO.Calls = Before, "named budget before hardware");
               IO.Epoch := 2; Before := IO.Calls;
               Run ("READ", Items, E.Unsupported_Value);
               Check (IO.Calls = Before, "stale target before effects"); IO.Epoch := 1;
               IO.Wrong_Last_Geometry := True;
               Run ("READ", Items, E.Unsupported_Value);
               Check (IO.Calls = Before, "all geometry preflighted before first read");
               IO.Wrong_Last_Geometry := False;
               IO.Revoke_After_First := True;
               Run ("READ", Items, E.Unsupported_Value);
               Check (IO.Calls = Before + 1, "permission rechecked after completed prefix");
               IO.Revoke_After_First := False; IO.Epoch := 1; Before := IO.Calls;
               IO.Fault := True;
               Run ("READ", Items, E.Unsupported_Value);
               Check (IO.Calls = Before + 2, "completed prefix stops on failure"); IO.Fault := False;
               Before := IO.Calls; IO.Unknown := True;
               Run ("READ", Items, E.Field_Indeterminate);
               Check (IO.Calls = Before + 2, "unknown stops after second datum");
               Saved := F.Uncertain (S);
               Check (Saved.Present and then Saved.Item.Destination.Register = Registers (2)
                  and then not Saved.Item.For_Write and then Saved.Item.Value = 0, "exact unknown read");
               Run ("READ", Items, E.Field_Indeterminate);
               Check (IO.Calls = Before + 2 and then F.Uncertain (S) = Saved, "no automatic retry");
               F.Reconcile (S, IO, Saved, Status); Check (Status /= C.Values.Available, "no evidence");
               IO.Confirmed := Saved; IO.Receipt := A.No_Observation; IO.Unknown := False;
               F.Reconcile (S, IO, Saved, Status); Check (Status = C.Values.Available, "reconcile");
               Run ("READ", Items, E.Returned);
               Check (Result.Value = Expected and then IO.Calls = Before + 2 + Natural (Count), "explicit retry");
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Any-space Field reads passed" & Checks'Image);
end Field_Any_Space_Tests;
