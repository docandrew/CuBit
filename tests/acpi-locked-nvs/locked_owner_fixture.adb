pragma Ada_2022;
with AML_Objects;
with Ada.Text_IO;
with Interfaces;
with Locked_Owner_Model;
with Locked_Test_Core;
with Locked_Test_Owner;
with AML_Decode;
with AML_Execute;
with Root_Test_Support;
package body Locked_Owner_Fixture is
   use Interfaces;
   use Locked_Owner_Model;
   use Root_Test_Support;
   package C renames Locked_Test_Core;
   package Owner renames Locked_Test_Owner;
         Expected : constant Unsigned_64 := (if Mode = 9 then 16#1A5# else 16#A5#);
         Datums : constant Natural := (if Mode = 9 then 2 else 1);
         subtype Bytes is AML_Decode.Bytes;
         use type Bytes, C.Stage_Status, C.Freeze_Status, C.Values.Access_Status, C.Values.Module_Outcome;
         use type AML_Execute.Execution_Status;
         function Table return Bytes is
            Payload : constant Bytes := [16#5B#, 16#80#] & Enc ("SANV")
              & [Unsigned_8'(if Mode = 5 then 1 else 0), 16#0C#,
                 Unsigned_8'(if Mode = 7 then 0 else 16#10#),
                 Unsigned_8'(if Mode = 7 then 16#30# else 16#10#), 0, 0, 16#0A#, 16]
              & Block ([16#5B#, 16#81#], Enc ("SANV") & [16#11#, 0, 8] & Enc ("MPGN") & [Unsigned_8'(if Mode in 9 | 10 then 9 else 8)])
              & [8] & Enc ("BOOT") & [0]
              & (if Mode = 6 then [16#70#, 1] & Enc ("MPGN") else [16#70#] & Enc ("MPGN") & Enc ("BOOT"))
              & Method ("READ", [16#A4#] & Enc ("MPGN"))
              & Method ("WRIT", [16#70#, 1] & Enc ("MPGN") & [16#A4#, 0]);
            Raw : Bytes (1 .. 36 + Payload'Length) := [others => 0];
            Sum : Unsigned_8 := 0;
         begin
            Raw (1 .. 4) := Enc ("DSDT"); Raw (9) := 2;
            for Index in 0 .. 3 loop Raw (5 + Index) := Unsigned_8 (Shift_Right (Unsigned_32 (Raw'Length), Index * 8) and 255); end loop;
            Raw (37 .. Raw'Last) := Payload;
            for Byte of Raw loop Sum := Sum + Byte; end loop;
            Raw (10) := 0 - Sum; return Raw;
         end Table;
         Service : aliased C.State (1, 4096, 4096, 1, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements);
         Stage : C.Stage_Status;
         Freeze : C.Freeze_Status;
         Status : C.Values.Access_Status;
         Report : C.Execution_Report;
         Members : C.Namespace.Initialization_Report;
         Found : C.Namespace.Lookup_Result;
         Node : C.Node_Descriptor;
         Result : C.Values.Result;
         Good : Boolean;
         Before : Natural;
   procedure Assert_At (Line : Positive; Good : Boolean) is
   begin
      if not Good then Ada.Text_IO.Put_Line ("failed fixture line" & Line'Image
        & " owner stage " & Owner.Observe.Stage'Image); end if;
      Check (Good);
   end Assert_At;
   procedure Run is
begin
         Now := 1; Word := 0; Loads := 0; Changes := 0; Late_Load := False; IPC_Now := 500;
         Configure;
         Owner.Begin_Startup (Destination, 1_000, Good); Assert_At (53, Good);
         C.Stage_Table (Service, 1, C.DSDT, Table, Stage); Assert_At (54, Stage = C.Staged);
         C.Freeze_Catalog (Service, Freeze); Assert_At (55, Freeze = C.Catalog_Frozen_OK);
         Owner.Execute_Module_Startup (Service, 10_000, Report, Status);
         Ada.Text_IO.Put_Line ("Owner case" & Mode'Image & " modules " & Report.Modules.Outcome'Image
           & " status " & Report.Modules.Failure'Image & " acquire" & Acquires'Image & " load" & Loads'Image);
         if Mode in 0 | 9 then
            Assert_At (60, Status = C.Values.Available and then Report.Modules.Outcome = C.Values.Modules_Completed);
            Assert_At (61, Acquires = 1 and then Releases = 1 and then Loads = Datums and then Legacy_Reads = 0);
            C.Find_Object (Service, C.Namespace.Root, "BOOT", Found, Status);
            C.Observe_Named_Value (Service, Found.Node, Result, Status);
            Assert_At (64, Status = C.Values.Available and then Result.Status = AML_Execute.Returned and then Result.Number = Expected);
            C.Initialize_Members (Service, Members, Status); Assert_At (65, Status = C.Values.Available);
            Owner.Prepare_Fields (Service, Good); Assert_At (66, Good);
            C.Find_Object (Service, C.Namespace.Root, "READ", Found, Status);
            C.Describe_Node (Service, Found.Node, Node, Status);
            Owner.Evaluate_Named (Service, Node, 1000, Result, Status);
            Assert_At (70, Status = C.Values.Available and then Result.Status = AML_Execute.Returned and then Result.Number = Expected);
            Owner.Invoke_With_Arguments (Service, Found.Node, [others => (C.Values.Immediate_Argument, 0)], 0, 1000, Result, Status);
            Assert_At (72, Status = C.Values.Available and then Result.Status = AML_Execute.Returned and then Result.Number = Expected);
            Assert_At (73, Acquires = 3 and then Releases = 3 and then Loads = 3 * Datums and then Changes = 6);
            C.Find_Object (Service, C.Namespace.Root, "WRIT", Found, Status);
            C.Describe_Node (Service, Found.Node, Node, Status); Before := Acquires;
            Owner.Evaluate_Named (Service, Node, 1000, Result, Status);
            Ada.Text_IO.Put_Line ("write status " & Result.Status'Image & " acquire" & Acquires'Image
              & " before" & Before'Image & " loads" & Loads'Image);
            Assert_At (77, Result.Status = AML_Execute.Unsupported and then Acquires = Before and then Loads = 3 * Datums);
         else
            Assert_At (79, Report.Modules.Outcome /= C.Values.Modules_Completed and then Legacy_Reads = 0);
            Assert_At (80, Loads = (if Mode in 3 | 4 | 8 | 10 | 11 then 1 else 0));
            if Mode in 1 | 5 | 6 | 7 then Assert_At (81, Acquires = 0); end if;
         end if;
         Owner.Close_Startup;
         Assert_At (84, Word = 0 and then Acquires = Releases);
         if Mode = 8 then Assert_At (85, Releases = 1 and then Queries_Lock = 2); end if;
         Before := Releases; Owner.Close_Startup; Assert_At (86, Releases = Before);
         if Mode = 11 then
            -- Physical release completed, but every termination reply was lost.
            -- Close must retain uncertainty and issue queries rather than replay.
            Assert_At (88, Owner.Observe.IO_Pending and then Releases = 1);
            Before := Queries_Lock;
            Owner.Close_Startup;
            Assert_At (89, Owner.Observe.IO_Pending and then Releases = 1
              and then Queries_Lock > Before and then Loads = 1);
            Owner.Prepare_Fields (Service, Good);
            Assert_At (90, not Good and then Loads = 1 and then Acquires = 1);
            Drop_Termination_Replies := False;
            Before := Queries_Lock;
            Owner.Close_Startup;
            Assert_At (91, Releases = 1 and then Queries_Lock > Before);
            -- Positive cleanup evidence does not rearm a failed provider.
            Owner.Prepare_Fields (Service, Good);
            Assert_At (92, not Good and then Owner.Observe.IO_Pending);
            Before := Queries_Lock;
            Owner.Close_Startup;
            Assert_At (93, Releases = 1 and then Queries_Lock = Before);
         end if;
         Assert_At (87, C.Valid (Service) and then C.Retained_Results (Service) = 0);
   Ada.Text_IO.Put_Line ("Locked owner checks" & Checks'Image);
   end Run;
end Locked_Owner_Fixture;
