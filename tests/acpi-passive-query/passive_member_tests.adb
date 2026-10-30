pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with ACPI_Service_Core;
with ACPI_Passive_Members;
with ACPI_Passive_Status;
with ACPI_Passive_Identity;
with ACPI_Passive_Path;
with ACPI_Describe_Observer;
with ACPI_Passive_Resource_Projection;
with ACPI_Resources_Observer;
with ACPI_Passive_Dispatch;
with ACPI_Observer_Loans;
with ACPI_Grant_Byte_IO;
with ACPI_Query_Sessions;
with ACPI_Query_Pump;
with CuBit.Process_IDs;
with CuBit.Grant_References;
with System;
with ACPI_Passive_Search;
with ACPI_Search_Observer;
with ACPI_Protocol;
with ACPI_Passive_Query;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with Firmware_Tables;
procedure Passive_Member_Tests is
   package C is new ACPI_Service_Core
     (Perform_Delay => AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 128,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 2);
   package P is new ACPI_Passive_Members (C);
   package S is new ACPI_Passive_Status (Core => C, Reader => P);
   package F renames S.Fields;
   package Identity is new ACPI_Passive_Identity (Core => C, Reader => P);
   package Paths is new ACPI_Passive_Path (Core => C);
   use type Paths.Outcome;
   use type Identity.Outcome;
   package Q is new ACPI_Passive_Search (Core => C, Reader => P);
   package W renames Q.Wire;
   package WP renames Q.Pages;
   package H is new ACPI_Search_Observer (Core => C, Reader => P, Search_Owner => Q);
   package D is new ACPI_Describe_Observer (Core => C, Reader => P, Search_Owner => Q,
     Identity => Identity, Paths => Paths);
   use type D.Outcome;
   use type D.Diagnostic;
   package R renames H.Replies;
   package Project renames ACPI_Passive_Resource_Projection;
   package G is new ACPI_Resources_Observer (Core => C, Reader => P, Search_Owner => Q);
   package U is new ACPI_Passive_Dispatch (Core => C, Reader => P, Search_Owner => Q,
     Identity_Owner => Identity, Path_Owner => Paths);
   use type U.Outcome;
   use type G.Outcome;
   use type G.Diagnostic;
   use type G.Data_IO.Item_Count;
   use type G.Data_IO.Template_Status;
   package RR renames Project.Records;
   use type Project.Outcome;
   use type RR.Resource_Kind;
   use type RR.Resource_Record;
   use type RR.U16;
   use type RR.Controller_Kind;
   use type RR.Memory_Encoding;
   use type RR.Address_Encoding;
   use type RR.I2C_Address_Mode;
   use type RR.Trigger;
   use type RR.IO_Restriction;
   use type Project.Data_IO.Byte_Count;
   Wire_Node_Limit : constant ACPI_Passive_Query.Node_Limit :=
     ACPI_Passive_Query.Node_Limit (C.Max_Namespace_Nodes);
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Value_Handle;
   use type C.Values.Module_Outcome;
   use type AML_Execute.Execution_Status;
   use type P.Outcome;
   use type P.Atom_Kind;
   use type P.Member_Name;
   use type AML_Decode.Integer_Value;
   use type Firmware_Tables.Byte;
   use type ACPI_Passive_Query.Field_Status;
   use type F.Field_Record;
   use type F.Field_Name;
   use type F.Value_Kind;
   use type Q.Outcome;
   use type W.Node_Index;
   use type W.Node_Ref;
   use type W.Instance_ID;
   use type W.Generation;
   use type W.Decode_Status;
   use type W.Operation;
   use type W.Position_Kind;
   use type W.Request;
   use type W.Query_Status;
   use type H.Outcome;
   use type H.Diagnostic;
   use type R.U32;
   Service, Other : aliased C.State (1, 4096, 4096);
   Data : Firmware_Tables.Bytes (Natural'Last - 63 .. Natural'Last);
   Items : P.Atoms (Natural'Last - 3 .. Natural'Last);
   Result : P.Report;
   Checks : Natural := 0;
   Raw_STA_With_Reserved_Bit : constant AML_Decode.Integer_Value := 16#8000_0001#;
   Bus_CID : constant String := "PCI\VEN_1234&DEV_ABCD";
   procedure Check (OK : Boolean; Text : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Text; end if; end Check;
   function Text_Matches (Expected : String) return Boolean is
   begin
      if Result.Copied_Bytes /= Expected'Length or else Expected'Length > Data'Length then return False; end if;
      for I in 1 .. Expected'Length loop
         if Data (Data'First + (I - 1)) /= Character'Pos (Expected (Expected'First + (I - 1))) then
            return False;
         end if;
      end loop;
      return True;
   end Text_Matches;
   function Find (State : C.State; Name : String) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
   begin
      C.Find_Child (State, C.Namespace.Root, Name, Node, Status);
      Check (Status = C.Values.Available, "find " & Name);
      C.Describe_Node (State, Node, Item, Status);
      Check (Status = C.Values.Available, "describe " & Name);
      return Item;
   end Find;
   procedure Prepare (State : aliased in out C.State; Bytes : Firmware_Tables.Bytes) is
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Status : C.Values.Access_Status;
   begin
      C.Stage_Table (State, 1, C.DSDT, Bytes, Staged); Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (State, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (State, 1000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "modules");
      C.Initialize_Members (State, Members, Status); Check (Status = C.Values.Available, "members");
   end Prepare;
   procedure Query (Device : String; Member : P.Member_Name; Expected : P.Outcome) is
   begin
      P.Read (Service, Find (Service, Device), Member, Data, Items, Result);
      Check (Result.Status = Expected, "typed outcome " & Device & Member'Image &
        " expected=" & Expected'Image & " actual=" & Result.Status'Image);
      Check (C.Retained_Results (Service) = 0, "no retained roots escape");
      if Expected /= P.Static_Value then
         Check ((for all Byte of Data => Byte = 0) and then
           (for all Item of Items => Item.Kind = P.No_Atom), "failure clears all output");
      end if;
      if Member = P.Device_Status then
         declare
            Projected : F.Field_Result;
            Observation : P.Report;
         begin
            S.Read (Service, Find (Service, Device), Projected, Observation);
            Check (Observation.Status = Expected, "projection preserves typed observation");
            Check (Projected.Accepted, "usable status observation projected");
            Check (C.Retained_Results (Service) = 0, "projection releases owned roots");
            declare
               Words : constant F.Record_Words := F.Encode (Projected.Value, Wire_Node_Limit, 0);
               Decoded : constant F.Field_Result := F.Decode (Words, Wire_Node_Limit, 0);
            begin
               Check (Decoded.Accepted and then Decoded.Value = Projected.Value,
                 "real AML status survives Describe record codec");
            end;
            case Expected is
               when P.Static_Value =>
                  Check (Projected.Value.Status = ACPI_Passive_Query.Present
                    and then Projected.Value.Kind = F.Integer_Value
                    and then Projected.Value.Number = ACPI_Passive_Query.Word (Items (Items'First).Number)
                    and then not Projected.Value.Defaulted, "observed status never defaulted");
               when P.Missing =>
                  Check (Projected.Value.Status = ACPI_Passive_Query.Absent and then Projected.Value.Defaulted
                    and then Projected.Value.Number = ACPI_Passive_Query.Default_STA,
                    "missing status explicitly defaulted only by projection");
               when P.Method_Valued =>
                  Check (Projected.Value.Status = ACPI_Passive_Query.Method_Valued
                    and then Projected.Value.Kind = F.No_Value and then not Projected.Value.Defaulted,
                    "method status never evaluated or defaulted");
               when P.Unsupported =>
                  Check (Projected.Value.Status = ACPI_Passive_Query.Unsupported
                    and then Projected.Value.Kind = F.No_Value and then not Projected.Value.Defaulted,
                    "unsupported status never defaulted");
               when others => raise Program_Error with "unexpected projection fixture";
            end case;
         end;
      end if;
   end Query;
   procedure Counter_Zero is
      Value : C.Values.Result;
      Status : C.Values.Access_Status;
   begin
      C.Observe_Named_Value (Service, C.Values.Position (Find (Service, "HITS")), Value, Status);
      Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned
        and then Value.Number = 0, "no passive query executes device AML");
   end Counter_Zero;
   procedure Search_Checks is
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      Long_CID : constant String :=
        "VENDOR\BUS_CHANNEL_0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_COMPLETE_IDENTIFIER_FINAL_Z";
      type Indices is array (Natural range <>) of W.Node_Index;
      function Index_Of (Name : String) return W.Node_Index is
        (W.Node_Index (C.Values.Position (Find (Service, Name))));
      function Text_Bytes (Text : String) return WP.Byte_Array is
         Bytes : WP.Byte_Array (101 .. 101 + Text'Length - 1);
      begin
         for I in Text'Range loop Bytes (Bytes'First + I - Text'First) := Character'Pos (Text (I)); end loop;
         return Bytes;
      end Text_Bytes;
      procedure Build (Item : in out W.Request; Text : String; Frame : out WP.Byte_Array) is
         Accepted : Boolean;
         Written : W.Word;
      begin
         WP.Rebase_Text (Item, WP.Frame_Byte_Count, Wire_Node_Limit, Accepted);
         Check (Accepted, "search request metadata rebased");
         WP.Encode_Request (Item, Text_Bytes (Text), Frame, Wire_Node_Limit, Written, Accepted);
         Check (Accepted and then WP.Decode (Frame, Wire_Node_Limit).Status = W.Decoded,
           "search request encoded with complete filter");
      end Build;
      procedure All_Matches
        (Text : String; External : Boolean; Page_Size, Visits : Positive; Expected : Indices) is
         Item : W.Request := ((Kind => W.Find_By_ID, others => <>), (Present => False));
         Frame : WP.Byte_Array (101 .. 101 + 4_096 - 1);
         Nodes : Q.Node_Array (Natural'Last - Page_Size + 1 .. Natural'Last);
         Scratch : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
         Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
         Result : Q.Report;
         Seen : array (W.Node_Index range 1 .. Wire_Node_Limit) of Boolean := [others => False];
         Total : Natural := 0;
         Start : W.Node_Index := 1;
         Finished : Boolean := False;
      begin
         Item.Key.ID := (if External then W.External_ID (WP.Frame_Byte_Count, Text'Length)
                         else W.From_Text (Text).Value);
         for Step in 1 .. C.Max_Namespace_Nodes + 1 loop
            Build (Item, (if External then Text else ""), Frame);
            Q.Search (Service, Scope, Frame, Visits, Nodes, Scratch, Atoms, Result);
            Check (Result.Status in Q.Page_Ready | Q.No_Static_Match, "static search page admitted");
            Check (Result.Visited_Nodes <= Visits, "per-page node work bound");
            Check (Result.Key.Instance = Scope.Instance and then Result.Key.Revision = Scope.Revision,
              "owner scope replaces initial wildcard");
            for I in 0 .. Result.Copied_Nodes - 1 loop
               declare Ref : constant W.Node_Ref := Nodes (Nodes'First + I); begin
                  Check (Ref.Instance = Scope.Instance and then Ref.Revision = Scope.Revision
                    and then Ref.Index in 1 .. Wire_Node_Limit, "scoped bounded result");
                  Check (not Seen (Ref.Index), "device returned once despite matching HID and duplicate CIDs");
                  Check ((for some Index of Expected => Index = Ref.Index), "only expected matching devices");
                  Seen (Ref.Index) := True; Total := Total + 1;
               end;
            end loop;
            if not Result.Next_Cursor.Present then Finished := True; exit; end if;
            Check (Result.Next_Cursor.Value.Next_Node > Start, "empty/work-bounded pages make progress");
            Start := Result.Next_Cursor.Value.Next_Node;
            Item := (Result.Key, Result.Next_Cursor);
         end loop;
         Check (Finished and then Total = Expected'Length, "full paginated static result set");
         Check ((for all Index of Expected => Seen (Index)), "every expected device found");
         Check (C.Retained_Results (Service) = 0, "search leaks no owned roots");
         Counter_Zero;
      end All_Matches;
      Item : W.Request := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value, others => <>),
                           (Present => False));
      Frame : WP.Byte_Array (101 .. 101 + 4_096 - 1);
      Nodes : Q.Node_Array (0 .. 7);
      Scratch : Firmware_Tables.Bytes (1 .. 512);
      Atoms : P.Atoms (0 .. 7);
      Result : Q.Report;
      procedure Expect_Refusal (Expected : Q.Outcome; Owner : Q.Scope_State := Scope) is
      begin
         Q.Search (Service, Owner, Frame, C.Max_Namespace_Nodes, Nodes, Scratch, Atoms, Result);
         Check (Result.Status = Expected, "search refusal " & Expected'Image & " got=" & Result.Status'Image);
         Check (Result.Copied_Nodes = 0 and then not Result.Next_Cursor.Present
           and then (for all Ref of Nodes => Ref = Q.No_Reference), "refusal returns no partial references or cursor");
      end Expect_Refusal;
   begin
      All_Matches ("PNP0C50", False, 8, C.Max_Namespace_Nodes,
        [Index_Of ("DEV0"), Index_Of ("INT0"), Index_Of ("DUPL"), Index_Of ("MCID"), Index_Of ("ACID")]);
      All_Matches ("PNP0C50", False, 1, 1,
        [Index_Of ("DEV0"), Index_Of ("INT0"), Index_Of ("DUPL"), Index_Of ("MCID"), Index_Of ("ACID")]);
      All_Matches ("INTC1057", False, 1, C.Max_Namespace_Nodes, [Index_Of ("DEV0")]);
      All_Matches ("CBT0001", False, 1, C.Max_Namespace_Nodes, [Index_Of ("DEV0")]);
      All_Matches (Bus_CID, True, 1, C.Max_Namespace_Nodes, [Index_Of ("DEV0")]);
      All_Matches (Long_CID, True, 1, C.Max_Namespace_Nodes, [Index_Of ("LCID")]);
      declare Other_CID : String := Long_CID; begin
         Other_CID (Other_CID'Last) := 'Y';
         All_Matches (Other_CID, True, 8, C.Max_Namespace_Nodes, []);
      end;
      All_Matches ("NONE0000", False, 8, C.Max_Namespace_Nodes, []);
      Build (Item, "", Frame);
      declare Uninitialized : C.State (1, 4096, 4096); begin
         Q.Search (Uninitialized, Scope, Frame, C.Max_Namespace_Nodes, Nodes, Scratch, Atoms, Result);
         Check (Result.Status = Q.Namespace_Not_Ready and then Result.Copied_Nodes = 0,
           "owner ready flag cannot expose an uninitialized namespace");
      end;
      Expect_Refusal (Q.Namespace_Not_Ready, (False, Scope.Instance, Scope.Revision));
      Expect_Refusal (Q.Invalid_Request, (True, 0, Scope.Revision));
      Item.Key.Instance := 1; Item.Key.Revision := Scope.Revision; Build (Item, "", Frame);
      Expect_Refusal (Q.Stale_Reference);
      Item.Key.Instance := Scope.Instance; Item.Key.Revision := 1; Build (Item, "", Frame);
      Expect_Refusal (Q.Stale_Reference);
      Item.Key.Revision := Scope.Revision;
      Item.Continuation := (True, (Item.Key, W.Node_Scan, Wire_Node_Limit, 0, 0));
      Check (W.Word (C.Namespace_Node_Count (Service)) < W.Word (Wire_Node_Limit), "test cursor beyond live high-water");
      Build (Item, "", Frame); Expect_Refusal (Q.Bad_Continuation);
      Item.Continuation := (Present => False); Build (Item, "", Frame);
      declare One : P.Atoms (1 .. 1); Empty : Q.Node_Array (1 .. 0); begin
         Q.Search (Service, Scope, Frame, C.Max_Namespace_Nodes, Empty, Scratch, Atoms, Result);
         Check (Result.Status = Q.Output_Required and then Result.Copied_Nodes = 0, "zero output capacity explicit");
         Q.Search (Service, Scope, Frame, C.Max_Namespace_Nodes, Nodes, Scratch, One, Result);
         Check (Result.Status = Q.Scratch_Required and then Result.Required_Atoms = 4
           and then not Result.Byte_Size_Known and then Result.Copied_Nodes = 0
           and then not Result.Next_Cursor.Present, "CID atom sizing does not skip incomplete package");
      end;
      Item.Key.ID := W.From_Text ("CBT0002").Value; Build (Item, "", Frame);
      declare Small : Firmware_Tables.Bytes (1 .. 64); begin
         Q.Search (Service, Scope, Frame, C.Max_Namespace_Nodes, Nodes, Small, Atoms, Result);
         Check (Result.Status = Q.Scratch_Required and then Result.Required_Bytes = Long_CID'Length
           and then Result.Byte_Size_Known and then Result.Copied_Nodes = 0
           and then (for all Ref of Nodes => Ref = Q.No_Reference),
           "late scratch failure rolls back earlier matching device");
         Check (Result.Visited_Nodes > Natural (Index_Of ("ZSTA")), "rollback occurs after earlier match slot");
      end;
      -- ADR matches only direct children of the live requested container.
      Item.Key := (Kind => W.Find_By_Address, Instance => Scope.Instance, Revision => Scope.Revision,
        Node => Index_Of ("PARN"), Address => 16#10000#, ID => W.No_ID);
      Build (Item, "", Frame);
      Q.Search (Service, Scope, Frame, C.Max_Namespace_Nodes, Nodes, Scratch, Atoms, Result);
      Check (Result.Status = Q.Page_Ready and then Result.Copied_Nodes = 1, "direct child ADR match only");
      declare Child : C.Namespace.Node_ID; Status : C.Values.Access_Status; begin
         C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Child, Status);
         Check (Status = C.Values.Available and then Nodes (0).Index = W.Node_Index (Child), "ADR returns nested child not root peer");
      end;
      Item.Key.Address := 16#1_0001_0000#; Build (Item, "", Frame);
      Q.Search (Service, Scope, Frame, C.Max_Namespace_Nodes, Nodes, Scratch, Atoms, Result);
      Check (Result.Status = Q.No_Static_Match and then Result.Copied_Nodes = 0, "ADR query is never narrowed to low32 bits");
      Item.Key.Node := Index_Of ("HITS"); Build (Item, "", Frame); Expect_Refusal (Q.Invalid_Parent);
      Item.Key.Node := Wire_Node_Limit; Build (Item, "", Frame); Expect_Refusal (Q.Invalid_Parent);
      Frame := [others => 0]; Expect_Refusal (Q.Invalid_Request);
      Counter_Zero;
   end Search_Checks;
   procedure Native_Grant_Checks is
      package PID renames CuBit.Process_IDs;
      package Refs renames CuBit.Grant_References;
      use type PID.Process_ID;
      use type Refs.Reference;
      Memory : aliased WP.Byte_Array (1 .. 1032) := [others => 16#AA#] with Volatile;
      Expected_Peer : constant PID.Process_ID := PID.From_Word (16#FEDC_BA98_7654_3210#);
      Expected_Ref : constant Refs.Reference := (Refs.Global_Slot'Last, Refs.Generation'Last);
      Held, Fail_Acquire, Null_Map, Fail_Return : Boolean := False;
      Acquires, Returns : Natural := 0;
      Seen_Peer : PID.Process_ID := PID.No_Process;
      Seen_Ref : Refs.Reference;
      Seen_Length : W.Word := 0;
      procedure Acquire_Runtime (Loan : Refs.Reference; Expected_Owner : PID.Process_ID; Byte_Length : W.Word;
                                 Address : out System.Address; Accepted : out Boolean) is
      begin
         Check (not Held, "native byte IO acquire has no prior hold"); Acquires := Acquires + 1;
         Seen_Peer := Expected_Owner; Seen_Ref := Loan; Seen_Length := Byte_Length;
         Accepted := not Fail_Acquire and then Loan = Expected_Ref and then Expected_Owner = Expected_Peer;
         Held := Accepted; Address := (if Null_Map or else not Accepted then System.Null_Address else Memory (5)'Address);
      end Acquire_Runtime;
      procedure Return_Runtime (Loan : Refs.Reference; Accepted : out Boolean) is
      begin
         Check (Held and then Loan = Expected_Ref, "native byte IO returns original typed reference"); Returns := Returns + 1;
         Accepted := not Fail_Return; if Accepted then Held := False; end if;
      end Return_Runtime;
      package IO is new ACPI_Grant_Byte_IO (Acquire_Runtime, Return_Runtime);
      Map : IO.Acquisition;
      Data : WP.Byte_Array (Natural'Last - 1023 .. Natural'Last);
      Accepted : Boolean;
      type Context is record Dispatched : U.Report; end record;
      Owner : Context;
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      procedure Loan_Acquire (Owner : in out Context; Loan : Refs.Reference; Expected_Owner : PID.Process_ID;
                              Byte_Length : W.Word; Acquired : out IO.Acquisition; Accepted : out Boolean) is
         pragma Unreferenced (Owner);
      begin IO.Acquire (Loan, Expected_Owner, Byte_Length, Acquired, Accepted); end Loan_Acquire;
      procedure Loan_In (Owner : in out Context; Acquired : IO.Acquisition; Data : out WP.Byte_Array; Accepted : out Boolean) is
         pragma Unreferenced (Owner);
      begin IO.Copy_In (Acquired, Data, Accepted); end Loan_In;
      procedure Evaluate (Owner : in out Context; Request_Data : WP.Byte_Array; Response_Data : out WP.Byte_Array;
                          Written, Required_Bytes : out W.Word; Accepted : out Boolean) is
         Nodes : Q.Node_Array (0 .. 7);
         Parents : Paths.Node_Array (0 .. 7);
         Reader_Data, Text_Source : Firmware_Tables.Bytes (1 .. 512);
         Atoms : P.Atoms (0 .. 7);
         Filter, Payload : WP.Byte_Array (0 .. 1023);
      begin
         Memory (5 .. 1028) := [others => 0];
         U.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Request_Data, C.Max_Namespace_Nodes, 32,
           Nodes, Parents, Reader_Data, Atoms, Text_Source, Filter, Payload, Response_Data, Owner.Dispatched);
         Written := Owner.Dispatched.Written; Required_Bytes := Owner.Dispatched.Required_Bytes;
         Accepted := Owner.Dispatched.Status = U.Reply_Ready;
      end Evaluate;
      procedure Loan_Out (Owner : in out Context; Acquired : IO.Acquisition; Data : WP.Byte_Array; Accepted : out Boolean) is
         pragma Unreferenced (Owner);
      begin IO.Copy_Out (Acquired, Data, Accepted); end Loan_Out;
      procedure Loan_Return (Owner : in out Context; Loan : Refs.Reference; Acquired : IO.Acquisition; Accepted : out Boolean) is
         pragma Unreferenced (Owner);
         Local : IO.Acquisition := Acquired;
      begin IO.Release (Loan, Local, Accepted); end Loan_Return;
      package Loans is new ACPI_Observer_Loans (Context, PID.Process_ID, Refs.Reference, Refs.Reference'(others => <>),
        IO.Acquisition, IO.No_Acquisition, Loan_Acquire, Loan_In, Evaluate, Loan_Out, Loan_Return);
      use type Loans.Outcome;
      State : Loans.State;
      Report : Loans.Report;
      Request_Data, Response_Data : WP.Byte_Array (Natural'Last - 1023 .. Natural'Last);
   begin
      IO.Acquire (Expected_Ref, PID.No_Process, 1024, Map, Accepted);
      Check (not Accepted and then not IO.Active (Map) and then Acquires = 0, "actual opaque no-process rejected before syscall callback");
      IO.Acquire (Expected_Ref, Expected_Peer, W.Word'Last, Map, Accepted);
      Check (not Accepted and then Acquires = 0, "native copy extent checked before narrowing");
      Fail_Acquire := True; IO.Acquire (Expected_Ref, Expected_Peer, 1024, Map, Accepted);
      Check (not Accepted and then not IO.Active (Map) and then not Held, "native acquisition failure has no token");
      Fail_Acquire := False; Null_Map := True; IO.Acquire (Expected_Ref, Expected_Peer, 1024, Map, Accepted);
      Check (Accepted and then IO.Active (Map) and then Held, "unusable successful mapping retains hold for cleanup");
      IO.Copy_In (Map, Data, Accepted);
      Check (not Accepted and then (for all B of Data => B = 0), "null mapping is not read");
      IO.Release (Expected_Ref, Map, Accepted);
      Check (Accepted and then not IO.Active (Map) and then not Held, "unusable mapping acquisition still returned");
      Null_Map := False; IO.Acquire (Expected_Ref, Expected_Peer, 1024, Map, Accepted);
      Check (Accepted and then Seen_Peer = Expected_Peer and then Seen_Ref = Expected_Ref and then Seen_Length = 1024,
        "real CuBit identity/reference types reach mapping callback unchanged");
      IO.Copy_In (Map, Data, Accepted);
      Check (Accepted and then (for all B of Data => B = 16#AA#), "actual byte-copy body reads owned mapped memory");
      IO.Copy_In (Map, Data (Data'First .. Data'First + 511), Accepted);
      Check (not Accepted and then (for all B of Data (Data'First .. Data'First + 511) => B = 0), "native copy rejects extent mismatch");
      Data := [others => 16#5A#]; IO.Copy_Out (Map, Data, Accepted);
      Check (Accepted and then (for all I in 5 .. 1028 => Memory (I) = 16#5A#)
        and then (for all I in 1 .. 4 => Memory (I) = 16#AA#)
        and then (for all I in 1029 .. 1032 => Memory (I) = 16#AA#), "actual copy-out preserves guard bytes outside acquired extent");
      IO.Release ((others => <>), Map, Accepted);
      Check (not Accepted and then IO.Active (Map) and then Held, "mismatched typed reference cannot return another hold");
      Fail_Return := True; IO.Release (Expected_Ref, Map, Accepted);
      Check (not Accepted and then IO.Active (Map), "native failed return preserves acquisition");
      Fail_Return := False; IO.Release (Expected_Ref, Map, Accepted);
      Check (Accepted and then not IO.Active (Map), "native successful return clears acquisition record");
      declare Before : constant Natural := Returns; begin
         IO.Release (Expected_Ref, Map, Accepted);
         Check (not Accepted and then Returns = Before, "cleared native token does not return twice");
      end;
      for Kind in W.Operation loop
         declare Item : W.Request; Written : W.Word; begin
            Memory := [others => 16#AA#];
            if Kind = W.Find_By_ID then Item := ((Kind => Kind, ID => W.From_Text ("PNP0C50").Value, others => <>), (Present => False));
            else Item := ((Kind => Kind, Instance => Scope.Instance, Revision => Scope.Revision,
              Node => W.Node_Index (C.Values.Position (Find (Service, (if Kind = W.Find_By_Address then "PARN" else "DEV0")))),
              Address => (if Kind = W.Find_By_Address then 16#10000# else 0), others => <>), (Present => False)); end if;
            WP.Encode_Request (Item, [], Memory (5 .. 1028), Wire_Node_Limit, Written, Accepted);
            Check (Accepted, "actual mapped byte IO query request encoding");
            Loans.Handle (State, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Ref, 1024,
              Request_Data, Response_Data, Report);
            Check (Report.Status = Loans.Completed and then Report.May_Reply and then not Held and then not Loans.Holding (State),
              "actual byte IO and real opaque types compose with loan state and AML dispatcher");
            Check (R.Matches_Request (Memory (5 .. 1028), Request_Data, Wire_Node_Limit), "real copy reply correlates after mapped request mutation");
            Check ((for all I in 1 .. 4 => Memory (I) = 16#AA#) and then (for all I in 1029 .. 1032 => Memory (I) = 16#AA#),
              "full native-copy query path preserves memory outside its acquired extent");
         end;
      end loop;
      Counter_Zero;
   end Native_Grant_Checks;
   procedure Loan_Dispatch_Checks is
      type Peer_ID is new W.Word;
      type Loan_Reference is record Slot, Generation : W.Word := 0; end record;
      No_Ref : constant Loan_Reference := (0, 0);
      Expected_Peer : constant Peer_ID := 16#FEDC_BA98_7654_3210#;
      Expected_Loan : constant Loan_Reference := (16#1234_5678#, 16#FEDC_BA98_7654_3210#);
      type Token is new W.Word;
      Expected_Token : constant Token := 16#BADC_0FFE_E0DD_F00D#;
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      type Failure is (None, Acquire_Error, Read_Error, Evaluate_Error, Write_Error, Bad_Count, Bad_Frame);
      type Loan_Context is record
         Memory : WP.Byte_Array (1 .. 4096) := [others => 0];
         Events : String (1 .. 64) := [others => ' '];
         Event_Count : Natural := 0;
         Fail : Failure := None;
         Return_Fails : Boolean := False;
         Active : Boolean := False;
         Seen_Peer : Peer_ID := 0;
         Seen_Loan, Returned_Loan : Loan_Reference;
         Seen_Length : W.Word := 0;
         Dispatch : U.Report;
         Save_Fails, Answer_Fails, Saved_Live : Boolean := False;
         Answers : Natural := 0;
         Last_Written : W.Word := 0;
         Now : W.Word := 100;
         Local_Deadline : W.Word := W.Word'Last;
         Clock_Usable, Local_Ready, Wait_Usable : Boolean := True;
         Request_Ready, Firmware_Ready, Other_Request : Boolean := False;
         Wait_Deadline : W.Word := 0;
      end record;
      procedure Event (Owner : in out Loan_Context; Value : Character) is
      begin Owner.Event_Count := Owner.Event_Count + 1; Owner.Events (Owner.Event_Count) := Value; end Event;
      procedure Acquire (Owner : in out Loan_Context; Loan : Loan_Reference; Expected_Owner : Peer_ID;
                         Byte_Length : W.Word; Acquired : out Token; Accepted : out Boolean) is
      begin
         Event (Owner, 'A'); Owner.Seen_Peer := Expected_Owner; Owner.Seen_Loan := Loan; Owner.Seen_Length := Byte_Length;
         Check (not Owner.Active, "loan not acquired twice");
         Accepted := Owner.Fail /= Acquire_Error and then Loan = Expected_Loan and then Expected_Owner = Expected_Peer;
         Owner.Active := Accepted; Acquired := (if Accepted then Expected_Token else 0);
      end Acquire;
      procedure Copy_In (Owner : in out Loan_Context; Acquired : Token; Data : out WP.Byte_Array; Accepted : out Boolean) is
      begin
         Event (Owner, 'I'); Check (Owner.Active and then Acquired = Expected_Token
           and then W.Word (Data'Length) = Owner.Seen_Length, "copy in uses only exact acquired extent");
         for I in 0 .. Data'Length - 1 loop Data (Data'First + I) := Owner.Memory (1 + I); end loop;
         Accepted := Owner.Fail /= Read_Error;
      end Copy_In;
      procedure Evaluate (Owner : in out Loan_Context; Request_Data : WP.Byte_Array;
                          Response_Data : out WP.Byte_Array; Written, Required_Bytes : out W.Word; Accepted : out Boolean) is
         Nodes : Q.Node_Array (Natural'Last - 7 .. Natural'Last);
         Parents : Paths.Node_Array (Natural'Last - 7 .. Natural'Last);
         Reader_Data, Text_Source : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
         Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
         Filter : WP.Byte_Array (Natural'Last - 511 .. Natural'Last);
         Payload : WP.Byte_Array (Natural'Last - 1023 .. Natural'Last);
      begin
         Event (Owner, 'E'); Check (Owner.Active, "evaluate while loan acquisition owned");
         -- Change mapped contents after the clone. Actual handlers must still
         -- decode the private request, not a live view of the mutable loan.
         Owner.Memory (1 .. Natural (Owner.Seen_Length)) := [others => 0];
         Response_Data := [others => 0]; Written := 0; Required_Bytes := 0;
         if Owner.Fail = Evaluate_Error then Accepted := False; return; end if;
         U.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Request_Data, C.Max_Namespace_Nodes, 32,
           Nodes, Parents, Reader_Data, Atoms, Text_Source, Filter, Payload, Response_Data, Owner.Dispatch);
         Written := Owner.Dispatch.Written; Required_Bytes := Owner.Dispatch.Required_Bytes;
         Accepted := Owner.Dispatch.Status = U.Reply_Ready;
         if Owner.Fail = Bad_Count then Written := W.Word (Response_Data'Length) + 1; Accepted := True; end if;
         if Owner.Fail = Bad_Frame then Response_Data (Response_Data'First) := 0; end if;
      end Evaluate;
      procedure Copy_Out (Owner : in out Loan_Context; Acquired : Token; Data : WP.Byte_Array; Accepted : out Boolean) is
      begin
         Event (Owner, 'O'); Check (Owner.Active and then Acquired = Expected_Token
           and then W.Word (Data'Length) = Owner.Seen_Length, "copy out uses exact acquired extent");
         for I in 0 .. Data'Length - 1 loop Owner.Memory (1 + I) := Data (Data'First + I); end loop;
         Accepted := Owner.Fail /= Write_Error;
      end Copy_Out;
      procedure Release (Owner : in out Loan_Context; Loan : Loan_Reference; Acquired : Token; Accepted : out Boolean) is
      begin
         Event (Owner, 'R'); Check (Owner.Active and then Acquired = Expected_Token and then Loan = Expected_Loan,
           "release original acquisition/reference");
         Owner.Returned_Loan := Loan; Accepted := not Owner.Return_Fails;
         if Accepted then Owner.Active := False; end if;
      end Release;
      package Loans is new ACPI_Observer_Loans (Loan_Context, Peer_ID, Loan_Reference, No_Ref, Token, 0,
        Acquire, Copy_In, Evaluate, Copy_Out, Release);
      type Saved_Token is new W.Word;
      Expected_Reply : constant Saved_Token := 16#FEDC_1234_BA98_5678#;
      procedure Save (Owner : in out Loan_Context; Handle : out Saved_Token; Accepted : out Boolean) is
      begin
         Event (Owner, 'S'); Check (not Owner.Saved_Live, "save current reply once before acquire");
         Accepted := not Owner.Save_Fails; Owner.Saved_Live := Accepted;
         Handle := (if Accepted then Expected_Reply else 0);
      end Save;
      procedure Answer (Owner : in out Loan_Context; Handle : Saved_Token; Operation : Loans.Report; Delivered : out Boolean) is
      begin
         Check (Owner.Saved_Live and then Handle = Expected_Reply and then not Owner.Active and then Operation.May_Reply,
           "saved reply answered only after original acquisition returned");
         Event (Owner, 'P'); Owner.Answers := Owner.Answers + 1; Owner.Last_Written := Operation.Written;
         Owner.Saved_Live := False; Delivered := not Owner.Answer_Fails;
      end Answer;
      package Sessions is new ACPI_Query_Sessions (Loans, Saved_Token, 0, Save, Answer);
      use type Sessions.Outcome;
      Session : Sessions.State;
      Session_Result : Sessions.Report;
      type Request_Kind is (No_Request, Query_Request, Other_Handled);
      procedure Prepare_Local (Owner : in out Loan_Context; Now, Deadline : out W.Word; Clock_Usable, Ready : out Boolean) is
      begin Event (Owner, 'L'); Now := Owner.Now; Deadline := Owner.Local_Deadline;
         Clock_Usable := Owner.Clock_Usable; Ready := Owner.Local_Ready; end Prepare_Local;
      procedure Firmware (Owner : in out Loan_Context; Handled : out Boolean) is
      begin Event (Owner, 'F'); Handled := Owner.Firmware_Ready; Owner.Firmware_Ready := False; end Firmware;
      procedure Request (Owner : in out Loan_Context; Kind : out Request_Kind; Sender : out Peer_ID;
                         Loan : out Loan_Reference; Stamp : out ACPI_Protocol.Role_Tag; Length : out W.Word) is
      begin
         Event (Owner, 'Q'); Kind := (if not Owner.Request_Ready then No_Request elsif Owner.Other_Request then Other_Handled else Query_Request);
         Owner.Request_Ready := False; Sender := Expected_Peer; Loan := Expected_Loan;
         Stamp := ACPI_Protocol.Observer_Tag; Length := 1024;
      end Request;
      procedure Immediate (Owner : in out Loan_Context; Sender : Peer_ID; Result : Sessions.Report; Delivered : out Boolean) is
      begin
         Check (Sender = Expected_Peer and then Result.Immediate_Reply, "pump answers only current unsaved refusal");
         Event (Owner, 'B'); Delivered := True;
      end Immediate;
      procedure Wait_Activity (Owner : in out Loan_Context; Deadline : W.Word; Usable : out Boolean) is
      begin Event (Owner, 'W'); Owner.Wait_Deadline := Deadline; Usable := Owner.Wait_Usable; end Wait_Activity;
      package Pump is new ACPI_Query_Pump (Sessions, W.Word'Last, Prepare_Local, Firmware,
        Request_Kind, No_Request, Query_Request, Request, Immediate, Wait_Activity);
      use type Pump.Outcome;
      use type Loans.Outcome;
      State : Loans.State;
      Owner : Loan_Context;
      Input, Output : WP.Byte_Array (Natural'Last - 1023 .. Natural'Last);
      Result : Loans.Report;
      Item : W.Request;
      procedure Build (Kind : W.Operation; Device : String := "DEV0") is
         Written : W.Word; Accepted : Boolean;
      begin
         Owner := (others => <>); Owner.Memory := [others => 16#AA#];
         if Kind = W.Find_By_ID then
            Item := ((Kind => Kind, ID => W.From_Text ("PNP0C50").Value, others => <>), (Present => False));
         else
            Item := ((Kind => Kind, Instance => Scope.Instance, Revision => Scope.Revision,
              Node => W.Node_Index (C.Values.Position (Find (Service, Device))),
              Address => (if Kind = W.Find_By_Address then 16#10000# else 0), others => <>), (Present => False));
         end if;
         WP.Encode_Request (Item, [], Owner.Memory (1 .. Input'Length), Wire_Node_Limit, Written, Accepted);
         Check (Accepted and then Written = WP.Frame_Byte_Count, "loan carries actual encoded query");
      end Build;
      procedure Handle (Length : W.Word := Input'Length; Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
                        Peer : Peer_ID := Expected_Peer; Ref : Loan_Reference := Expected_Loan) is
      begin
         Loans.Handle (State, Owner, Stamp, Peer, Ref, Length, Input, Output, Result);
         Check (Result.May_Reply = not Loans.Holding (State), "loan reply gate matches retained acquisition");
         Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "loan-dispatched query releases namespace roots");
      end Handle;
      procedure Reply_Event is
      begin Check (not Loans.Holding (State) and then not Owner.Active, "return acquisition precedes any reply"); Event (Owner, 'P'); end Reply_Event;
   begin
      for Kind in W.Operation loop
         Build (Kind, (if Kind = W.Find_By_Address then "PARN" else "DEV0")); Handle;
         Check (Result.Status = Loans.Completed and then Result.May_Reply and then Result.Released
           and then Owner.Seen_Peer = Expected_Peer and then Owner.Seen_Loan = Expected_Loan,
           "all query kinds use full opaque peer/reference through loan path");
         declare Hdr : constant R.Header_Result := R.Read_Envelope (Owner.Memory (1 .. Input'Length), Wire_Node_Limit); begin
            Check (Hdr.Accepted and then Hdr.Value.Status = W.OK and then Hdr.Value.Context.Key.Kind = Kind,
              "loan reply routed to correct handler");
            Check (R.Matches_Request (Owner.Memory (1 .. Input'Length), Input, Wire_Node_Limit),
              "reply correlates with private request despite mapped request mutation");
            Check (Owner.Dispatch.Routed and then Owner.Dispatch.Selected = Kind and then Result.Written = R.Extent (Hdr.Value),
              "unified dispatch reports exact envelope");
         end;
         Check ((for all I in Input'Length + 1 .. Owner.Memory'Last => Owner.Memory (I) = 16#AA#),
           "copy out preserves bytes beyond declared loan extent");
         Check ((for all I in Natural (Result.Written) + 1 .. Input'Length => Owner.Memory (I) = 0),
           "loan padding normalized after written envelope");
         Reply_Event; Check (Owner.Events (1 .. Owner.Event_Count) = "AIEORP", "normal acquire/clone/evaluate/copy/return/reply order");
      end loop;
      Build (W.Describe); Handle (Stamp => 0);
      Check (Result.Status = Loans.Denied and then Owner.Event_Count = 0 and then not Loans.Holding (State), "loan role admission before acquire");
      for Length of W.Word_Array'([0, WP.Frame_Byte_Count - 1, Input'Length + 1, W.Word'Last]) loop
         Handle (Length); Check (Result.Status = Loans.Invalid_Extent and then Owner.Event_Count = 0, "loan length bounded before narrowing/acquire");
      end loop;
      Handle (Peer => 255);
      Check (Result.Status = Loans.Acquire_Refused and then Owner.Seen_Peer = 255 and then Owner.Event_Count = 1,
        "wrong expected owner refused without clone or evaluate");
      for Mode in Failure range Acquire_Error .. Bad_Frame loop
         Build (W.Resources); Owner.Fail := Mode; Handle;
         Check (Result.Status = (case Mode is when Acquire_Error => Loans.Acquire_Refused,
           when Read_Error => Loans.Copy_In_Failed, when Evaluate_Error => Loans.Evaluation_Refused,
           when Write_Error => Loans.Copy_Out_Failed, when Bad_Count | Bad_Frame => Loans.Invalid_Response),
           "typed loan failure classification");
         Check (Result.May_Reply and then not Owner.Active and then Result.Written = 0
           and then (for all B of Output => B = 0), "every acquired failure returns before error reply");
         Check (Owner.Events (Owner.Event_Count) = (if Mode = Acquire_Error then 'A' else 'R'), "failure cleanup order");
      end loop;
      Build (W.Find_By_ID); Handle (Length => WP.Frame_Byte_Count);
      Check (Result.Status = Loans.Evaluation_Refused and then Result.Required_Bytes = R.Header_Byte_Count
        and then Result.May_Reply and then Owner.Events (1 .. Owner.Event_Count) = "AIER", "small response loan releases before size response");
      Build (W.Resources); Owner.Return_Fails := True; Handle;
      Check (Result.Status = Loans.Completed and then not Result.May_Reply and then Loans.Holding (State)
        and then not Result.Released and then Owner.Active, "failed return retains original acquisition and suppresses reply");
      Handle (Peer => 1, Ref => No_Ref);
      Check (Result.Status = Loans.Busy and then not Result.May_Reply and then Owner.Events (1 .. Owner.Event_Count) = "AIEOR",
        "pending return blocks another acquisition without losing original reference");
      declare Released : Boolean; begin
         Loans.Retry_Return (State, Owner, Released);
         Check (not Released and then Loans.Holding (State) and then Owner.Returned_Loan = Expected_Loan,
           "failed cleanup retry still retains original loan");
         Owner.Return_Fails := False; Loans.Retry_Return (State, Owner, Released);
         Check (Released and then not Loans.Holding (State) and then not Owner.Active, "cleanup retry releases before publication");
         Reply_Event;
         Check (Owner.Events (1 .. Owner.Event_Count) = "AIEORRRP", "no reply until successful return retry");
         Loans.Retry_Return (State, Owner, Released);
         Check (Released and then Owner.Events (1 .. Owner.Event_Count) = "AIEORRRP", "idle cleanup retry does not double return");
      end;
      declare
         Nodes : Q.Node_Array (0 .. 7);
         Parents : Paths.Node_Array (0 .. 7);
         Reader_Data, Text_Source : Firmware_Tables.Bytes (1 .. 512);
         Atoms : P.Atoms (0 .. 7);
         Filter, Payload : WP.Byte_Array (0 .. 1023);
         Routed : U.Report;
      begin
         Input := [others => 0];
         U.Handle (Service, 0, Scope, Input, C.Max_Namespace_Nodes, 32,
           Nodes, Parents, Reader_Data, Atoms, Text_Source, Filter, Payload, Output, Routed);
         Check (Routed.Status = U.Denied and then not Routed.Routed and then (for all B of Output => B = 0),
           "unified dispatcher admits before even malformed frame decode");
         U.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Input, C.Max_Namespace_Nodes, 32,
           Nodes, Parents, Reader_Data, Atoms, Text_Source, Filter, Payload, Output, Routed);
         Check (Routed.Status = U.Invalid_Request and then not Routed.Routed and then (for all B of Output => B = 0),
           "unified dispatcher refuses malformed copied frame without routing");
      end;
      Build (W.Resources);
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Answered and then not Session_Result.Immediate_Reply
        and then Sessions.Ready (Session) and then not Sessions.Has_Saved_Reply (Session)
        and then Owner.Answers = 1 and then Owner.Last_Written > 0 and then Owner.Events (1 .. Owner.Event_Count) = "SAIEORP",
        "session saves before acquire and answers after return");
      Build (W.Describe); Owner.Save_Fails := True;
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Save_Refused and then Session_Result.Immediate_Reply
        and then Sessions.Ready (Session) and then Owner.Events (1 .. Owner.Event_Count) = "S" and then not Owner.Active,
        "save failure preserves current reply and touches no loan");
      Build (W.Describe);
      Sessions.Begin_Request (Session, Owner, 0, Expected_Peer, Expected_Loan, Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Denied and then Session_Result.Immediate_Reply and then Owner.Event_Count = 0,
        "session admits role before saving reply");
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        W.Word'Last, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Invalid_Extent and then Session_Result.Immediate_Reply and then Owner.Event_Count = 0,
        "session extent admission matches loan policy before saving reply");
      Build (W.Resources); Owner.Return_Fails := True;
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Deferred and then not Session_Result.Immediate_Reply
        and then Sessions.Has_Saved_Reply (Session) and then not Sessions.Ready (Session)
        and then Owner.Saved_Live and then Owner.Answers = 0 and then Owner.Events (1 .. Owner.Event_Count) = "SAIEOR",
        "session preserves original saved reply and completed result while cleanup pending");
      Event (Owner, 'F');
      Sessions.Progress (Session, Owner, Session_Result);
      Check (Session_Result.Status = Sessions.Deferred and then Owner.Events (1 .. Owner.Event_Count) = "SAIEORFR"
        and then Owner.Answers = 0, "one cleanup attempt per event-loop checkpoint with other work between attempts");
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, 1, No_Ref, Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Busy and then Session_Result.Immediate_Reply
        and then Owner.Saved_Live and then Owner.Events (1 .. Owner.Event_Count) = "SAIEORFR",
        "new request refused without replacing pending reply or acquiring another loan");
      Owner.Return_Fails := False; Sessions.Progress (Session, Owner, Session_Result);
      Check (Session_Result.Status = Sessions.Answered and then Session_Result.Operation.Status = Loans.Completed
        and then Sessions.Ready (Session) and then Owner.Answers = 1 and then Owner.Last_Written > 0
        and then Owner.Events (1 .. Owner.Event_Count) = "SAIEORFRRP", "event-loop cleanup success answers saved result without replay");
      Sessions.Progress (Session, Owner, Session_Result);
      Check (Session_Result.Status = Sessions.Idle and then Owner.Answers = 1, "idle checkpoint never answers twice");
      Build (W.Find_By_ID); Owner.Answer_Fails := True;
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Delivery_Lost and then Sessions.Ready (Session)
        and then not Owner.Saved_Live and then Owner.Events (1 .. Owner.Event_Count) = "SAIEORP", "lost delivery consumes saved authority without work replay");
      Sessions.Progress (Session, Owner, Session_Result);
      Check (Session_Result.Status = Sessions.Idle and then Owner.Answers = 1, "failed answer is not retried with a consumed handle");
      Build (W.Resources); Owner.Fail := Read_Error; Owner.Return_Fails := True;
      Sessions.Begin_Request (Session, Owner, ACPI_Protocol.Observer_Tag, Expected_Peer, Expected_Loan,
        Input'Length, Input, Output, Session_Result);
      Check (Session_Result.Status = Sessions.Deferred and then Session_Result.Operation.Status = Loans.Copy_In_Failed,
        "pending cleanup retains original error outcome");
      Owner.Return_Fails := False; Sessions.Progress (Session, Owner, Session_Result);
      Check (Session_Result.Status = Sessions.Answered and then Session_Result.Operation.Status = Loans.Copy_In_Failed
        and then Owner.Last_Written = 0 and then Owner.Events (1 .. Owner.Event_Count) = "SAIRRP",
        "deferred error answered after cleanup without reevaluation");
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Clock_Usable := False;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Waited and then Owner.Wait_Deadline = W.Word'Last
           and then Owner.Events (1 .. Owner.Event_Count) = "LFQW", "idle explicit forever wait needs no fabricated clock");
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Waited and then Owner.Events (1 .. Owner.Event_Count) = "LFQWLFQW",
           "spurious wake repeats typed polls and wait");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Firmware_Ready := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Worked and then Result.Firmware_Handled and then Result.Request_Taken
           and then Result.Request.Status = Sessions.Answered and then not Pump.Pending (State)
           and then Owner.Events (1 .. Owner.Event_Count) = "LFQSAIEORP",
           "local checkpoint and firmware poll precede service request with no busy wait");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Return_Fails := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Worked and then Pump.Pending (State) and then Owner.Answers = 0, "pump preserves deferred query session");
         Owner.Event_Count := 0;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Waited and then Result.Wait_Deadline = 110
           and then Owner.Events (1 .. Owner.Event_Count) = "LRFQW", "failed cleanup alone schedules finite retry without spin");
         Owner.Event_Count := 0; Owner.Request_Ready := True; Owner.Firmware_Ready := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Worked and then Result.Firmware_Handled and then Result.Request.Status = Sessions.Busy
           and then Result.Immediate_Delivered and then Owner.Events (1 .. Owner.Event_Count) = "LRFQB",
           "continuous service work cannot suppress cleanup checkpoint or firmware poll");
         Owner.Event_Count := 0; Owner.Now := 105; Owner.Return_Fails := False;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Worked and then Result.Cleanup.Status = Sessions.Answered
           and then not Pump.Pending (State) and then Owner.Answers = 1 and then Owner.Events (1 .. Owner.Event_Count) = "LRPFQ",
           "cleanup checkpoint answers saved result once and continues typed polls");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Return_Fails := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Owner.Local_Deadline := 105; Owner.Event_Count := 0;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Waited and then Result.Wait_Deadline = 105, "earlier local deadline wins over cleanup timer");
         Owner.Return_Fails := False; Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (not Pump.Pending (State), "earlier-deadline case releases held query");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Return_Fails := True; Owner.Clock_Usable := False;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Retry_Clock_Unavailable and then Pump.Pending (State) and then Owner.Saved_Live,
           "clockless pending cleanup never fabricates an indefinite retry deadline");
         Owner.Clock_Usable := True; Owner.Return_Fails := False; Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (not Pump.Pending (State), "clock recovery preserves and releases original session");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Return_Fails := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Owner.Now := 99; Owner.Event_Count := 0;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Clock_Regressed and then Result.Observed_Now = 99 and then Result.Previous_Now = 100
           and then Owner.Events (1 .. Owner.Event_Count) = "L" and then Pump.Pending (State), "backwards clock stops before polling without dropping saved state");
         Owner.Now := 101; Owner.Return_Fails := False; Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (not Pump.Pending (State), "regressed-clock recovery releases original session");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Return_Fails := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Owner.Now := W.Word'Last - 5;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Deadline_Overflow and then Pump.Pending (State), "retry addition cannot wrap or collide with forever");
         Owner.Return_Fails := False; Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (not Pump.Pending (State), "overflow case can finish cleanup without inventing a deadline");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Local_Ready := False; Owner.Request_Ready := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Local_Stopped and then Owner.Events (1 .. Owner.Event_Count) = "L"
           and then not Pump.Pending (State), "expired local policy stops before taking a request");
         Owner.Local_Ready := True; Owner.Request_Ready := False; Owner.Wait_Usable := False;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Wait_Unavailable, "native wait failure is explicit");
      end;
      declare State : Pump.State; Result : Pump.Report; begin
         Build (W.Resources); Owner.Request_Ready := True; Owner.Other_Request := True;
         Pump.Step (State, Owner, 10, Input, Output, Result);
         Check (Result.Status = Pump.Worked and then Result.Request_Taken and then Owner.Events (1 .. Owner.Event_Count) = "LFQ"
           and then Owner.Answers = 0, "other typed service adapter retains ownership of its work");
      end;
      Counter_Zero;
   end Loan_Dispatch_Checks;
   procedure Resources_Checks is
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      Item : W.Request;
      Frame : WP.Byte_Array (101 .. 101 + WP.Frame_Byte_Count - 1);
      Response : WP.Byte_Array (Natural'Last - 4095 .. Natural'Last);
      Payload : WP.Byte_Array (Natural'Last - 1023 .. Natural'Last);
      Template : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
      Source : Firmware_Tables.Bytes (Natural'Last - 127 .. Natural'Last);
      Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
      Report : G.Report;
      Last_Size : Natural := Response'Length;
      procedure Start (Device : String) is
      begin
         Item := ((Kind => W.Resources, Instance => Scope.Instance, Revision => Scope.Revision,
                   Node => W.Node_Index (C.Values.Position (Find (Service, Device))), others => <>), (Present => False));
      end Start;
      procedure Cursor (Offset, Subentry : RR.U32) is
      begin Item.Continuation := (True, (Item.Key, W.Resource_Subentry, 0, Offset, Subentry)); end Cursor;
      procedure Invoke (Size : Natural := Response'Length; Source_Size : Natural := Source'Length;
                        Template_Size : Natural := Template'Length; Payload_Size : Natural := Payload'Length;
                        Quota : G.Data_IO.Item_Count := 32; Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
                        Owner : Q.Scope_State := Scope) is
         Written : W.Word; Accepted : Boolean;
      begin
         WP.Encode_Request (Item, [], Frame, Wire_Node_Limit, Written, Accepted);
         Check (Accepted and then Written = WP.Frame_Byte_Count, "Resources request encoded"); Last_Size := Size;
         G.Handle (Service, Stamp, Owner, Frame, Quota,
           Template (Template'First .. Template'First + (Template_Size - 1)), Atoms,
           Source (Source'First .. Source'First + (Source_Size - 1)),
           Payload (Payload'First .. Payload'First + (Payload_Size - 1)),
           Response (Response'First .. Response'First + (Size - 1)), Report);
         Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "Resources releases temporary roots");
      end Invoke;
      function Header return R.Header is
         Decoded : constant R.Header_Result := R.Read_Envelope
           (Response (Response'First .. Response'First + (Last_Size - 1)), Wire_Node_Limit);
      begin
         Check (Report.Status = G.Reply_Ready and then Decoded.Accepted, "Resources complete envelope");
         Check (R.Matches_Request (Response (Response'First .. Response'First + (Last_Size - 1)), Frame, Wire_Node_Limit),
           "Resources correlated complete reply");
         Check (Report.Written = R.Extent (Decoded.Value), "Resources exact written extent"); return Decoded.Value;
      end Header;
      function Record_Value return RR.Resource_Record is
         Decoded : constant RR.Resource_Result := R.Resource_At
           (Response (Response'First .. Response'First + (Last_Size - 1)), 0, Wire_Node_Limit);
      begin Check (Decoded.Accepted, "client decodes actual resource record"); return Decoded.Value; end Record_Value;
   begin
      Start ("DEV0"); Invoke;
      declare Hdr : constant R.Header := Header; Value : constant RR.Resource_Record := Record_Value; begin
         Check (Hdr.Status = W.OK and then Hdr.Record_Count = 1 and then not Hdr.Context.Continuation.Present
           and then Value.Kind = RR.Fixed_Memory and then Value.Fixed.Base = 16#FD000000#,
           "real fixed memory Resources reply terminates before EndTag");
      end;
      Start ("ECID");
      declare
         Total : Natural := 0; Finished : Boolean := False;
      begin
         for Step in 1 .. 10 loop
            Invoke (Size => R.Header_Byte_Count + R.Resource_Byte_Count + 6);
            declare Hdr : constant R.Header := Header; Value : constant RR.Resource_Record := Record_Value; begin
               Check (Hdr.Status = W.OK and then Hdr.Record_Count = 1 and then Report.Checked_Descriptors = 4,
                 "whole mixed template preflight before bounded resource page");
               case Total is
                  when 0 | 1 =>
                     Check (Value.Kind = RR.Interrupt_Entry and then Value.IRQ.GSI = (if Total = 0 then 3 else 5)
                       and then Value.Source_Offset = 0 and then Value.Subentry = RR.U32 (Total), "IRQ subentry cursor order");
                  when 2 | 3 | 4 =>
                     Check (Value.Kind = RR.GPIO_Pin and then Value.GPIO.Pin = (if Total = 4 then 16#FFFF# else 7)
                       and then Value.Source_Offset = 4 and then Value.Subentry = RR.U32 (Total - 2), "GPIO duplicate/sentinel wire order");
                  when 5 =>
                     Check (Value.Kind = RR.I2C_Connection and then Value.I2C.Slave_Address = 16#2C#
                       and then Value.Source_Offset = 39, "I2C descriptor wire page");
                  when 6 => Check (Value.Kind = RR.Unsupported_Descriptor and then Value.Name = 14
                    and then Value.Source_Offset = 63, "unknown descriptor remains explicit");
                  when others => Check (False, "no extra resource page");
               end case;
               if Value.Controller.Kind = RR.Unresolved_Path then
                  Check (Value.Controller.Path.Length = 6 and then Value.Controller.Path.Offset = R.Resource_Byte_Count,
                    "complete controller path span after record directory");
                  declare Expected : constant String := (if Value.Kind = RR.GPIO_Pin then "\GPIO" else "\I2C0"); begin
                     for I in 0 .. Expected'Length - 1 loop
                        Check (Response (Response'First + (R.Header_Byte_Count + R.Resource_Byte_Count + I)) =
                          Character'Pos (Expected (Expected'First + I)), "complete controller path byte through reply");
                     end loop;
                     Check (Response (Response'First + (R.Header_Byte_Count + R.Resource_Byte_Count + Expected'Length)) = 0,
                       "controller path includes terminator");
                  end;
               end if;
               Total := Total + 1;
               if not Hdr.Context.Continuation.Present then Finished := True; exit; end if;
               Check (Hdr.Context /= Item, "resource continuation progresses"); Item := Hdr.Context;
            end;
         end loop;
         Check (Finished and then Total = 7, "all mixed resource subentries exactly once");
      end;
      Start ("INT0"); Invoke (Size => R.Header_Byte_Count, Source_Size => 0, Payload_Size => 0);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.OK and then Hdr.Record_Count = 0
        and then not Hdr.Context.Continuation.Present, "empty static template distinct from missing CRS"); end;
      Start ("NONE"); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Not_Found, "missing CRS typed not-found"); end;
      Start ("METH"); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused
        and then Report.Observation.Status = P.Method_Valued, "method CRS never invoked by passive endpoint"); end;
      Start ("ACID"); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Invalid_Template
        and then Report.Scan.Status = G.Data_IO.Bad_Checksum, "whole template checksum checked"); end;
      Start ("LCID"); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Invalid_Descriptor
        and then Report.Checked_Descriptors = 2 and then Report.Failed_Offset = 12 and then Hdr.Record_Count = 0,
        "malformed late descriptor prevents earlier resource publication"); end;
      Start ("ECID"); Invoke (Quota => 3);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Template_Quota,
        "explicit owner item budget bounds template work"); end;
      Invoke (Template_Size => 66);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Internal_Scratch_Limit
        and then Report.Observation.Required_Bytes = 67, "template copy pressure is owner scratch"); end;
      Invoke (Source_Size => 5);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Internal_Scratch_Limit
        and then Report.Required_Source_Bytes = 6, "full source validation pressure is owner scratch"); end;
      Cursor (1,0); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "header-like payload byte is not a descriptor boundary"); end;
      Cursor (4,3); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "GPIO terminal subentry refused"); end;
      Cursor (65,0); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "EndTag is not a resource continuation"); end;
      Cursor (RR.U32'Last,0); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "wide resource cursor checked before narrowing"); end;
      Cursor (39,0); Invoke (Size => R.Header_Byte_Count + R.Resource_Byte_Count + 5);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Insufficient_Space
        and then Hdr.Required_Bytes = R.Header_Byte_Count + R.Resource_Byte_Count + 6,
        "I2C full path requires explicit larger client loan"); end;
      Start ("DEV0"); Invoke (Payload_Size => R.Resource_Byte_Count - 1);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = G.Internal_Scratch_Limit,
        "private payload capacity not confused with client capacity"); end;
      Invoke (Size => R.Header_Byte_Count);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Insufficient_Space
        and then Hdr.Required_Bytes = R.Header_Byte_Count + R.Resource_Byte_Count, "fixed resource exact client size hint"); end;
      Invoke (Stamp => 0); Check (Report.Status = G.Denied and then not Report.Backend_Invoked, "Resources role admission before backend");
      Invoke (Size => R.Header_Byte_Count - 1);
      Check (Report.Status = G.Reply_Space_Required and then not Report.Backend_Invoked, "Resources header size admission");
      Invoke (Owner => (False, Scope.Instance, Scope.Revision));
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Namespace_Not_Loaded, "Resources owner readiness"); end;
      Item.Key.Instance := 1; Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Stale_Generation, "Resources opaque full-width instance scope"); end;
      Frame := [others => 0];
      G.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Frame, 32, Template, Atoms, Source, Payload, Response, Report);
      Check (Report.Status = G.Invalid_Request and then not Report.Backend_Invoked
        and then (for all B of Response => B = 0), "malformed resource request refuses without backend");
      Counter_Zero;
   end Resources_Checks;
   procedure Resource_Projection_Checks is
      Packet : Firmware_Tables.Bytes (Natural'Last - 255 .. Natural'Last) := [others => 0];
      Source : Firmware_Tables.Bytes (Natural'Last - 63 .. Natural'Last);
      Value : RR.Resource_Result;
      Report : Project.Report;
      Size : Natural := 0;
      Pool : constant RR.U32 := R.Resource_Byte_Count;
      procedure Put (At_Byte : Natural; Number : W.Word; Bytes : Positive := 1) is
         Remaining : W.Word := Number;
      begin
         for I in 0 .. Bytes - 1 loop
            Packet (Packet'First + (At_Byte + I)) := Firmware_Tables.Byte (Remaining mod 256);
            Remaining := Remaining / 256;
         end loop;
      end Put;
      procedure Large (Name, Length : Natural) is
      begin
         Packet := [others => 0]; Size := Length + 3;
         Put (0, W.Word (128 + Name)); Put (1, W.Word (Length), 2);
      end Large;
      procedure Load (Bytes : Firmware_Tables.Bytes) is
      begin
         Packet := [others => 0]; Size := Bytes'Length;
         for I in 0 .. Bytes'Length - 1 loop Packet (Packet'First + I) := Bytes (Bytes'First + I); end loop;
      end Load;
      procedure Probe (Expected : Project.Outcome := Project.Record_Ready; Subentry : RR.U32 := 0;
                       Source_Size : Natural := Source'Length; Pool_Offset : RR.U32 := Pool) is
      begin
         Project.Read (Packet (Packet'First .. Packet'First + (Size - 1)), 0, Subentry, Pool_Offset,
           Source (Source'First .. Source'First + (Source_Size - 1)), Value, Report);
         Check (Report.Status = Expected, "resource projection typed outcome");
         if Expected = Project.Record_Ready then
            Check (Value.Accepted and then RR.Valid (Value.Value, Wire_Node_Limit,
              Pool_Offset + RR.U32 (Report.Source_Bytes)), "resource projection valid typed record");
            declare
               Header : constant R.Header := (Status => W.OK,
                 Context => ((Kind => W.Resources, Instance => W.Instance_ID'Last, Revision => W.Generation'Last,
                   Node => 1, others => <>), (Present => False)),
                 Payload_Bytes => Pool + RR.U32 (Report.Source_Bytes), Record_Count => 1, others => <>);
               Payload : WP.Byte_Array (101 .. 101 + R.Resource_Byte_Count + Source'Length - 1) := [others => 0];
               Response : WP.Byte_Array (101 .. 101 + R.Header_Byte_Count + Payload'Length - 1);
               Words : constant RR.Record_Words := RR.Encode (Value.Value, Wire_Node_Limit, Header.Payload_Bytes);
               Written : W.Word; Accepted : Boolean; Decoded : RR.Resource_Result;
            begin
               for I in Words'Range loop
                  declare Number : W.Word := Words (I); begin
                     for J in 0 .. WP.Bytes_Per_Word - 1 loop
                        Payload (Payload'First + (I * WP.Bytes_Per_Word + J)) := WP.Byte (Number mod 256); Number := Number / 256;
                     end loop;
                  end;
               end loop;
               for I in 0 .. Natural (Report.Source_Bytes) - 1 loop
                  Payload (Payload'First + (Natural (Pool) + I)) := Source (Source'First + I);
               end loop;
               R.Encode_Envelope (Header, Payload (Payload'First .. Payload'First + (Natural (Header.Payload_Bytes) - 1)),
                 Response, Wire_Node_Limit, Written, Accepted);
               Check (Accepted and then Written = R.Extent (Header), "projected resource actual reply envelope");
               Decoded := R.Resource_At (Response, 0, Wire_Node_Limit);
               Check (Decoded.Accepted and then Decoded.Value = Value.Value, "client decodes projected resource reply");
            end;
         else
            Check (not Value.Accepted and then (for all B of Source (Source'First .. Source'First + (Source_Size - 1)) => B = 0),
              "resource projection refuses without source output");
         end if;
      end Probe;
      I2C : constant Firmware_Tables.Bytes := [16#8E#,21,0,2,0,1,6,0,0,1,6,0,16#A0#,16#86#,1,0,16#2C#,0,
        Character'Pos ('\'),Character'Pos ('I'),Character'Pos ('2'),Character'Pos ('C'),Character'Pos ('0'),0];
      Pins : constant Firmware_Tables.Bytes := [16#8C#,32,0,1,1,1,0,9,0,0,0,0,0,0,23,0,0,29,0,35,0,0,0,
        7,0,7,0,16#FF#,16#FF#,Character'Pos ('\'),Character'Pos ('G'),Character'Pos ('P'),Character'Pos ('I'),Character'Pos ('O'),0];
   begin
      P.Read (Service, Find (Service, "DEV0"), P.Current_Resources, Packet, Items, Result);
      Check (Result.Status = P.Static_Value and then Result.Copied_Bytes = 14, "real static CRS copy for projection");
      Size := Result.Copied_Bytes; Probe;
      Check (Value.Value.Kind = RR.Fixed_Memory and then Value.Value.Fixed.Base = 16#FD00_0000#
        and then Value.Value.Fixed.Length = 4096 and then Value.Value.Fixed.Writeable, "real AML fixed memory projected");
      Project.Read (Packet (Packet'First .. Packet'First + (Size - 1)), Report.Next_Offset, 0, Pool, Source, Value, Report);
      Check (Report.Status = Project.End_Tag and then not Value.Accepted, "real CRS EndTag terminates projection");
      Load (I2C); Probe;
      Check (Value.Value.Kind = RR.I2C_Connection and then Value.Value.I2C.Slave_Address = 16#2C#
        and then Value.Value.I2C.Speed_Hertz = 100_000 and then Value.Value.Shared
        and then Value.Value.Controller.Kind = RR.Unresolved_Path and then Report.Source_Bytes = 6, "I2C facts and complete source");
      for I in 0 .. 5 loop Check (Source (Source'First + I) = I2C (I2C'Last - 5 + I), "full I2C source bytes including NUL"); end loop;
      Probe (Project.Output_Required, Source_Size => 5); Check (Report.Required_Source_Bytes = 6, "full source capacity hint");
      Probe (Project.Invalid_Pool_Offset, Pool_Offset => RR.U32'Last);
      Probe (Project.Bad_Subentry, Subentry => RR.U32'Last);
      Put (7, 1, 2); Put (16, 16#3FF#, 2); Probe;
      Check (Value.Value.I2C.Addressing = RR.Ten_Bit and then Value.Value.I2C.Slave_Address = 1023, "ten-bit I2C address preserved");
      Put (16, 1024, 2); Probe (Project.Malformed_Descriptor);
      Load (I2C); Put (3,3); Probe;
      Check (Value.Value.Kind = RR.Unsupported_Descriptor, "future serial revision retained without interpretation");
      Load (I2C); Put (9,2); Probe;
      Check (Value.Value.Kind = RR.Unsupported_Descriptor, "future I2C type revision retained without interpretation");
      Load (Pins);
      for Pin in 0 .. 2 loop
         Probe (Subentry => RR.U32 (Pin));
         Check (Value.Value.Kind = RR.GPIO_Pin and then Value.Value.Total_Subentries = 3
           and then Value.Value.Subentry = RR.U32 (Pin) and then Value.Value.GPIO.Restriction = RR.Input_Only,
           "GPIO IO pin list preserves position and restriction");
         Check (Value.Value.GPIO.Pin = (if Pin = 2 then 16#FFFF# else 7), "duplicate pins and no-pin sentinel preserved");
         Check ((if Pin < 2 then Report.Next_Offset = 0 and then Report.Next_Subentry = RR.U32 (Pin + 1)
                 else Report.Next_Offset = 35 and then Report.Next_Subentry = 0), "GPIO subentry cursor advances");
      end loop;
      Probe (Project.Bad_Subentry, Subentry => 3);
      Put (3,2); Probe;
      Check (Value.Value.Kind = RR.Unsupported_Descriptor, "future GPIO revision retained without pin interpretation");
      Load ([16#23#,16#28#,0,16#31#]);
      Probe; Check (Value.Value.Kind = RR.Interrupt_Entry and then Value.Value.IRQ.GSI = 3
        and then Value.Value.IRQ.Mode = RR.Edge and then Value.Value.Shared and then Value.Value.IRQ.Wake, "PIC IRQ flags and first mask bit");
      Probe (Subentry => 1); Check (Value.Value.IRQ.GSI = 5 and then Report.Next_Offset = 4, "PIC IRQ mask flattening");
      Probe (Project.Bad_Subentry, Subentry => 2);
      Load ([16#22#,0,0]); Probe (Project.Empty_Descriptor);
      Load ([16#47#,1,16#F8#,3,16#F8#,3,1,8]); Probe;
      Check (Value.Value.Kind = RR.IO_Range and then Value.Value.IO.Minimum = 16#3F8# and then Value.Value.IO.Length = 8, "IO port range");
      Load ([16#4B#,16#F8#,3,8]); Probe; Check (Value.Value.IO.Fixed, "fixed IO");
      Load ([16#81#,9,0,1,0,1,0,2,0,0,16#10#,0]); Probe;
      Check (Value.Value.Kind = RR.Memory_Range and then Value.Value.Memory.Encoding = RR.Memory_24
        and then Value.Value.Memory.Minimum_Base = 65536 and then Value.Value.Memory.Alignment = 65536
        and then Value.Value.Memory.Length = 4096, "Memory24 scaling and zero alignment");
      Large (5,17); Put (3,1); Put (4,16#12345678#,4); Put (8,16#23456789#,4); Put (12,4096,4); Put (16,8192,4); Probe;
      Check (Value.Value.Memory.Encoding = RR.Memory_32 and then Value.Value.Memory.Minimum_Base = 16#12345678#, "Memory32 values");
      for Width in 0 .. 2 loop
         declare Bytes : constant Positive := 2 ** (Width + 1); Name : constant Natural := (if Width = 0 then 8 elsif Width = 1 then 7 else 10); begin
            Large (Name,3 + 5 * Bytes); Put (4,1); Put (5,1); Put (6 + Bytes,16#1234#,Bytes); Probe;
            Check (Value.Value.Kind = RR.Address_Window and then Value.Value.Window.Minimum = 16#1234#
              and then Value.Value.Window.Encoding = RR.Address_Encoding'Val (Width), "address window widths");
         end;
      end loop;
      Large (11,53); Put (4,1); Put (5,1); Put (6,1); Put (16,16#FEDCBA9876543210#,8); Probe;
      Check (Value.Value.Window.Encoding = RR.Extended_Address and then Value.Value.Window.Revision = 1
        and then Value.Value.Window.Minimum = 16#FEDCBA9876543210#, "extended window full-width minimum");
      Load ([16#89#,6,0,16#1F#,1,16#78#,16#56#,16#34#,16#12#]); Probe;
      Check (Value.Value.Kind = RR.Interrupt_Entry and then Value.Value.IRQ.GSI = 16#12345678#, "extended interrupt full GSI");
      Load ([16#71#,16#AB#]); Probe; Check (Value.Value.Kind = RR.Unsupported_Descriptor and then Value.Value.Name = 14,
        "unknown descriptor identity retained");
      Load ([16#8E#,16#FF#,0]); Probe (Project.Malformed_Descriptor);
      Load ([16#78#]); Probe (Project.Malformed_Descriptor);
      Load ([16#79#,0]); Probe (Project.End_Tag);
      Counter_Zero;
   end Resource_Projection_Checks;
   procedure Describe_Checks is
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      Item : W.Request;
      Frame : WP.Byte_Array (101 .. 101 + WP.Frame_Byte_Count - 1);
      Response : WP.Byte_Array (Natural'Last - 4095 .. Natural'Last);
      Payload : WP.Byte_Array (Natural'Last - 2047 .. Natural'Last);
      Scratch : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
      Text : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
      Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
      Parents : Paths.Node_Array (Natural'Last - 7 .. Natural'Last);
      Report : D.Report;
      Last_Size : Natural := Response'Length;
      Counts : array (F.Field_Name) of Natural := [others => 0];
      Path_Value, UID_Value : String (1 .. 128) := [others => ' '];
      Path_Length, UID_Length : Natural := 0;
      Last_Field : F.Field_Name := F.Device_Path;
      Saw_Empty_CID : Boolean := False;
      procedure Start (Device : C.Node_Descriptor) is
      begin
         Item := ((Kind => W.Describe, Instance => Scope.Instance, Revision => Scope.Revision,
                   Node => W.Node_Index (C.Values.Position (Device)), others => <>), (Present => False));
      end Start;
      procedure Invoke (Size : Natural := Response'Length; Text_Size : Natural := Text'Length;
                        Parent_Size : Natural := Parents'Length; Reader_Size : Natural := Scratch'Length;
                        Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
                        Owner : Q.Scope_State := Scope) is
         Written : W.Word;
         Accepted : Boolean;
      begin
         WP.Encode_Request (Item, [], Frame, Wire_Node_Limit, Written, Accepted);
         Check (Accepted and then Written = WP.Frame_Byte_Count, "Describe request encoding");
         Last_Size := Size;
         D.Handle (Service, Stamp, Owner, Frame,
           Parents (Parents'First .. Parents'First + (Parent_Size - 1)),
           Scratch (Scratch'First .. Scratch'First + (Reader_Size - 1)), Atoms,
           Text (Text'First .. Text'First + (Text_Size - 1)), Payload,
           Response (Response'First .. Response'First + (Size - 1)), Report);
         Check (C.Valid (Service) and then C.Retained_Results (Service) = 0, "Describe releases all temporary roots");
      end Invoke;
      function Header return R.Header is
         Decoded : constant R.Header_Result := R.Read_Envelope
           (Response (Response'First .. Response'First + (Last_Size - 1)), Wire_Node_Limit);
      begin
         Check (Report.Status = D.Reply_Ready and then Decoded.Accepted, "Describe complete envelope accepted");
         Check (R.Matches_Request (Response (Response'First .. Response'First + (Last_Size - 1)), Frame, Wire_Node_Limit),
           "Describe reply correlated with original bytes");
         Check (Report.Written = R.Extent (Decoded.Value), "Describe exact written extent");
         return Decoded.Value;
      end Header;
      function Field_Text (Value : F.Field_Record) return String is
         Value_Text : String (1 .. Natural (Value.Text.Length));
      begin
         for I in Value_Text'Range loop
            Value_Text (I) := Character'Val (Response (Response'First +
              (R.Header_Byte_Count + Natural (Value.Text.Offset) + I - 1)));
         end loop;
         return Value_Text;
      end Field_Text;
      procedure Crawl (Device : C.Node_Descriptor; Small : Boolean := False) is
         Finished : Boolean := False;
      begin
         Start (Device); Counts := [others => 0]; Path_Length := 0; UID_Length := 0;
         Last_Field := F.Device_Path; Saw_Empty_CID := False;
         for Step in 1 .. 100 loop
            Invoke (Size => (if not Small then Response'Length
              elsif not Item.Continuation.Present or else Item.Continuation.Value.Part = W.Description_Path then 517 else 645));
            declare Hdr : constant R.Header := Header; begin
               Check (Hdr.Status = W.OK and then Hdr.Record_Count <= 3 and then not Hdr.CIDs_Truncated, "Describe page bounded and complete");
               if Hdr.Record_Count = 0 then
                  Check (Item.Continuation.Present and then Item.Continuation.Value.Part = W.Description_CID
                    and then Hdr.Context.Continuation.Present and then Hdr.Context.Continuation.Value.Part = W.Description_UID,
                    "empty static CID page advances to UID");
                  Saw_Empty_CID := True;
               end if;
               for Position in 0 .. Natural (Hdr.Record_Count) - 1 loop
                  declare
                     Value : constant F.Field_Result := F.Field_At
                       (Response (Response'First + R.Header_Byte_Count .. Response'First +
                         (R.Header_Byte_Count + Natural (Hdr.Payload_Bytes) - 1)), Hdr.Record_Count, F.U32 (Position), Wire_Node_Limit);
                  begin
                     Check (Value.Accepted, "client decodes actual Describe field payload");
                     Check (F.Field_Name'Pos (Value.Value.Name) >= F.Field_Name'Pos (Last_Field), "Describe fields remain ordered");
                     Last_Field := Value.Value.Name; Counts (Last_Field) := Counts (Last_Field) + 1;
                     if Last_Field = F.Device_Path then
                        Check (Value.Value.Logical_Offset = W.Word (Path_Length), "path chunks contiguous");
                        declare Part : constant String := Field_Text (Value.Value); begin
                           Path_Value (Path_Length + 1 .. Path_Length + Part'Length) := Part;
                           Path_Length := Path_Length + Part'Length;
                        end;
                     elsif Last_Field = F.Unique_ID and then Value.Value.Kind = F.Text_Chunk then
                        Check (Value.Value.Logical_Offset = W.Word (UID_Length), "UID chunks contiguous");
                        declare Part : constant String := Field_Text (Value.Value); begin
                           UID_Value (UID_Length + 1 .. UID_Length + Part'Length) := Part;
                           UID_Length := UID_Length + Part'Length;
                        end;
                     elsif Last_Field = F.Compatible_ID and then Value.Value.Kind = F.Text_Chunk then
                        Check (W.Word (Value.Value.Text.Length) = Value.Value.Total_Length
                          and then Value.Value.Logical_Offset = 0, "CID text complete in one record");
                        if C.Values.Node_Name (Device) = "DEV0" then
                           Check (Field_Text (Value.Value) = "PCI\VEN_1234&DEV_ABCD" and then Value.Value.Ordinal = 3, "full bus CID through reply");
                        elsif C.Values.Node_Name (Device) = "LCID" then
                           Check (Field_Text (Value.Value) = "VENDOR\BUS_CHANNEL_0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_COMPLETE_IDENTIFIER_FINAL_Z",
                             "complete long CID through Describe reply");
                        end if;
                     end if;
                     if C.Values.Node_Name (Device) = "METH" and then Last_Field /= F.Device_Path then
                        Check (Value.Value.Status = W.Method_Valued, "Describe never invokes method-valued metadata");
                     elsif Last_Field = F.Device_Status and then C.Values.Node_Name (Device) = "NONE" then
                        Check (Value.Value.Status = W.Absent and then Value.Value.Defaulted and then Value.Value.Number = W.Default_STA,
                          "Describe explicit absent STA default");
                     elsif Last_Field = F.Device_Status and then C.Values.Node_Name (Device) = "ZSTA" then
                        Check (Value.Value.Status = W.Present and then not Value.Value.Defaulted and then Value.Value.Number = 0,
                          "Describe observed zero STA");
                     end if;
                  end;
               end loop;
               if not Hdr.Context.Continuation.Present then Finished := True; exit; end if;
               Check (Hdr.Context /= Item, "Describe continuation advances"); Item := Hdr.Context;
            end;
         end loop;
         Check (Finished and then Counts (F.Hardware_ID) = 1 and then Counts (F.Address) = 1
           and then Counts (F.Device_Status) = 1 and then Counts (F.Unique_ID) >= 1, "Describe complete fields without duplicates");
         Counter_Zero;
      end Crawl;
   begin
      Crawl (Find (Service, "DEV0"));
      Check (Path_Value (1 .. Path_Length) = "\DEV0" and then UID_Value (1 .. UID_Length) = "unit-a"
        and then Counts (F.Compatible_ID) = 4, "complete real device description");
      Crawl (Find (Service, "DEV0"), Small => True);
      Check (UID_Value (1 .. UID_Length) = "unit-a" and then Counts (F.Unique_ID) = 2, "UID reconstruction over two real replies");
      declare Child : C.Node_Descriptor; Node : C.Namespace.Node_ID; Status : C.Values.Access_Status; begin
         C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Node, Status);
         C.Describe_Node (Service, Node, Child, Status); Check (Status = C.Values.Available, "nested Describe fixture");
         Crawl (Child, Small => True);
         Check (Path_Value (1 .. Path_Length) = "\PARN.CHLD" and then Counts (F.Device_Path) = 2, "nested path reconstruction over real replies");
      end;
      Crawl (Find (Service, "METH")); Crawl (Find (Service, "NONE")); Crawl (Find (Service, "ZSTA"));
      Crawl (Find (Service, "LCID")); Crawl (Find (Service, "ACID")); Crawl (Find (Service, "INT0"));
      Crawl (Find (Service, "ECID")); Check (Saw_Empty_CID and then Counts (F.Compatible_ID) = 0, "empty CID distinct from missing fact");
      Start (Find (Service, "DEV0")); Invoke (Stamp => 0);
      Check (Report.Status = D.Denied and then not Report.Backend_Invoked and then Report.Written = 0,
        "Describe denied before backend");
      Invoke (Size => R.Header_Byte_Count - 1);
      Check (Report.Status = D.Reply_Space_Required and then not Report.Backend_Invoked, "Describe header capacity admission");
      Invoke (Size => 516);
      declare Hdr : constant R.Header := Header; begin
         Check (Hdr.Status = W.Insufficient_Space and then Hdr.Required_Bytes = 517, "path group exact minimum frame sizing");
      end;
      Invoke (Owner => (False, Scope.Instance, Scope.Revision));
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Namespace_Not_Loaded, "Describe not-ready frame"); end;
      Item.Key.Revision := 1; Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Stale_Generation, "Describe stale scope frame"); end;
      Start (Find (Service, "HITS")); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused, "Describe nondevice refused"); end;
      Start (Find (Service, "DEV0"));
      Item.Continuation := (True, (Item.Key, W.Description_Path, 0, 1, 0)); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "Describe terminal path cursor refused"); end;
      Item.Continuation := (True, (Item.Key, W.Description_CID, 0, 4, 0)); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "Describe terminal CID cursor refused"); end;
      Item.Continuation := (True, (Item.Key, W.Description_UID, 0, 6, 0)); Invoke;
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Bad_Cursor, "Describe terminal UID cursor refused"); end;
      Start (Find (Service, "LCID")); Item.Continuation := (True, (Item.Key, W.Description_CID, 0, 0, 0));
      Invoke (Size => R.Header_Byte_Count + F.Record_Byte_Count + 80);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Insufficient_Space, "complete long CID asks for larger loan"); end;
      Invoke (Text_Size => 1);
      declare Hdr : constant R.Header := Header; begin
         Check (Hdr.Status = W.Refused and then Report.Reason = D.Internal_Scratch_Limit, "CID owner text capacity is not client shortage");
      end;
      Start (Find (Service, "DEV0")); Invoke (Parent_Size => 0);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Report.Reason = D.Internal_Scratch_Limit,
        "parent scratch failure rolls back whole Describe"); end;
      Invoke (Reader_Size => 0);
      declare Hdr : constant R.Header := Header; begin Check (Hdr.Status = W.Refused and then Hdr.Record_Count = 0,
        "late HID read failure does not publish earlier path"); end;
      Frame := [others => 0];
      D.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Frame, Parents, Scratch, Atoms, Text, Payload, Response, Report);
      Check (Report.Status = D.Invalid_Request and then not Report.Backend_Invoked
        and then (for all B of Response => B = 0), "malformed Describe frame cleared before backend");
      Counter_Zero;
   end Describe_Checks;
   procedure Path_Checks is
      Scratch : Paths.Node_Array (Natural'Last - 7 .. Natural'Last);
      Text : Firmware_Tables.Bytes (Natural'Last - 31 .. Natural'Last);
      Field : F.Field_Result;
      Report : Paths.Report;
      Child : C.Node_Descriptor;
      Node : C.Namespace.Node_ID;
      Status : C.Values.Access_Status;
      Pool : constant F.U32 := F.Record_Byte_Count;
      procedure Read (Device : C.Node_Descriptor; Offset : F.U32 := 0;
                      Size : Natural := Text'Length; Slots : Natural := Scratch'Length) is
      begin
         Paths.Read (Service, Device, Offset, Pool,
           Scratch (Scratch'First .. Scratch'First + (Slots - 1)),
           Text (Text'First .. Text'First + (Size - 1)), Field, Report);
         Check (Report.Visited_Nodes <= C.Max_Namespace_Nodes and then C.Retained_Results (Service) = 0,
           "bounded path traversal without roots");
         if Report.Status = Paths.Field_Ready then
            Check (Field.Accepted and then Field.Value.Name = F.Device_Path
              and then Report.Copied_Segments > 0 and then Report.Text_Bytes <= W.Word (Size), "path forward progress");
            declare
               Encoded : constant F.Record_Words := F.Encode (Field.Value, Wire_Node_Limit, Pool + F.U32 (Report.Text_Bytes));
               Decoded : constant F.Field_Result := F.Decode (Encoded, Wire_Node_Limit, Pool + F.U32 (Report.Text_Bytes));
            begin
               Check (Decoded.Accepted and then Decoded.Value = Field.Value, "path field codec round trip");
            end;
         else
            Check (not Field.Accepted and then Report.Text_Bytes = 0 and then not Report.More
              and then (for all B of Text (Text'First .. Text'First + (Size - 1)) => B = 0),
              "path failure clears field and output");
         end if;
      end Read;
      procedure Is_Text (Value : String) is
      begin
         Check (Field.Accepted and then Report.Text_Bytes = W.Word (Value'Length), "path byte count");
         for I in 0 .. Value'Length - 1 loop
            Check (Text (Text'First + I) = Character'Pos (Value (Value'First + I)), "path byte");
         end loop;
      end Is_Text;
   begin
      Read (Find (Service, "DEV0")); Is_Text ("\DEV0");
      Check (Report.Total_Segments = 1 and then not Report.More and then Report.Next_Segment = 1,
        "single device complete path");
      C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Node, Status);
      Check (Status = C.Values.Available, "nested path lookup");
      C.Describe_Node (Service, Node, Child, Status); Check (Status = C.Values.Available, "nested path device");
      Read (Child); Is_Text ("\PARN.CHLD");
      Check (Report.Total_Segments = 2 and then Field.Value.Total_Length = 10, "complete nested path length");
      Read (Child, Size => 5); Is_Text ("\PARN");
      Check (Report.More and then Report.Next_Segment = 1 and then Field.Value.Logical_Offset = 0,
        "path cursor counts complete segments");
      Read (Child, Offset => Report.Next_Segment, Size => 5); Is_Text (".CHLD");
      Check (not Report.More and then Report.Next_Segment = 2 and then Field.Value.Logical_Offset = 5
        and then Field.Value.Total_Length = 10, "path continuation byte offset separate from segment cursor");
      for Size in 0 .. 10 loop
         Read (Child, Size => Size);
         Check ((if Size < 5 then Report.Status = Paths.Output_Required
                 else Report.Status = Paths.Field_Ready and then Report.Copied_Segments = Size / 5),
           "path capacity emits only whole segments");
      end loop;
      Read (Child, Slots => 1);
      Check (Report.Status = Paths.Scratch_Required and then Report.Required_Nodes = 2,
        "path scratch pressure reports full parent depth");
      Read (Child, Slots => 0);
      Check (Report.Status = Paths.Scratch_Required and then Report.Visited_Nodes = 2,
        "zero path scratch still bounds and sizes parent walk");
      Read (Child, Offset => 2); Check (Report.Status = Paths.Bad_Position, "terminal path cursor refused");
      Read (Child, Offset => F.U32'Last); Check (Report.Status = Paths.Bad_Position, "path cursor checked before narrowing");
      Read (Find (Other, "DEV0")); Check (Report.Status = Paths.Invalid_Device, "foreign path descriptor refused");
      Read (Find (Service, "HITS")); Check (Report.Status = Paths.Invalid_Device, "nondevice path refused");
      Paths.Read (Service, Child, 0, F.U32'Last, Scratch, Text, Field, Report);
      Check (Report.Status = Paths.Observation_Failed and then not Field.Accepted
        and then (for all B of Text => B = 0), "path span overflow clears output");
      Counter_Zero;
   end Path_Checks;
   procedure Identity_Checks is
      Scratch : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
      Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
      Text : Firmware_Tables.Bytes (Natural'Last - 127 .. Natural'Last);
      Field : F.Field_Result;
      Report : Identity.Report;
      Pool : constant F.U32 := F.Record_Byte_Count;
      procedure Read (Name : String; Member : Identity.Member_Name;
                      Ordinal : F.U32 := 0; Offset : W.Word := 0; Size : Natural := Text'Length) is
      begin
         Identity.Read (Service, Find (Service, Name), Member, Ordinal, Offset, Pool,
           Scratch, Atoms, Text (Text'First .. Text'First + (Size - 1)), Field, Report);
         Check (C.Valid (Service) and then C.Retained_Results (Service) = 0,
           "identity leaves valid service and no roots");
         if Field.Accepted then
            Check (Report.Status = Identity.Field_Ready, "accepted identity report");
            declare
               Payload : WP.Byte_Array (Natural'Last - 511 .. Natural'Last) := [others => 0];
               Words : constant F.Record_Words := F.Encode (Field.Value, Wire_Node_Limit,
                 Pool + F.U32 (Report.Text_Bytes));
               Value : F.Field_Result;
            begin
               for I in Words'Range loop
                  declare Number : W.Word := Words (I); begin
                     for J in 0 .. WP.Bytes_Per_Word - 1 loop
                        Payload (Payload'First + (I * WP.Bytes_Per_Word + J)) := WP.Byte (Number mod 256);
                        Number := Number / 256;
                     end loop;
                  end;
               end loop;
               for I in 0 .. Natural (Report.Text_Bytes) - 1 loop
                  Payload (Payload'First + (Natural (Pool) + I)) := Text (Text'First + I);
               end loop;
               Value := F.Field_At (Payload (Payload'First .. Payload'First +
                 (Natural (Pool) + Natural (Report.Text_Bytes) - 1)), 1, 0, Wire_Node_Limit);
               Check (Value.Accepted and then Value.Value = Field.Value,
                 "real identity field encodes and decodes through Describe payload");
            end;
         else
            Check (Report.Status /= Identity.Field_Ready, "refused identity has no field");
            Check ((for all B of Text (Text'First .. Text'First + (Size - 1)) => B = 0),
              "refused identity clears text");
         end if;
      end Read;
      procedure Is_ID (Value : String) is
      begin
         Check (Field.Accepted and then Field.Value.Status = W.Present
           and then Field.Value.Kind = F.Identifier and then W.Image (Field.Value.ID) = Value,
           "identity canonical ID " & Value);
      end Is_ID;
      procedure Is_Text (Value : String) is
      begin
         Check (Field.Accepted and then Field.Value.Kind = F.Text_Chunk
           and then Report.Text_Bytes = W.Word (Value'Length), "identity full text length");
         for I in 0 .. Value'Length - 1 loop
            Check (Text (Text'First + I) = Character'Pos (Value (Value'First + I)), "identity text byte");
         end loop;
      end Is_Text;
      Long_ID : constant String := "VENDOR\BUS_CHANNEL_0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_COMPLETE_IDENTIFIER_FINAL_Z";
   begin
      Read ("DEV0", F.Hardware_ID); Is_ID ("INTC1057");
      Read ("INT0", F.Hardware_ID); Is_ID ("PNP0C50");
      Read ("DEV0", F.Compatible_ID, 0); Is_ID ("PNP0C50");
      Check (Report.More_CIDs, "CID list continues");
      Read ("DEV0", F.Compatible_ID, 1); Is_ID ("ELAN06FA");
      Read ("DEV0", F.Compatible_ID, 2); Is_ID ("CBT0001");
      Read ("DEV0", F.Compatible_ID, 3); Is_Text ("PCI\VEN_1234&DEV_ABCD");
      Check (not Report.More_CIDs and then Field.Value.Ordinal = 3, "final full bus CID ordinal");
      Read ("DEV0", F.Compatible_ID, 4); Check (Report.Status = Identity.End_Of_List, "CID end");
      Read ("DEV0", F.Compatible_ID, 5); Check (Report.Status = Identity.Bad_Position, "CID beyond end");
      Read ("ECID", F.Compatible_ID); Check (Report.Status = Identity.End_Of_List, "empty static CID package");
      Read ("ACID", F.Compatible_ID, 1); Is_ID ("PNP0C50");
      Read ("MCID", F.Hardware_ID);
      Check (Field.Accepted and then Field.Value.Status = W.Method_Valued, "method HID retained as method");
      Read ("MCID", F.Compatible_ID); Is_ID ("PNP0C50");
      Read ("LCID", F.Compatible_ID); Is_Text (Long_ID);
      Read ("LCID", F.Compatible_ID, Size => Long_ID'Length - 1);
      Check (Report.Status = Identity.Output_Required and then Report.Required_Text_Bytes = W.Word (Long_ID'Length),
        "CID never silently chunked");
      Read ("LCID", F.Compatible_ID, Offset => 1);
      Check (Report.Status = Identity.Bad_Position, "CID byte offset refused");
      Read ("DEV0", F.Unique_ID); Is_Text ("unit-a");
      for I in 0 .. 5 loop
         Read ("DEV0", F.Unique_ID, Offset => W.Word (I), Size => 1);
         Check (Field.Accepted and then Field.Value.Logical_Offset = W.Word (I)
           and then Field.Value.Total_Length = 6 and then F.Has_More (Field.Value, Wire_Node_Limit, Pool + 1) = (I < 5),
           "UID chunk advances");
         Check (Text (Text'First) = Character'Pos (String'("unit-a") (I + 1)), "UID chunk byte");
      end loop;
      Read ("DEV0", F.Unique_ID, Offset => 6); Check (Report.Status = Identity.Bad_Position, "UID end offset refused");
      Read ("DEV0", F.Unique_ID, Size => 0);
      Check (Report.Status = Identity.Output_Required and then Report.Required_Text_Bytes = 1, "UID requires forward progress");
      Read ("INT0", F.Unique_ID); Check (Field.Accepted and then Field.Value.Kind = F.Integer_Value
        and then Field.Value.Number = 7, "integer UID");
      Read ("INT0", F.Unique_ID, Offset => 1); Check (Report.Status = Identity.Bad_Position, "integer UID has no offset");
      Read ("DEV0", F.Address); Check (Field.Accepted and then Field.Value.Number = 16#10000#, "address word");
      Read ("ZSTA", F.Device_Status); Check (Field.Accepted and then Field.Value.Number = 0
        and then not Field.Value.Defaulted, "zero STA preserved");
      Read ("ST32", F.Device_Status); Check (Field.Accepted and then Field.Value.Number = W.Word (Raw_STA_With_Reserved_Bit), "STA reserved bits preserved");
      Read ("NONE", F.Device_Status); Check (Field.Accepted and then Field.Value.Defaulted
        and then Field.Value.Status = W.Absent and then Field.Value.Number = W.Default_STA, "missing STA explicit default");
      for Member in Identity.Member_Name loop
         Read ("METH", Member); Check (Field.Accepted and then Field.Value.Status = W.Method_Valued, "method metadata never evaluated");
         Read ("NONE", Member);
         Check (Field.Accepted and then
           (if Member = F.Address then Field.Value.Status = W.Present and then Field.Value.Number = 0
            else Field.Value.Status = W.Absent), "direct metadata reflects actual fixture");
         Read ("DEV0", Member, Ordinal => F.U32'Last);
         Check (Report.Status = Identity.Bad_Position, "extreme ordinal refused");
      end loop;
      Read ("FILD", F.Hardware_ID); Check (Field.Accepted and then Field.Value.Status = W.Unsupported, "Field IO never evaluated");
      Read ("UNSP", F.Unique_ID); Check (Field.Accepted and then Field.Value.Status = W.Unsupported, "unsupported static UID");
      Identity.Read (Service, Find (Other, "DEV0"), F.Hardware_ID, 0, 0, Pool,
        Scratch, Atoms, Text, Field, Report);
      Check (Report.Status = Identity.Observation_Failed and then not Field.Accepted, "foreign identity descriptor refused");
      Identity.Read (Service, Find (Service, "DEV0"), F.Compatible_ID, 0, 0, Pool,
        Scratch, Atoms (1 .. 0), Text, Field, Report);
      Check (Report.Status = Identity.Scratch_Required and then not Field.Accepted, "CID atom scratch pressure explicit");
      Identity.Read (Service, Find (Service, "DEV0"), F.Hardware_ID, 0, 0, Pool,
        Scratch (1 .. 0), Atoms, Text, Field, Report);
      Check (Report.Status = Identity.Scratch_Required and then not Field.Accepted, "identity byte scratch pressure explicit");
      Read ("DEV0", F.Unique_ID, Offset => W.Word'Last);
      Check (Report.Status = Identity.Bad_Position, "full-word UID offset refused before narrowing");
      Identity.Read (Service, Find (Service, "DEV0"), F.Unique_ID, 0, 0, F.U32'Last,
        Scratch, Atoms, Text, Field, Report);
      Check (Report.Status = Identity.Observation_Failed and then not Field.Accepted
        and then (for all B of Text => B = 0), "overflowing payload span refused before copying");
   end Identity_Checks;
   procedure Observer_Checks is
      Scope : constant Q.Scope_State := (True, W.Instance_ID'Last, W.Generation'Last);
      Item : W.Request := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value, others => <>),
                           (Present => False));
      Frame : WP.Byte_Array (101 .. 101 + 4_096 - 1);
      Response : WP.Byte_Array (101 .. 101 + 4_096 - 1);
      Nodes : Q.Node_Array (Natural'Last - 7 .. Natural'Last);
      Scratch : Firmware_Tables.Bytes (Natural'Last - 511 .. Natural'Last);
      Atoms : P.Atoms (Natural'Last - 7 .. Natural'Last);
      Payload : WP.Byte_Array (Natural'Last - 63 .. Natural'Last);
      Filter : WP.Byte_Array (Natural'Last - 511 .. Natural'Last);
      Result : H.Report;
      Last_Size : Positive := Response'Length;
      procedure Build (Text : String := "") is
         Bytes : WP.Byte_Array (101 .. 101 + Text'Length - 1);
         Accepted : Boolean;
         Written : W.Word;
      begin
         for I in Text'Range loop Bytes (Bytes'First + I - Text'First) := Character'Pos (Text (I)); end loop;
         WP.Rebase_Text (Item, WP.Frame_Byte_Count, Wire_Node_Limit, Accepted);
         Check (Accepted, "observer request rebased");
         WP.Encode_Request (Item, Bytes, Frame, Wire_Node_Limit, Written, Accepted);
         Check (Accepted, "observer request encoded");
      end Build;
      procedure Invoke
        (Size : Positive := Response'Length; Visits : Positive := C.Max_Namespace_Nodes;
         Stamp : ACPI_Protocol.Role_Tag := ACPI_Protocol.Observer_Tag;
         Owner : Q.Scope_State := Scope; Filter_Size : Positive := Filter'Length) is
      begin
         Response := [others => 16#FF#]; Last_Size := Size;
         H.Handle (Service, Stamp, Owner, Frame, Visits, Nodes, Scratch, Atoms, Payload,
           Filter (Filter'First .. Filter'First + (Filter_Size - 1)),
           Response (Response'First .. Response'First + (Size - 1)), Result);
         Check (Result.Scan.Visited_Nodes <= Visits, "observer node work bound");
         Check (C.Retained_Results (Service) = 0, "observer returns no retained roots");
         if Result.Status /= H.Reply_Ready then
            Check (Result.Written = 0 and then
              (for all B of Response (Response'First .. Response'First + (Size - 1)) => B = 0),
              "unframed failure clears private response");
         end if;
      end Invoke;
      function Decode_Reply return R.Header is
         Decoded : constant R.Header_Result := R.Read_Envelope
           (Response (Response'First .. Response'First + (Last_Size - 1)), Wire_Node_Limit);
      begin
         Check (Result.Status = H.Reply_Ready and then Decoded.Accepted, "client accepts framed reply");
         Check (R.Matches_Request (Response (Response'First .. Response'First + (Last_Size - 1)), Frame,
           Wire_Node_Limit), "client correlates complete request filter");
         Check (R.Extent (Decoded.Value) = Result.Written, "exact published frame extent");
         return Decoded.Value;
      end Decode_Reply;
   begin
      Build;
      Invoke (Stamp => 0);
      Check (Result.Status = H.Denied and then not Result.Backend_Invoked, "no observer stamp means no parse/scan");
      Frame := [others => 0]; Invoke (Stamp => ACPI_Protocol.Role_Tag'Last);
      Check (Result.Status = H.Denied and then not Result.Backend_Invoked, "role denial precedes malformed-frame processing");
      Build;
      Invoke (Size => R.Header_Byte_Count - 1);
      Check (Result.Status = H.Reply_Space_Required and then not Result.Backend_Invoked
        and then Result.Required_Bytes = R.Header_Byte_Count, "header capacity refusal before namespace scan");
      Invoke (Size => R.Header_Byte_Count);
      declare Header : constant R.Header := Decode_Reply; begin
         Check (Header.Status = W.Insufficient_Space and then Header.Required_Bytes = R.Header_Byte_Count + R.Node_Byte_Count
           and then Header.Record_Count = 0 and then not Header.Context.Continuation.Present,
           "small result buffer receives typed size hint with no partial page");
      end;
      -- Small canonical replies page naturally through actual namespace matches.
      declare
         Seen : array (W.Node_Index range 1 .. Wire_Node_Limit) of Boolean := [others => False];
         Total : Natural := 0;
         Finished : Boolean := False;
      begin
         for Step in 1 .. C.Max_Namespace_Nodes + 1 loop
            Build; Invoke (Size => R.Header_Byte_Count + R.Node_Byte_Count, Visits => 1);
            declare Header : constant R.Header := Decode_Reply; begin
               Check (Header.Status in W.OK | W.Not_Found, "small observer page status");
               Check (Header.Record_Count <= 1, "reply capacity limits row count");
               for I in 0 .. Natural (Header.Record_Count) - 1 loop
                  declare Node : constant R.Node_Result := R.Node_At
                    (Response (Response'First .. Response'First + (Last_Size - 1)), R.U32 (I), Wire_Node_Limit); begin
                     Check (Node.Accepted and then not Seen (Node.Value.Index), "client receives distinct live-bounded ref");
                     Seen (Node.Value.Index) := True; Total := Total + 1;
                  end;
               end loop;
               if not Header.Context.Continuation.Present then Finished := True; exit; end if;
               Item := Header.Context;
            end;
         end loop;
         Check (Finished and then Total = 5, "all static matches through small framed pages");
      end;
      Item := ((Kind => W.Find_By_ID, ID => W.From_Text ("NONE0000").Value, others => <>), (Present => False));
      Build; Invoke (Size => R.Header_Byte_Count);
      declare Header : constant R.Header := Decode_Reply; begin
         Check (Header.Status = W.Not_Found and then Header.Record_Count = 0, "terminal empty result fits header only");
      end;
      Item.Key.ID := W.From_Text ("PNP0C50").Value; Build;
      Invoke (Owner => (False, Scope.Instance, Scope.Revision));
      declare Header : constant R.Header := Decode_Reply; begin Check (Header.Status = W.Namespace_Not_Loaded, "readiness error framed"); end;
      Item.Key.Instance := 1; Item.Key.Revision := Scope.Revision; Build; Invoke;
      declare Header : constant R.Header := Decode_Reply; begin Check (Header.Status = W.Stale_Generation, "stale scope error framed"); end;
      Item.Key.Instance := Scope.Instance;
      Item.Continuation := (True, (Item.Key, W.Node_Scan, Wire_Node_Limit, 0, 0));
      Build; Invoke;
      declare Header : constant R.Header := Decode_Reply; begin Check (Header.Status = W.Bad_Cursor, "semantic bad cursor framed"); end;
      -- Complete external filters take space independently of typed rows. The
      -- first omitted match, rather than the scan's later position, is resumed.
      Item := ((Kind => W.Find_By_ID, ID => W.External_ID (WP.Frame_Byte_Count, 7), others => <>), (Present => False));
      declare Total : Natural := 0; Finished : Boolean := False;
         Seen : array (W.Node_Index range 1 .. Wire_Node_Limit) of Boolean := [others => False];
      begin
         for Step in 1 .. C.Max_Namespace_Nodes + 1 loop
            Build ("PNP0C50"); Invoke (Size => R.Header_Byte_Count + 7 + 2 * R.Node_Byte_Count);
            declare Header : constant R.Header := Decode_Reply; begin
               Check (Header.Status = W.OK and then Header.Record_Count in 1 .. 2, "external filter page fits all published rows");
               for I in 0 .. Natural (Header.Record_Count) - 1 loop
                  declare Node : constant R.Node_Result := R.Node_At
                    (Response (Response'First .. Response'First + (Last_Size - 1)), R.U32 (I), Wire_Node_Limit); begin
                     Check (Node.Accepted and then not Seen (Node.Value.Index), "no lost or duplicated trimmed result");
                     Seen (Node.Value.Index) := True; Total := Total + 1;
                  end;
               end loop;
               if not Header.Context.Continuation.Present then Finished := True; exit; end if;
               Check (Header.Record_Count = 1 and then Result.Scan.Copied_Nodes = 2 and then Result.Returned_Nodes = 1
                 and then Header.Context.Continuation.Value.Next_Node = Nodes (Nodes'First + 1).Index,
                 "continuation rewinds to first omitted match without rescan");
               Item := Header.Context;
            end;
         end loop;
         Check (Finished and then Total = 5, "complete external-filter result set");
      end;
      Item := ((Kind => W.Find_By_ID, ID => W.External_ID (WP.Frame_Byte_Count, 7), others => <>), (Present => False));
      Build ("PNP0C50"); Invoke (Size => R.Header_Byte_Count + 7 + 2 * R.Node_Byte_Count, Filter_Size => 7);
      Check (Result.Status = H.Scratch_Required and then Result.Required_Filter_Bytes = 14
        and then Result.Reason = H.Internal_Scratch_Limit, "private filter scratch is not client loan shortage");
      Build ("PNP0C50"); Invoke (Size => R.Header_Byte_Count + 7 + R.Node_Byte_Count);
      declare Header : constant R.Header := Decode_Reply; begin
         Check (Header.Status = W.Insufficient_Space and then Header.Required_Bytes = R.Header_Byte_Count + 14 + R.Node_Byte_Count,
           "insufficient continuation room yields exact one-row continuation size");
      end;
      declare
         Long_ID : constant String :=
           "VENDOR\BUS_CHANNEL_0123456789_ABCDEFGHIJKLMNOPQRSTUVWXYZ_COMPLETE_IDENTIFIER_FINAL_Z";
         Expected_Node : constant W.Node_Index := W.Node_Index (C.Values.Position (Find (Service, "LCID")));
         Total : Natural := 0;
         Finished : Boolean := False;
      begin
         Item := ((Kind => W.Find_By_ID, ID => W.External_ID (WP.Frame_Byte_Count, Long_ID'Length), others => <>),
                  (Present => False));
         for Step in 1 .. C.Max_Namespace_Nodes + 1 loop
            Build (Long_ID);
            Invoke (Size => R.Header_Byte_Count + 2 * Long_ID'Length + R.Node_Byte_Count, Visits => 1);
            declare Header : constant R.Header := Decode_Reply; begin
               Check (Header.Status in W.OK | W.Not_Found and then Header.Record_Count <= 1,
                 "long-ID work page has bounded records");
               if Header.Record_Count = 1 then
                  declare Node : constant R.Node_Result := R.Node_At
                    (Response (Response'First .. Response'First + (Last_Size - 1)), 0, Wire_Node_Limit); begin
                     Check (Node.Accepted and then Node.Value.Index = Expected_Node, "complete long-ID result through reply bytes");
                     Total := Total + 1;
                  end;
               end if;
               if not Header.Context.Continuation.Present then Finished := True; exit; end if;
               Item := Header.Context;
            end;
         end loop;
         Check (Finished and then Total = 1, "long filter survives all empty and matching work pages");
      end;
      -- Reader resource pressure is a typed backend refusal, not fake absence
      -- or a request for the client to enlarge its reply loan.
      Item := ((Kind => W.Find_By_ID, ID => W.From_Text ("PNP0C50").Value, others => <>), (Present => False));
      Build;
      declare Small : Firmware_Tables.Bytes (1 .. 1); begin
         Last_Size := Response'Length;
         H.Handle (Service, ACPI_Protocol.Observer_Tag, Scope, Frame, C.Max_Namespace_Nodes,
           Nodes, Small, Atoms, Payload, Filter, Response, Result);
         declare Header : constant R.Header := Decode_Reply; begin
            Check (Header.Status = W.Refused and then Result.Scan.Status = Q.Scratch_Required
              and then Result.Reason = H.Internal_Scratch_Limit and then Header.Required_Bytes = 0,
              "backend scratch pressure does not become NotFound/default/loan-size error");
         end;
      end;
      Item.Key := (Kind => W.Describe, Instance => Scope.Instance, Revision => Scope.Revision,
        Node => W.Node_Index (C.Values.Position (Find (Service, "DEV0"))), others => <>);
      Build; Invoke;
      declare Header : constant R.Header := Decode_Reply; begin
         Check (Header.Status = W.Refused and then not Result.Backend_Invoked, "other operations not misrouted to search");
      end;
      Frame := [others => 0]; Invoke;
      Check (Result.Status = H.Invalid_Request and then not Result.Backend_Invoked, "malformed observer request never scans");
      Counter_Zero;
   end Observer_Checks;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "fixture path");
   declare
      Input : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      Ada.Streams.Stream_IO.Open (Input, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (Input, Stream, Last); Ada.Streams.Stream_IO.Close (Input);
      declare Bytes : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Bytes'Range loop Bytes (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         Prepare (Service, Bytes); Prepare (Other, Bytes);
      end;
   end;
   for Repetition in 1 .. 8 loop
      Query ("DEV0", P.Hardware_ID, P.Static_Value);
      Check (Result.Copied_Atoms = 1 and then Result.Copied_Bytes = 8 and then
        Items (Items'First).Kind = P.Text_Atom, "static HID text");
      Check (Text_Matches ("INTC1057"), "HID bytes match independent fixture");
      Query ("DEV0", P.Compatible_ID, P.Static_Value);
      Check (Result.Copied_Atoms = 4 and then Result.Copied_Bytes = 15 + Bus_CID'Length and then
        Items (Items'First).Number = 16#500C_D041# and then
        Items (Items'First + 1).Kind = P.Text_Atom, "flat mixed CID package");
      Check (Text_Matches ("ELAN06FACBT0001" & Bus_CID) and then
        Items (Items'First + 1).Offset = 0 and then Items (Items'First + 1).Length = 8 and then
        Items (Items'First + 2).Offset = 8 and then Items (Items'First + 2).Length = 7 and then
        Items (Items'First + 3).Kind = P.Text_Atom and then Items (Items'First + 3).Offset = 15
        and then Items (Items'First + 3).Length = Bus_CID'Length,
        "CID strings concatenate with exact zero-based offsets");
      Query ("DEV0", P.Unique_ID, P.Static_Value);
      Check (Result.Copied_Bytes = 6, "static text UID");
      Check (Text_Matches ("unit-a"), "UID bytes match independent fixture");
      Query ("DEV0", P.Address, P.Static_Value);
      Check (Items (Items'First).Number = 16#10000# and then Result.Copied_Bytes = 0, "static ADR integer");
      Query ("DEV0", P.Device_Status, P.Static_Value);
      Check (Items (Items'First).Kind = P.Integer_Atom and then Items (Items'First).Number = 16#0F#
        and then Result.Copied_Atoms = 1 and then Result.Copied_Bytes = 0, "direct static STA");
      Query ("ZSTA", P.Device_Status, P.Static_Value);
      Check (Items (Items'First).Kind = P.Integer_Atom and then Items (Items'First).Number = 0,
        "zero STA is observed, not missing/default");
      Query ("ST32", P.Device_Status, P.Static_Value);
      Check (Items (Items'First).Number = Raw_STA_With_Reserved_Bit, "raw STA bits retained for policy");
      Query ("ALIA", P.Device_Status, P.Static_Value);
      Check (Items (Items'First).Number = 9, "static integer alias STA");
      Query ("NONE", P.Device_Status, P.Missing);
      Query ("STTX", P.Device_Status, P.Unsupported);
      Query ("STBF", P.Device_Status, P.Unsupported);
      Query ("STPK", P.Device_Status, P.Unsupported);
      Query ("FILD", P.Device_Status, P.Unsupported);
      declare
         Node : C.Namespace.Node_ID;
         Child : C.Node_Descriptor;
         Status : C.Values.Access_Status;
      begin
         C.Find_Child (Service, C.Values.Position (Find (Service, "PARN")), "CHLD", Node, Status);
         Check (Status = C.Values.Available, "nested child lookup");
         C.Describe_Node (Service, Node, Child, Status);
         Check (Status = C.Values.Available, "nested child descriptor");
         P.Read (Service, Child, P.Device_Status, Data, Items, Result);
         Check (Result.Status = P.Missing and then Result.Copied_Atoms = 0
           and then Result.Copied_Bytes = 0, "missing child STA does not inherit parent STA");
         Check ((for all Byte of Data => Byte = 0) and then C.Retained_Results (Service) = 0,
           "missing nested STA clears output without leaking roots");
         declare Field : F.Field_Result; Observation : P.Report; begin
            S.Read (Service, Child, Field, Observation);
            Check (Field.Accepted and then Observation.Status = P.Missing and then
              Field.Value.Defaulted and then Field.Value.Number = ACPI_Passive_Query.Default_STA,
              "nested child default does not inherit observed parent zero");
         end;
      end;
      Query ("DEV0", P.Current_Resources, P.Static_Value);
      Check (Result.Copied_Atoms = 0 and then Result.Copied_Bytes = 14 and then
        Data (Data'First) = 16#86# and then Data (Data'First + 12) = 16#79#,
        "exact resource buffer prefix");
      declare Expected : constant Firmware_Tables.Bytes :=
        [16#86#, 9, 0, 1, 0, 0, 0, 16#FD#, 0, 16#10#, 0, 0, 16#79#, 0];
      begin
         Check ((for all I in Expected'Range => Data (Data'First + (I - 1)) = Expected (I)),
           "every resource byte matches independent fixture");
      end;
      for Member in P.Member_Name loop Query ("METH", Member, P.Method_Valued); end loop;
      Query ("FILD", P.Hardware_ID, P.Unsupported);
      Query ("NONE", P.Hardware_ID, P.Missing);
      Query ("UNSP", P.Unique_ID, P.Unsupported);
      Query ("INT0", P.Hardware_ID, P.Static_Value);
      Check (Items (Items'First).Number = 16#500C_D041#, "integer HID unchanged");
      Query ("INT0", P.Compatible_ID, P.Static_Value);
      Check (Result.Copied_Bytes = 7, "scalar string CID");
      Check (Text_Matches ("PNP0C50"), "scalar CID bytes match fixture");
      Query ("INT0", P.Unique_ID, P.Static_Value);
      Check (Items (Items'First).Number = 7, "integer UID unchanged");
      Counter_Zero;
   end loop;
   P.Read (Service, Find (Other, "DEV0"), P.Hardware_ID, Data, Items, Result);
   Check (Result.Status = P.Invalid_Device, "foreign namespace refused");
   P.Read (Service, Find (Other, "DEV0"), P.Device_Status, Data, Items, Result);
   Check (Result.Status = P.Invalid_Device, "foreign STA namespace refused");
   declare Field : F.Field_Result; Observation : P.Report; begin
      S.Read (Service, Find (Other, "DEV0"), Field, Observation);
      Check (not Field.Accepted and then Observation.Status = P.Invalid_Device,
        "foreign descriptor cannot fabricate a default status");
      S.Read (Service, Find (Service, "HITS"), Field, Observation);
      Check (not Field.Accepted and then Observation.Status = P.Invalid_Device,
        "non-device cannot fabricate a default status");
   end;
   P.Read (Service, Find (Service, "HITS"), P.Hardware_ID, Data, Items, Result);
   Check (Result.Status = P.Invalid_Device, "non-device refused");
   declare
      Empty : P.Atoms (1 .. 0);
      No_Bytes : Firmware_Tables.Bytes (1 .. 0);
      One : P.Atoms (0 .. 0);
      Small : Firmware_Tables.Bytes (1 .. 1);
   begin
      P.Read (Service, Find (Service, "DEV0"), P.Address, Data, Empty, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Atoms = 1,
        "integer needs atom slot");
      P.Read (Service, Find (Service, "DEV0"), P.Device_Status, Data, Empty, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Atoms = 1
        and then Result.Copied_Atoms = 0 and then (for all Byte of Data => Byte = 0),
        "STA needs an atom slot and clears refusal output");
      P.Read (Service, Find (Service, "DEV0"), P.Device_Status, No_Bytes, One, Result);
      Check (Result.Status = P.Static_Value and then One (0).Kind = P.Integer_Atom
        and then One (0).Number = 16#0F# and then Result.Copied_Bytes = 0,
        "integer STA needs no text bytes");
      P.Read (Service, Find (Service, "DEV0"), P.Compatible_ID, Data, One, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Atoms = 4
        and then not Result.Size_Known, "CID slots bound traversal without partial size claim");
      P.Read (Service, Find (Service, "DEV0"), P.Hardware_ID, Small, Items, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Bytes = 8
        and then Result.Size_Known and then Small (1) = 0, "HID exact byte capacity");
      P.Read (Service, Find (Service, "DEV0"), P.Compatible_ID, Small, Items, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Bytes = 15 + Bus_CID'Length
        and then Result.Size_Known, "CID sizing releases children and parent");
      P.Read (Service, Find (Service, "DEV0"), P.Current_Resources, Small, Items, Result);
      Check (Result.Status = P.Capacity_Required and then Result.Required_Bytes = 14,
        "CRS exact byte capacity");
   end;
   declare
      Held : C.Values.Result;
      Handle : C.Values.Value_Handle;
      Status : C.Values.Access_Status;
      Found : C.Namespace.Node_ID;
   begin
      C.Find_Child (Service, C.Values.Position (Find (Service, "DEV0")), "_HID", Found, Status);
      Check (Status = C.Values.Available, "held result lookup");
      C.Observe_Named_Value (Service, Found, Held, Status);
      Check (Status = C.Values.Available and then Held.Status = AML_Execute.Object_Returned,
        "owner retains unrelated text result");
      Handle := Held.Handle;
      P.Read (Service, Find (Service, "DEV0"), P.Hardware_ID, Data, Items, Result);
      Check (Result.Status = P.Static_Value and then C.Retained_Results (Service) = 1,
        "successful query preserves unrelated owner's root");
      P.Read (Service, Find (Service, "DEV0"), P.Compatible_ID, Data, Items, Result);
      Check (Result.Status = P.Read_Failed and then Result.Access_Result = C.Values.Root_Limit
        and then C.Retained_Results (Service) = 1 and then
        (for all Byte of Data => Byte = 0), "package retention pressure clears output and preserves owner root");
      C.Release_Result (Service, Handle, Status);
      Check (Status = C.Values.Available and then Handle = C.Values.No_Value and then
        C.Retained_Results (Service) = 0, "owner can still release its retained result");
   end;
   Search_Checks;
   Observer_Checks;
   Identity_Checks;
   Path_Checks;
   Describe_Checks;
   Resource_Projection_Checks;
   Resources_Checks;
   Loan_Dispatch_Checks;
   Native_Grant_Checks;
   for Mode in 1 .. 3 loop
      declare
         Value : C.Values.Result;
         Status : C.Values.Access_Status;
      begin
         C.Invoke_Retained (Service, C.Values.Position (Find (Service, "SETM")),
           [0 => AML_Decode.Integer_Value (Mode), others => 0], 1, 1000, Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.No_Return,
           "explicit test mutation outside passive reader");
      end;
      Query ("DEV0", (case Mode is when 1 => P.Compatible_ID, when 2 => P.Unique_ID,
        when others => P.Address), (if Mode = 1 then P.Unsupported else P.Static_Value));
      if Mode = 2 then
         Check (Items (Items'First).Kind = P.Text_Atom, "Store to named text preserves target type");
      elsif Mode = 3 then
         Check (Items (Items'First).Kind = P.Integer_Atom, "Store to named integer preserves target type");
      end if;
      Check (C.Retained_Results (Service) = 0, "failed type releases whole retained tree");
      Counter_Zero;
   end loop;
   Ada.Text_IO.Put_Line ("Passive checks" & Checks'Image);
end Passive_Member_Tests;
