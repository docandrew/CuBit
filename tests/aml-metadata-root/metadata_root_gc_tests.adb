with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Root_Test_Support; use Root_Test_Support;
procedure Metadata_Root_GC_Tests is
   use type AML_Decode.Bytes;
   package E renames AML_Execute;
   package N is new AML_Namespace (Capacity => 48, Perform_Delay => AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type AML_Decode.Integer_Value;
   use type V.Access_Status;
   use type V.Collection_Count;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type N.Notification_Class;
   use type N.Notification_Target_Kind;
   use type E.Execution_Status;
   A : V.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Loaded : N.Load_Status;
   Status : V.Access_Status;
   Report : N.Initialization_Report;
   Result : V.Result;
   Stats_Before, Stats_After : V.Collection_Statistics;
   Event : N.Notification_Observation;
   Present : Boolean;
   Checks : Natural := 0;
   Root_Name : constant Bytes := [16#5C#, 0];
   Root_Ref : constant Bytes := Bytes'[16#71#] & Root_Name;
   Pressure_Count : constant := 20;
   Pressure_Bytes : constant := AML_Decode.Max_Buffer_Length;
   -- Each allocation enters the collecting hook. Use a supported literal size;
   -- counter checks demonstrate actual collection and reclamation.
   function Pressure return Bytes is
      One : constant Bytes := Bytes'[16#70#] & Block ([16#11#], [16#0B#, Byte (Pressure_Bytes mod 256), Byte (Pressure_Bytes / 256)]) & [16#60#];
      Code : Bytes (1 .. Pressure_Count * One'Length + 2);
   begin
      for I in 0 .. Pressure_Count - 1 loop
         Code (I * One'Length + 1 .. (I + 1) * One'Length) := One;
      end loop;
      Code (Code'Last - 1 .. Code'Last) := [16#A4#, 1];
      return Method ("GC00", Code);
   end Pressure;
   -- Root -> outer Package -> inner Package -> live DEV0 metadata and Buffer.
   -- Package name members deliberately remain live; this is not a captured leaf.
   Nested_Package : constant Bytes := Block ([16#12#], Bytes'[1]
     & Block ([16#12#], Bytes'[2] & Enc ("DEV0")
       & Block ([16#11#], [16#0A#, 2, 16#D3#, 16#7E#])));
   Inner_Read : constant Bytes := Bytes'[16#70#, 16#83#] & Root_Ref & [16#60#]
     & [16#70#, 16#83#, 16#88#, 16#60#, 0, 0, 16#61#];
   Nested_Buffer_Read : constant Bytes := Inner_Read
     & [16#70#, 16#83#, 16#88#, 16#61#, 1, 0, 16#62#];
   Code : constant Bytes :=
     Bytes'[16#08#] & Enc ("CHLD") & [16#0A#, 7]
     & [16#08#] & Enc ("VAR0") & [0]
     & Block ([16#5B#, 16#82#], Enc ("DEV0"))
     -- Copy through explicit RefOf(root) argument; physical scope is untouched.
     & Block ([16#14#], Enc ("CPY0") & [2, 16#9D#, 16#68#, 16#69#, 16#A4#, 1])
     & Method ("BSET", Enc ("CPY0") & Block ([16#11#], [16#0A#, 2, 16#A5#, 16#5A#])
         & Root_Ref & [16#A4#, 1])
     & Method ("ISET", Enc ("CPY0") & [16#0A#, 42] & Root_Ref & [16#A4#, 1])
     & Method ("MSET", Enc ("CPY0DEV0") & Root_Ref & [16#A4#, 1])
     & Method ("PSET", Enc ("CPY0") & Nested_Package & Root_Ref & [16#A4#, 1])
     & Method ("PMTY", Inner_Read & [16#A4#, 16#8E#, 16#83#, 16#88#, 16#61#, 0, 0])
     & Method ("PBC0", Nested_Buffer_Read & [16#A4#, 16#83#, 16#88#, 16#62#, 0, 0])
     & Method ("PBC1", Nested_Buffer_Read & [16#A4#, 16#83#, 16#88#, 16#62#, 1, 0])
     & Method ("RTYP", [16#A4#, 16#8E#] & Root_Name)
     & Method ("RVAL", [16#A4#, 16#83#] & Root_Ref)
     & Method ("RBC0", [16#70#, 16#83#] & Root_Ref & [16#60#, 16#A4#, 16#83#, 16#88#, 16#60#, 0, 0])
     & Method ("RBC1", [16#70#, 16#83#] & Root_Ref & [16#60#, 16#A4#, 16#83#, 16#88#, 16#60#, 1, 0])
     & Method ("CHCK", [16#A4#] & Enc ("CHLD"))
     & Method ("NTFY", [16#86#] & Root_Name & [16#0A#, 16#80#, 16#A4#, 1])
     & Method ("CVAR", [16#9D#] & Enc ("DEV0VAR0") & [16#A4#, 16#8E#] & Enc ("VAR0"))
     & Method ("NVAR", [16#86#] & Enc ("VAR0") & [16#0A#, 16#81#, 16#A4#, 1])
     & Pressure;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   function Find (Name : String) return N.Node_ID is
      Located : N.Lookup_Result;
   begin
      V.Find_Object (A, N.Root, Name, Located, Status);
      Check (Status = V.Available and then Located.Status = N.Found
        and then Located.Node /= N.Root, "find child " & Name);
      return Located.Node;
   end Find;
   procedure Invoke (Name : String) is
      Node : constant N.Node_ID := Find (Name);
   begin
      V.Invoke (A, Input, Node, [others => (V.Immediate_Argument, 0)], 0, 20_000, Result, Status);
      Check (Status = V.Available, "invocation access " & Name);
      Check (V.Retained_Count (A) = 0, "scalar invocation releases all frames and pins");
   end Invoke;
   procedure Call (Name : String; Expected : Integer_Value) is
   begin
      Invoke (Name);
      Check (Result.Status = E.Returned and then Result.Number = Expected, "result " & Name);
   end Call;
   procedure Load (Width : Integer_Width) is
   begin
      V.Load (A, Code, Width, Loaded, Status);
      Check (Status = V.Available and then Loaded = N.Loaded, "load root fixture");
      V.Seal (A, Report, Status); Check (Status = V.Available, "seal root fixture");
   end Load;
   Variable_Node : N.Node_ID;
begin
   V.Initialize (A, Status); Check (Status = V.Available, "initialize collecting owner");
   for Width in Integer_Width loop
      Load (Width);
      Call ("RTYP", 6); Call ("CHCK", 7);
      V.Configure_Notifications (A, True, Status); Check (Status = V.Available, "enable observer");
      Call ("NTFY", 1);
      Check (V.Pending_Notifications (A) = 1, "initial semantic root notification admitted");
      Call ("BSET", 1); Call ("RTYP", 3); Call ("RBC0", 16#A5#); Call ("RBC1", 16#5A#);
      Stats_Before := V.Reclamation_Metrics (A);
      Call ("GC00", 1);
      Stats_After := V.Reclamation_Metrics (A);
      Check (Stats_After.Completed > Stats_Before.Completed
        and then Stats_After.Freed_Objects > Stats_Before.Freed_Objects,
        "later invocation actually collects and reclaims");
      -- No return handle or earlier invocation frame retains the root buffer.
      Call ("RBC0", 16#A5#); Call ("RBC1", 16#5A#); Call ("CHCK", 7);
      -- CPY0 captures DEV0 directly into the semantic root. The scalar return
      -- leaves no result pin or prior invocation frame to retain that snapshot.
      Call ("MSET", 1); Call ("RTYP", 6);
      Stats_Before := V.Reclamation_Metrics (A);
      Call ("GC00", 1);
      Stats_After := V.Reclamation_Metrics (A);
      Check (Stats_After.Completed > Stats_Before.Completed
        and then Stats_After.Freed_Objects > Stats_Before.Freed_Objects,
        "later invocation collects with captured metadata as root payload");
      Call ("RTYP", 6); Call ("CHCK", 7);
      -- Independently exercise transitive root graph retention. Public package
      -- construction binds DEV0 live, not as a captured metadata snapshot.
      Call ("PSET", 1); Call ("RTYP", 4); Call ("PMTY", 6);
      Call ("PBC0", 16#D3#); Call ("PBC1", 16#7E#);
      Stats_Before := V.Reclamation_Metrics (A);
      Call ("GC00", 1);
      Stats_After := V.Reclamation_Metrics (A);
      Check (Stats_After.Completed > Stats_Before.Completed
        and then Stats_After.Freed_Objects > Stats_Before.Freed_Objects,
        "later invocation collects with only root retaining nested package graph");
      Call ("RTYP", 4); Call ("PMTY", 6);
      Call ("PBC0", 16#D3#); Call ("PBC1", 16#7E#); Call ("CHCK", 7);
      Call ("ISET", 1); Call ("RTYP", 1); Call ("RVAL", 42); Call ("CHCK", 7);
      -- Source-derived policy: pinned exoparg2 validates Node->Type, so physical
      -- Scope identity cannot authorize Notify after semantic integer replacement.
      Invoke ("NTFY");
      Check (Result.Status = E.Unsupported_Value and then V.Pending_Notifications (A) = 1,
        "Notify rejects semantic integer without discarding prior admitted event");
      V.Take_Notification (A, Event, Present, Status);
      Check (Status = V.Available and then Present and then Event.Node = N.Root
        and then Event.Path_Length = 0 and then Event.Target_Kind = N.Device_Notification
        and then Event.Class = N.Device_Notify and then Event.Number = 16#80#,
        "old root event retains original descriptive snapshot");
      -- Local consistency policy; not claimed as an observed ACPICA callback.
      Variable_Node := Find ("VAR0");
      Call ("CVAR", 6); Call ("NVAR", 1);
      V.Take_Notification (A, Event, Present, Status);
      Check (Status = V.Available and then Present and then Event.Node = Variable_Node
        and then Event.Path_Length = 1 and then Event.Path (1) = "VAR0"
        and then Event.Number = 16#81#, "copied metadata Notify identifies destination variable");
      V.Reset (A, Status); Check (Status = V.Available, "reset root payload");
      Check (V.Pending_Notifications (A) = 0 and then V.Retained_Count (A) = 0
        and then V.Observe_Usage (A).Values.Objects = 0, "reset drops root payload and observer state");
   end loop;
   Load (Bits_64); Call ("RTYP", 6); Call ("CHCK", 7);
   Ada.Text_IO.Put_Line ("Metadata root GC checks" & Checks'Image);
end Metadata_Root_GC_Tests;
