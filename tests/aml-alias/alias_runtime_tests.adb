with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
with Alias_Test_Support; use Alias_Test_Support;
procedure Alias_Runtime_Tests is
   use type AML_Decode.Bytes;
   package N is new AML_Namespace (Capacity => 96, Perform_Delay => AML_Delays.Unavailable_Provider);
   package V is new N.Owned.Collecting;
   use type V.Access_Status;
   use type V.Audit_Value;
   use type V.Value_Handle;
   use type V.Value_Kind;
   use type AML_Execute.Named_Metadata_Type;
   use type N.Load_Status;
   use type N.Lookup_Status;
   use type AML_Execute.Execution_Status;
   use type Integer_Value;
   A : V.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Loaded : N.Load_Status;
   Status : V.Access_Status;
   Report : N.Initialization_Report;
   Result : V.Result;
   Item : N.Notification_Observation;
   Found : Boolean;
   Checks : Natural := 0;
   function Name_Value (Name : String; Value : Bytes) return Bytes is
     (Bytes'[16#08#] & Enc (Name) & Value);
   function Index_Of (Name : String; Index : Byte) return Bytes is
     (Bytes'[16#88#] & Enc (Name) & [Index, 0]);
   Code : constant Bytes :=
     Alias_Of (Bytes'[16#5C#, 0], Enc ("RALS")) &
     Method ("DREF", [16#70#, 16#71#, 16#5C#, 0, 16#60#, 16#A4#, 16#8E#, 16#60#]) &
     Method ("DNTF", [16#86#, 16#5C#, 0, 16#0A#, 16#80#, 16#A4#, 1]) &
     -- Local malformed-parser fixtures, not ACPICA conformance cases.
     Method ("NULR", [16#70#, 16#71#, 0, 16#60#, 16#A4#, 1]) &
     Method ("NULN", [16#86#, 0, 16#0A#, 16#80#, 16#A4#, 1]) &
     Method ("ROOT", [16#A4#, 16#8E#] & Enc ("RALS")) &
     Method ("RLOC", [16#70#, 16#71#] & Enc ("RALS") & [16#60#, 16#A4#, 16#8E#, 16#60#]) &
     Method ("MKPK", [16#A4#] & Block ([16#12#], [1] & Enc ("RALS"))) &
     Method ("WROT", [16#70#, 1] & Enc ("RALS") & [16#A4#, 1]) &
     Method ("CROT", [16#9D#, 1] & Enc ("RALS") & [16#A4#, 1]) &
     Method ("RVAL", [16#A4#, 16#83#, 16#71#] & Enc ("RALS")) &
     Method ("RNTF", [16#86#] & Enc ("RALS") & [16#0A#, 16#80#, 16#A4#, 1]) &
     Name_Value ("NUM0", [16#0A#, 7]) & Alias_Of ("NUM0", "AINT") & Alias_Of ("AINT", "A002") &
     Name_Value ("BUF0", Block ([16#11#], [16#0A#, 2, 1, 2])) & Alias_Of ("BUF0", "ABUF") &
     Name_Value ("PKG0", Block ([16#12#], [2, 16#0A#, 11, 16#0A#, 22])) & Alias_Of ("PKG0", "APKG") &
     Name_Value ("VAL0", [16#0A#, 22]) &
     Block ([16#5B#, 16#82#], Enc ("DEV0") & Name_Value ("VAL0", [16#0A#, 11])
       & Method ("MTH0", [16#A4#] & Enc ("VAL0"))) &
     Alias_Of (Bytes'[16#5C#, 16#2E#] & Enc ("DEV0MTH0"), Enc ("AMTH")) & Alias_Of ("DEV0", "ADEV") &
     Method ("CALL", [16#A4#] & Enc ("AMTH")) &
     Method ("CDIR", [16#9D#, 16#0A#, 91] & Enc ("NUM0") & [16#A4#] & Enc ("A002")) &
     Method ("WINT", [16#70#, 16#0A#, 9] & Enc ("AINT") & [16#A4#] & Enc ("NUM0")) &
     Method ("RINT", [16#A4#] & Enc ("A002")) &
     Method ("WBUF", [16#70#, 16#0A#, 16#33#] & Index_Of ("ABUF", 1)
       & [16#A4#, 16#83#] & Index_Of ("BUF0", 1)) &
     Method ("WPKG", [16#70#, 16#0A#, 44] & Index_Of ("APKG", 0)
       & [16#A4#, 16#83#] & Index_Of ("PKG0", 0)) &
     Method ("CINT", [16#9D#] & Block ([16#11#], [16#0A#, 1, 16#AA#]) & Enc ("AINT")
       & [16#A4#, 16#8E#] & Enc ("NUM0")) &
     Method ("RBUF", [16#A4#, 16#83#] & Index_Of ("A002", 0)) &
     Method ("CBUF", [16#9D#, 16#0A#, 55] & Enc ("ABUF") & [16#A4#] & Enc ("BUF0")) &
     Method ("CPKG", [16#9D#] & Block ([16#12#], [1, 16#0A#, 77]) & Enc ("APKG")
       & [16#A4#, 16#83#] & Index_Of ("PKG0", 0)) &
     Method ("NTFY", [16#86#] & Enc ("ADEV") & [16#0A#, 16#80#, 16#A4#, 1]);
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message & " check" & Checks'Image; end if;
   end Check;
   function Find (Name : String; Parent : N.Node_ID := N.Root) return N.Node_ID is
      Located : N.Lookup_Result;
   begin
      V.Find_Object (A, Parent, Name, Located, Status);
      Check (Status = V.Available and then Located.Status = N.Found
        and then Located.Node /= N.Root, "canonical service selector " & Name);
      return Located.Node;
   end Find;
   procedure Call (Name : String; Expected : Integer_Value) is
      Node : constant N.Node_ID := Find (Name);
   begin
      V.Invoke (A, Input, Node, [others => (V.Immediate_Argument, 0)], 0, 1000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Returned, "invoke " & Name);
      Check (Result.Number = Expected, "result " & Name);
      Check (V.Retained_Count (A) = 0, "no retained result leak");
   end Call;
   procedure Reject_Root_Write (Name : String; Expected : AML_Execute.Execution_Status := AML_Execute.Unsupported_Value) is
      Node : constant N.Node_ID := Find (Name);
      Before : constant V.Audit_Value := V.Audit (A);
   begin
      V.Invoke (A, Input, Node, [others => (V.Immediate_Argument, 0)], 0, 1000, Result, Status);
      Check (Status = V.Available and then Result.Status = Expected,
        "defined rejection of root metadata write " & Name);
      Check (V.Audit (A) = Before and then V.Retained_Count (A) = 0
        and then V.Pending_Notifications (A) = 0, "root rejection preserves value state " & Name);
   end Reject_Root_Write;
   procedure Check_Root_Package is
      Node : constant N.Node_ID := Find ("MKPK");
      Parent, Element : V.Value_Handle;
      Description : V.Value_Description;
   begin
      V.Invoke (A, Input, Node, [others => (V.Immediate_Argument, 0)], 0, 1000, Result, Status);
      Check (Status = V.Available and then Result.Status = AML_Execute.Object_Returned,
        "runtime root package returned with owned handle");
      Parent := Result.Handle;
      V.Describe (A, Parent, Description, Status);
      Check (Status = V.Available and then Description.Kind = V.Package_Description
        and then Description.Length = 1, "runtime root package length");
      V.Read_Element (A, Parent, 0, Element, Status);
      Check (Status = V.Available and then Element /= V.No_Value
        and then V.Retained_Count (A) = 2, "independent root package element pin");
      V.Release (A, Parent, Status);
      Check (Status = V.Available and then Parent = V.No_Value and then V.Retained_Count (A) = 1,
        "element survives parent pin release");
      V.Describe (A, Element, Description, Status);
      Check (Status = V.Available and then Description.Kind = V.Metadata_Description
        and then Description.Object_Type = AML_Execute.Device_Metadata,
        "runtime package element is root Device metadata");
      V.Release (A, Element, Status);
      Check (Status = V.Available and then Element = V.No_Value and then V.Retained_Count (A) = 0,
        "root package handles released exactly");
   end Check_Root_Package;
   Original, Alternate, Device_Node, Method_Node, Physical : N.Node_ID;
   Located : N.Lookup_Result;
begin
   V.Initialize (A, Status); Check (Status = V.Available, "initialize");
   V.Load (A, Code, Bits_64, Loaded, Status);
   Check (Status = V.Available and then Loaded = N.Loaded, "load all aliases");
   V.Seal (A, Report, Status); Check (Status = V.Available, "seal package values");
   Original := Find ("NUM0"); Alternate := Find ("AINT");
   V.Find_Child (A, N.Root, "AINT", Physical, Status);
   Check (Status = V.Available and then Physical /= N.Root and then Physical /= Original,
     "physical selector retains alias spelling");
   V.Find_Child (A, N.Root, "RALS", Physical, Status);
   Check (Status = V.Available and then Physical /= N.Root, "physical root alias binding");
   V.Find_Object (A, N.Root, "RALS", Located, Status);
   Check (Status = V.Available and then Located.Status = N.Found and then Located.Node = N.Root,
     "object selector distinguishes found root");
   V.Find_Object (A, N.Root, "MISS", Located, Status);
   Check (Status = V.Available and then Located.Status = N.Not_Found,
     "object selector missing remains not found");
   Check (Original = Alternate and then Find ("A002") = Original, "canonical service alias chain");
   Device_Node := Find ("DEV0"); Method_Node := Find ("MTH0", Device_Node);
   Check (Find ("AMTH") = Method_Node, "method alias original identity");
   Call ("ROOT", 6); Call ("RLOC", 6); Call ("DREF", 6);
   Reject_Root_Write ("WROT", AML_Execute.Unsupported); Check_Root_Package;
   Reject_Root_Write ("NULR"); Reject_Root_Write ("NULN", AML_Execute.Unsupported);
   Call ("AMTH", 11); Call ("CALL", 11);
   Call ("WINT", 9); Call ("RINT", 9);
   Call ("WBUF", 16#33#); Call ("WPKG", 44);
   Call ("CINT", 3); Call ("RBUF", 16#AA#);
   Check (Find ("AINT") = Original and then Find ("A002") = Original, "replacement preserves alias original node");
   Call ("CBUF", 55); Call ("CPKG", 77);
   Call ("CDIR", 91); Call ("RINT", 91);
   V.Configure_Notifications (A, True, Status); Check (Status = V.Available, "enable notifications");
   Call ("NTFY", 1);
   V.Take_Notification (A, Item, Found, Status);
   Check (Status = V.Available and then Found and then Item.Current_Target, "alias notification admitted");
   Check (Item.Node = Device_Node and then Item.Path_Length = 1 and then Item.Path (1) = "DEV0"
     and then Item.Number = 16#80#, "notification original target identity and path");
   Call ("RNTF", 1);
   V.Take_Notification (A, Item, Found, Status);
   Check (Status = V.Available and then Found and then Item.Current_Target,
     "root alias notification admitted");
   Check (Item.Node = N.Root and then Item.Path_Length = 0 and then Item.Number = 16#80#,
     "root notification has canonical root descriptor");
   Call ("DNTF", 1);
   V.Take_Notification (A, Item, Found, Status);
   Check (Status = V.Available and then Found and then Item.Current_Target
     and then Item.Node = N.Root and then Item.Path_Length = 0 and then Item.Number = 16#80#,
     "direct rooted NullName notify path");
   -- Mutate root only after the existing Device/root notification controls.
   -- Explicit Copy has separate authority; ordinary Store rejection stays above.
   Call ("CROT", 1); Call ("ROOT", 1); Call ("RVAL", 1);
   V.Find_Object (A, N.Root, "RALS", Located, Status);
   Check (Status = V.Available and then Located.Status = N.Found and then Located.Node = N.Root,
     "root semantic replacement preserves canonical namespace identity");
   Check (Find ("AINT") = Original and then Find ("DEV0") = Device_Node,
     "root semantic replacement preserves physical child namespace");
   Check (V.Pending_Notifications (A) = 0 and then V.Retained_Count (A) = 0, "clean finish");
   Ada.Text_IO.Put_Line ("Alias runtime checks" & Checks'Image);
end Alias_Runtime_Tests;
