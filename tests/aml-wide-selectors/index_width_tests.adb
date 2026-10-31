pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Index_Width_Oracle;
procedure Index_Width_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 64, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   use type C.Stage_Status;
   use type C.Freeze_Status;
   use type C.Values.Access_Status;
   use type C.Values.Module_Outcome;
   use type C.Values.Value_Kind;
   use type C.Values.Value_Handle;
   use type AML_Execute.Execution_Status;
   use type AML_Identity.Identity;
   use type A.Register_ID;
   use type A.Endpoint_ID;
   use type A.Request;
   subtype Slot is Positive range 1 .. 6;
   type Offsets is array (Slot) of Natural;
   Offset : constant Offsets := [0,8,16,24,16,20];
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [for S in Slot => 16#A000_0000_0000_0000# + A.Register_ID (S)];
   type Memory is array (Natural range 0 .. 31) of AML_Decode.Byte;
   type Log is array (Positive range 1 .. 128) of A.Request;
   type Permissions is array (Slot) of Boolean;
   subtype Denial_Prefix is Integer range -1 .. Log'Length - 1;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Bytes : Memory := [others => 0];
      Requests : Log := [others => (others => <>)];
      Calls : Natural := 0;
      Readable, Writable : Permissions := [others => True];
      Revoke_After : Natural range 0 .. Log'Length := 0;
      Revoke_Slot : Slot := Slot'First;
      Revoke_Write : Boolean := False;
   end record;
   function Width (S : Slot) return T.Access_Width is
     (if S <= 4 then T.Qword_Access else T.Byte_Access);
   function Pending (IO : Context) return Boolean is (IO.Owner = AML_Identity.No_Identity);
   procedure Locate (ID : A.Register_ID; S : out Slot; Found : out Boolean) is
   begin
      S := Slot'First; Found := False;
      for Candidate in Slot loop
         if IDs (Candidate) = ID then S := Candidate; Found := True; return; end if;
      end loop;
   end Locate;
   procedure Describe (IO : Context; Target : A.Target;
     Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean) is
      S : Slot;
      Found : Boolean;
   begin
      Locate (Target.Register, S, Found);
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 32),
        Byte_Offset => AML_Decode.Integer_Value (Offset (S)), Width => Width (S));
      Accepted := Found and then IO.Owner /= AML_Identity.No_Identity
        and then Target.Provider = IO.Owner and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
      S : Slot;
      Found : Boolean;
   begin
      Describe (IO, Target, Geometry, Accepted); Locate (Target.Register, S, Found);
      Accepted := Accepted and then Found and then (if For_Write then IO.Writable (S) else IO.Readable (S));
   end Authorize;
   function Read (Bytes : Memory; S : Slot) return AML_Decode.Integer_Value is
      Number : AML_Decode.Integer_Value := 0;
   begin
      for I in 0 .. T.Octets (Width (S)) - 1 loop
         Number := Number or Interfaces.Shift_Left (AML_Decode.Integer_Value (Bytes (Offset (S) + I)), I * 8);
      end loop;
      return Number;
   end Read;
   procedure Write (Bytes : in out Memory; S : Slot; Number : AML_Decode.Integer_Value) is
   begin
      for I in 0 .. T.Octets (Width (S)) - 1 loop
         Bytes (Offset (S) + I) := AML_Decode.Byte (Interfaces.Shift_Right (Number, I * 8) and 16#FF#);
      end loop;
   end Write;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
   begin
      Reply := (others => <>);
      IO.Calls := IO.Calls + 1; IO.Requests (IO.Calls) := Item;
      Locate (Item.Destination.Register, S, Found);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Found or else not Allowed then return; end if;
      if Item.For_Write then Write (IO.Bytes, S, Item.Value); end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else Read (IO.Bytes, S)), A.No_Operation);
      if IO.Revoke_After > 0 and then IO.Calls = IO.Revoke_After then
         if IO.Revoke_Write then IO.Writable (IO.Revoke_Slot) := False;
         else IO.Readable (IO.Revoke_Slot) := False; end if;
         IO.Revoke_After := 0;
      end if;
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Exercise (Revision : Positive) is
      Raw : constant AML_Decode.Bytes := (if Revision = 1 then Index_Width_Oracle.Table32 else Index_Width_Oracle.Table64);
      Service : aliased C.State (1, Raw'Length, Raw'Length, 1);
      IO : Context;
      Items : F.Binding_Set;
      Status : C.Values.Access_Status;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Region : C.Node_Descriptor;
      Issued : Boolean;
      Initial : Memory;
      procedure Test (Name : AML_Names.Segment; Nested, Writing : Boolean;
        Deny_After : Denial_Prefix := -1; Wide_Data : Boolean := False;
        Tail_Read_Denied : Boolean := False) is
         Expected : Memory := Initial;
         Trace : Log := [others => (others => <>)];
         Count : Natural := 0;
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Result : C.Values.Result;
         Wanted : AML_Execute.Execution_Status;
         Description : C.Values.Value_Description;
         Handle : C.Values.Value_Handle;
         Data : AML_Decode.Bytes (1 .. 10);
         Copied : Natural;
         Expected_Buffer : constant AML_Decode.Bytes :=
           (if Name = "WFUL" then (if Revision = 1 then Index_Width_Oracle.WFUL32 else Index_Width_Oracle.WFUL64)
            elsif Name = "WONE" then (if Revision = 1 then Index_Width_Oracle.WONE32 else Index_Width_Oracle.WONE64)
            else (if Revision = 1 then Index_Width_Oracle.WZER32 else Index_Width_Oracle.WZER64));
         S : Slot;
         Found : Boolean;
         procedure Transfer (S : Slot; Writing : Boolean; Number : AML_Decode.Integer_Value := 0) is
         begin
            Count := Count + 1; Trace (Count) := ((IO.Owner, 7, 1, IDs (S)), Writing, Number);
            if Writing then Write (Expected, S, Number); end if;
         end Transfer;
         procedure Direct_Selector (Number : AML_Decode.Integer_Value) is
            Low : constant AML_Decode.Integer_Value := (Read (Expected, 1) and 7) or Interfaces.Shift_Left (Number, 3);
            High : constant AML_Decode.Integer_Value := Read (Expected, 2) and not 16#FFF#;
         begin
            Transfer (1, False); Transfer (1, True, Low);
            Transfer (2, False); Transfer (2, True, High);
         end Direct_Selector;
         procedure Selector is
         begin
            if Nested then
               for I in 0 .. 9 loop
                  Direct_Selector (AML_Decode.Integer_Value (4 + I));
                  Transfer (5, True, (if I = 0 then 3 else 0));
               end loop;
            else Direct_Selector (16#1234_5678#); end if;
         end Selector;
      begin
         IO.Bytes := Initial; IO.Calls := 0;
         IO.Readable := [others => True]; IO.Writable := [others => True]; IO.Revoke_After := 0;
         if Wide_Data then
            Direct_Selector (4);
            Transfer (3, True, (if Name = "WFUL" then 16#5A#
              elsif Name = "WONE" then (not AML_Decode.Integer_Value'(16#38#)) or 16#10# else 16#10#));
            Transfer (4, False);
            Transfer (4, True, Read (Expected, 4) and not 16#FFFF#);
            Transfer (3, False); Transfer (4, False);
         else
            Selector;
            if Nested then Transfer (6, Writing, (if Writing then 16#5A# else 0));
            else
               Transfer (3, False);
               if Writing then
                  declare Value : constant AML_Decode.Integer_Value := (Read (Expected, 3) and not 16#FF#) or 16#5A#; begin
                     Selector; Transfer (3, True, Value);
                  end;
               end if;
            end if;
         end if;
         if Deny_After >= 0 then
            Check (Deny_After < Count, "denial is before next request");
            Locate (Trace (Deny_After + 1).Destination.Register, S, Found);
            Check (Found, "modeled denial target");
            IO.Revoke_Slot := S; IO.Revoke_Write := Trace (Deny_After + 1).For_Write;
            if Deny_After = 0 then
               if IO.Revoke_Write then IO.Writable (S) := False; else IO.Readable (S) := False; end if;
            else IO.Revoke_After := Deny_After; end if;
            Expected := Initial;
            Count := Deny_After;
            for P in 1 .. Count loop
               if Trace (P).For_Write then
                  Locate (Trace (P).Destination.Register, S, Found);
                  Check (Found, "modeled completed write"); Write (Expected, S, Trace (P).Value);
               end if;
            end loop;
         end if;
         if Tail_Read_Denied then
            Check (Wide_Data, "tail denial is a wide data case");
            IO.Readable (4) := False; Expected := Initial; Count := 0;
         end if;
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available, Name & " lookup");
         C.Describe_Node (Service, Node, Item, Status);
         Check (Status = C.Values.Available, Name & " descriptor");
         F.Evaluate_Named (Service, IO, Items, Item, 100_000, Result, Status);
         Wanted := (if Tail_Read_Denied or else (Wide_Data and then Deny_After in 0 .. 6)
           then AML_Execute.Unsupported
           elsif Deny_After >= 0 then AML_Execute.Unsupported_Value
           elsif Wide_Data then AML_Execute.Object_Returned else AML_Execute.Returned);
         Check (Status = C.Values.Available and then Result.Status = Wanted,
           Name & " expected " & Wanted'Image & " got " & Result.Status'Image);
         if Wide_Data and then Wanted = AML_Execute.Object_Returned then
            Handle := Result.Handle;
            C.Describe_Result (Service, Handle, Description, Status);
            Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
              and then Description.Length = Data'Length, Name & " buffer length");
            C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
            Check (Status = C.Values.Available and then Copied = Data'Length, Name & " buffer copy");
            for I in Data'Range loop Check (Data (I) = Expected_Buffer (I), Name & " ACPICA byte" & I'Image); end loop;
            C.Release_Result (Service, Handle, Status);
            Check (Status = C.Values.Available and then Handle = C.Values.No_Value, Name & " buffer release");
         elsif Wanted = AML_Execute.Returned then Check (Result.Number = (if Writing then 0
           elsif Nested then (if Revision = 1 then Index_Width_Oracle.NRED32 else Index_Width_Oracle.NRED64)
           else (if Revision = 1 then Index_Width_Oracle.BRED32 else Index_Width_Oracle.BRED64)),
           Name & " ACPICA result"); end if;
         Check (IO.Calls = Count, Name & " exact dispatch count");
         for P in 1 .. Count loop Check (IO.Requests (P) = Trace (P), Name & " request" & P'Image); end loop;
         Check (IO.Bytes = Expected, Name & " all memory effects incl padded selector high bits");
         Check (not F.Uncertain (Service).Present and then C.Retained_Results (Service) = 0, Name & " no result or unknown");
      end Test;
      procedure Unsupported_Data_Read (Name : AML_Names.Segment; Writing : Boolean) is
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Result : C.Values.Result;
      begin
         IO.Bytes := Initial; IO.Calls := 0;
         IO.Readable := [others => True]; IO.Writable := [others => True]; IO.Revoke_After := 0;
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available, Name & " rejected method lookup");
         C.Describe_Node (Service, Node, Item, Status);
         Check (Status = C.Values.Available, Name & " rejected method descriptor");
         F.Evaluate_Named (Service, IO, Items, Item, 100_000, Result, Status);
         Check (Status = C.Values.Available and then Result.Status =
           (if Writing then AML_Execute.Unsupported else AML_Execute.Unsupported_Value), Name & " wide data read rejected");
         Check (IO.Calls = 0 and then IO.Bytes = Initial and then C.Retained_Results (Service) = 0,
           Name & " unsupported data read has no effects/results");
      end Unsupported_Data_Read;
   begin
      for I in Initial'Range loop
         Initial (I) := (if Revision = 1 and then I mod 8 >= 4 then 0 else AML_Decode.Byte (16#A0# + I));
      end loop;
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "identity");
      C.Stage_Table (Service, 1, C.DSDT, Raw, Staged); Check (Staged = C.Staged, "stage");
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 100_000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed, "module");
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      declare Node : C.Namespace.Node_ID; begin
         C.Find_Child (Service, C.Namespace.Root, "REG0", Node, Status);
         C.Describe_Node (Service, Node, Region, Status); Check (Status = C.Values.Available, "region");
      end;
      F.Clear (Items);
      for S in Slot loop
         F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
         Check (Status = C.Values.Available, "region binding");
      end loop;
      declare
         Node : C.Namespace.Node_ID;
         Description : C.Values.Field_Description;
         Key : C.Values.Field_Key;
      begin
         C.Find_Child (Service, C.Namespace.Root, "DAT0", Node, Status);
         C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
         Check (Status = C.Values.Available, "data descriptor");
         F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (5)), Items, Status);
         Check (Status = C.Values.Available, "data binding");
      end;
      Test ("BRED", False, False); Test ("BWRT", False, True);
      Test ("NRED", True, False); Test ("NWRT", True, True);
      for Prefix in 0 .. 4 loop Test ("BRED", False, False, Prefix); end loop;
      for Prefix in 0 .. 50 loop Test ("NRED", True, False, Prefix); end loop;
      Test ("BRED", False, False); Test ("NRED", True, False);
      Test ("WFUL", False, True, Wide_Data => True);
      Test ("WONE", False, True, Wide_Data => True);
      Test ("WZER", False, True, Wide_Data => True);
      for Prefix in 0 .. 8 loop
         Test ("WFUL", False, True, Prefix, Wide_Data => True);
         Test ("WONE", False, True, Prefix, Wide_Data => True);
         Test ("WZER", False, True, Prefix, Wide_Data => True);
      end loop;
      Test ("WFUL", False, True, Wide_Data => True, Tail_Read_Denied => True);
      Unsupported_Data_Read ("WRED", False); Unsupported_Data_Read ("WPRV", True);
      Test ("WFUL", False, True, Wide_Data => True);
   end Exercise;
begin
   Check (Ada.Command_Line.Argument_Count = 1, "revision argument");
   if Ada.Command_Line.Argument (1) = "32" then Exercise (1);
   elsif Ada.Command_Line.Argument (1) = "64" then Exercise (2);
   else raise Program_Error with "unknown revision"; end if;
   Ada.Text_IO.Put_Line ("Index width checks" & Natural'Image (Checks));
end Index_Width_Tests;
