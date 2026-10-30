pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Decode;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Datums;
with AML_Field_Layout;
with AML_Identity;
with AML_Identity.Issuer;
with AML_Names;
with Bank_Width_Oracle;
procedure Bank_Width_Tests is
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
     Namespace_Node_Capacity => 96, Retained_Result_Capacity => 16);
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
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
   type Slot is (Bank_Port, Byte_Data, Word_Low, Word_High, Dword_Low, Dword_High, Qword_Low, Qword_High);
   type Register_Values is array (Slot) of AML_Decode.Integer_Value;
   type Register_IDs is array (Slot) of A.Register_ID;
   IDs : constant Register_IDs := [16#8000_0000_0000_0011#, 16#8000_0000_0000_0022#,
     16#8000_0000_0000_0033#, 16#9000_0000_0000_0044#, 16#A000_0000_0000_0055#,
     16#B000_0000_0000_0066#,16#C000_0000_0000_0077#,16#D000_0000_0000_0088#];
   type Bank_Trace is array (Positive range 1 .. 16) of AML_Decode.Integer_Value;
   type Slot_Trace is array (Positive range 1 .. 16) of Slot;
   type Direction_Trace is array (Positive range 1 .. 16) of Boolean;
   type Bus_Event is record
      Destination : Slot := Bank_Port;
      Writing : Boolean := False;
      Value : AML_Decode.Integer_Value := 0;
   end record;
   type Bus_Trace is array (Positive range 1 .. 64) of Bus_Event;
   type Context is limited record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Registers : Register_Values := [others => 0];
      Bank_Writes : Natural := 0;
      Trace : Bank_Trace := [others => 0];
      Data_Reads : Natural := 0;
      Data_Requests : Natural := 0;
      Data_Slots : Slot_Trace := [others => Bank_Port];
      Data_Writes : Direction_Trace := [others => False];
      Events : Bus_Trace := [others => <>];
      Event_Count : Natural := 0;
      Allow_Data_Read : Boolean := True;
   end record;
   function Pending (IO : Context) return Boolean is
     (IO.Owner = AML_Identity.No_Identity);
   function Width_Of (S : Slot) return T.Access_Width is
     (case S is when Bank_Port | Byte_Data => T.Byte_Access,
       when Word_Low | Word_High => T.Word_Access,
       when Dword_Low | Dword_High => T.Dword_Access,
       when Qword_Low | Qword_High => T.Qword_Access);
   function Offset_Of (S : Slot) return AML_Decode.Integer_Value is
     (case S is when Bank_Port => 0, when Byte_Data => 8,
       when Word_Low => 16, when Word_High => 18,
       when Dword_Low => 24, when Dword_High => 28,
       when Qword_Low => 40, when Qword_High => 48);
   procedure Locate (ID : A.Register_ID; S : out Slot; Found : out Boolean) is
   begin
      S := Bank_Port; Found := False;
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
      Geometry := (Region => (Space => 0, Base => 16#1000#, Length => 64),
        Byte_Offset => Offset_Of (S), Width => Width_Of (S));
      Accepted := Found and then IO.Owner /= AML_Identity.No_Identity
        and then Target.Provider = IO.Owner and then Target.Endpoint = 7 and then Target.Epoch = 1;
   end Describe;
   procedure Authorize (IO : Context; Target : A.Target;
     For_Write : Boolean; Accepted : out Boolean) is
      Geometry : AML_Field_Layout.Register_Geometry;
   begin
      Describe (IO, Target, Geometry, Accepted);
      Accepted := Accepted and then (For_Write or else Target.Register = IDs (Bank_Port) or else IO.Allow_Data_Read);
   end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      S : Slot;
      Found, Allowed : Boolean;
   begin
      Reply := (others => <>);
      Locate (Item.Destination.Register, S, Found);
      Authorize (IO, Item.Destination, Item.For_Write, Allowed);
      if not Found or else not Allowed then return; end if;
      IO.Event_Count := IO.Event_Count + 1;
      if IO.Event_Count > IO.Events'Length then raise Program_Error with "bus trace capacity"; end if;
      IO.Events (IO.Event_Count) := (S, Item.For_Write, Item.Value);
      if S /= Bank_Port then
         IO.Data_Requests := IO.Data_Requests + 1;
         if IO.Data_Requests > IO.Data_Slots'Length then raise Program_Error with "data trace capacity"; end if;
         IO.Data_Slots (IO.Data_Requests) := S;
         IO.Data_Writes (IO.Data_Requests) := Item.For_Write;
      end if;
      if Item.For_Write then
         if Item.Value > AML_Field_Datums.Width_Mask (Width_Of (S)) then return; end if;
         IO.Registers (S) := Item.Value;
         if S = Bank_Port then
            IO.Bank_Writes := IO.Bank_Writes + 1;
            if IO.Bank_Writes > IO.Trace'Length then raise Program_Error with "trace capacity"; end if;
            IO.Trace (IO.Bank_Writes) := Item.Value;
         end if;
      elsif S /= Bank_Port then IO.Data_Reads := IO.Data_Reads + 1;
      end if;
      Reply := (A.Completed, IO.Owner, (if Item.For_Write then 0 else IO.Registers (S)), A.No_Operation);
   end Dispatch;
   procedure Confirmed (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO, Expected);
   begin Accepted := False; end Confirmed;
   package F is new C.Field_IO (Context, Pending, Describe, Dispatch, Confirmed,
     Authorize_Target => Authorize);
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label; end if;
   end Check;
   procedure Exercise (Revision : AML_Decode.Byte) is
      Raw : constant AML_Decode.Bytes := (if Revision = 1 then Bank_Width_Oracle.Table32 else Bank_Width_Oracle.Table64);
      Service : aliased C.State (1, Raw'Length, Raw'Length);
      IO : Context;
      Items : F.Binding_Set;
      Staged : C.Stage_Status;
      Frozen : C.Freeze_Status;
      Status : C.Values.Access_Status;
      Execution : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
      Issued : Boolean;
      procedure Expect (Name : AML_Names.Segment; Expected : AML_Decode.Integer_Value;
        Wide : Boolean := False) is
         Node : C.Namespace.Node_ID;
         Item : C.Node_Descriptor;
         Result : C.Values.Result;
         Description : C.Values.Value_Description;
         Handle : C.Values.Value_Handle;
         Data : AML_Decode.Bytes (1 .. 8);
         Copied : Natural;
      begin
         C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
         Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " lookup");
         C.Describe_Node (Service, Node, Item, Status);
         Check (Status = C.Values.Available, Name & " descriptor");
         F.Evaluate_Named (Service, IO, Items, Item, 10_000, Result, Status);
         Check (Status = C.Values.Available, Name & " access status");
         if Wide and then Revision = 1 then
            Check (Result.Status = AML_Execute.Object_Returned, Name & " AML32 buffer status: " & Result.Status'Image);
            Handle := Result.Handle;
            C.Describe_Result (Service, Handle, Description, Status);
            Check (Status = C.Values.Available and then Description.Kind = C.Values.Buffer_Description
              and then Description.Length = Data'Length, Name & " buffer length");
            C.Read_Result_Bytes (Service, Handle, 0, Data, Copied, Status);
            Check (Status = C.Values.Available and then Copied = Data'Length, Name & " buffer copy");
            for I in Data'Range loop
               Check (Data (I) = AML_Decode.Byte (Interfaces.Shift_Right (Expected, (I - 1) * 8) and 255),
                 Name & " ACPICA little-endian buffer byte");
            end loop;
            Check (C.Retained_Results (Service) = 1, Name & " one retained result");
            C.Release_Result (Service, Handle, Status);
            Check (Status = C.Values.Available and then Handle = C.Values.No_Value, Name & " release");
         else
            Check (Result.Status = AML_Execute.Returned, Name & " scalar status: " & Result.Status'Image);
            Check (Result.Number = Expected, Name & " ACPICA scalar");
         end if;
         Check (C.Retained_Results (Service) = 0, Name & " no retained roots");
      end Expect;
      procedure Reset is
      begin
         IO.Allow_Data_Read := True; IO.Bank_Writes := 0; IO.Data_Requests := 0; IO.Event_Count := 0;
         Expect ("IN00", 0);
         IO.Bank_Writes := 0; IO.Data_Reads := 0; IO.Data_Requests := 0; IO.Event_Count := 0; IO.Trace := [others => 0];
      end Reset;
      procedure Trace_Is (Expected : Bank_Trace; Count : Natural; Label : String) is
      begin
         Check (IO.Bank_Writes = Count, Label & " selector write count");
         Check (IO.Event_Count = 2 * Count, Label & " complete bank/data request count");
         for I in 1 .. Count loop
            Check (IO.Trace (I) = Expected (I), Label & " bank selector value");
            Check (IO.Events (2 * I - 1).Destination = Bank_Port
              and then IO.Events (2 * I - 1).Writing and then IO.Events (2 * I - 1).Value = Expected (I),
              Label & " selector is immediately before data");
            Check (IO.Events (2 * I).Destination /= Bank_Port, Label & " next request is data");
         end loop;
      end Trace_Is;
      procedure Data_Is (Expected : Slot_Trace; Directions : String; Label : String) is
      begin
         Check (IO.Data_Requests = Directions'Length, Label & " data request count");
         for I in 1 .. Directions'Length loop
            Check (IO.Data_Slots (I) = Expected (I), Label & " named data target");
            Check (IO.Data_Writes (I) = (Directions (Directions'First + I - 1) = 'w'), Label & " data direction");
            Check (IO.Events (2 * I).Destination = Expected (I)
              and then IO.Events (2 * I).Writing = IO.Data_Writes (I), Label & " interleaved named data target");
         end loop;
      end Data_Is;
   begin
      AML_Identity.Issuer.Issue (IO.Owner, Issued); Check (Issued, "provider identity");
      C.Stage_Table (Service, 1, C.DSDT, Raw, Staged); Check (Staged = C.Staged, "exact table stage");
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK, "freeze");
      C.Execute_Frozen (Service, 100_000, Execution, Status);
      Check (Status = C.Values.Available and then Execution.Modules.Outcome = C.Values.Modules_Completed,
        "width module import: " & Execution.Modules.Outcome'Image & "/" & Execution.Modules.Failure'Image);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available, "members");
      F.Clear (Items);
      for S in Slot loop
         declare
            Name : constant AML_Names.Segment := (case S is when Bank_Port => "BNK0",
              when Byte_Data => "DATB", when Word_Low => "DAW0", when Word_High => "DAW1",
              when Dword_Low => "DAD0", when Dword_High => "DAD1", when Qword_Low => "DAQ0", when Qword_High => "DAQ1");
            Node : C.Namespace.Node_ID;
            Description : C.Values.Field_Description;
            Key : C.Values.Field_Key;
         begin
            C.Find_Child (Service, C.Namespace.Root, Name, Node, Status);
            Check (Status = C.Values.Available and then Node /= C.Namespace.Root, Name & " leaf");
            C.Read_Field_Descriptor (Service, Node, Description, Key, Status);
            Check (Status = C.Values.Available, Name & " direct descriptor");
            F.Add_Binding (Service, IO, Key, (IO.Owner, 7, 1, IDs (S)), Items, Status);
            Check (Status = C.Values.Available, Name & " named leaf binding");
         end;
      end loop;
      declare
         Node : C.Namespace.Node_ID;
         Region : C.Node_Descriptor;
      begin
         C.Find_Child (Service, C.Namespace.Root, "REG0", Node, Status);
         C.Describe_Node (Service, Node, Region, Status); Check (Status = C.Values.Available, "wide catalog region");
         for S in Slot range Byte_Data .. Qword_High loop
            F.Add_Region_Register (Service, IO, Region, (IO.Owner, 7, 1, IDs (S)), True, True, Items, Status);
            Check (Status = C.Values.Available, "explicit wide named catalog entry");
         end loop;
      end;
      Reset; Expect ("RAB0", 16#A5#); Trace_Is ([others => 3], 1, "AnyAcc read");
      Data_Is ([1 => Byte_Data, others => Bank_Port], "r", "AnyAcc read");
      Reset; Expect ("RAW0", 16#4DA5#); Trace_Is ([others => 3], 2, "Word read");
      Data_Is ([1 => Word_Low, 2 => Word_High, others => Bank_Port], "rr", "Word read");
      Reset; Expect ("RAD0", 16#09A5C3E1#); Trace_Is ([others => 3], 2, "Dword read");
      Data_Is ([1 => Dword_Low, 2 => Dword_High, others => Bank_Port], "rr", "Dword read");
      Reset; Expect ("RAQ0", 16#09F1D2B394755637#, Wide => True); Trace_Is ([others => 3], 2, "Qword read");
      Data_Is ([1 => Qword_Low, 2 => Qword_High, others => Bank_Port], "rr", "Qword read");
      Reset; Expect ("WWR0", 0); Trace_Is ([others => 3], 4, "Word Preserve");
      Data_Is ([1|2 => Word_Low, 3|4 => Word_High, others => Bank_Port], "rwrw", "Word Preserve");
      Check (IO.Registers (Word_Low) = 16#34C3# and then IO.Registers (Word_High) = 16#6B12#,
        "Word Preserve independent ACPICA bytes");
      Expect ("DAW0", 16#34C3#); Expect ("DAW1", 16#6B12#);
      Reset; Expect ("WDR0", 0); Trace_Is ([others => 3], 4, "Dword Preserve");
      Data_Is ([1|2 => Dword_Low, 3|4 => Dword_High, others => Bank_Port], "rwrw", "Dword Preserve");
      Check (IO.Registers (Dword_Low) = 16#34567887# and then IO.Registers (Dword_High) = 16#6B4D2F12#,
        "Dword Preserve independent ACPICA bytes");
      Expect ("DAD0", 16#34567887#); Expect ("DAD1", 16#6B4D2F12#);
      Reset; Expect ("WQR0", 0); Trace_Is ([others => 3], 4, "Qword Preserve");
      Data_Is ([1|2 => Qword_Low, 3|4 => Qword_High, others => Bank_Port], "rwrw", "Qword Preserve");
      Check (IO.Registers (Qword_Low) = 16#7766554433221118# and then IO.Registers (Qword_High) = 16#E2C3A48566472888#,
        "Qword Preserve independent ACPICA bytes");
      Expect ("DAQ0", 16#7766554433221118#, Wide => True);
      Expect ("DAQ1", 16#E2C3A48566472888#, Wide => True);
      Expect ("RAQ0", 16#8877665544332211#, Wide => True);
      Reset; IO.Allow_Data_Read := False; Expect ("WON0", 0);
      Check (IO.Registers (Word_Low) = 16#F3FF# and then IO.Data_Reads = 0, "WriteAsOnes without data-read permission");
      Trace_Is ([others => 3], 1, "WriteAsOnes");
      Data_Is ([1 => Word_Low, others => Bank_Port], "w", "WriteAsOnes");
      Reset; IO.Allow_Data_Read := False; Expect ("WZE0", 0);
      Check (IO.Registers (Word_Low) = 16#0300# and then IO.Data_Reads = 0, "WriteAsZeros without data-read permission");
      Trace_Is ([others => 3], 1, "WriteAsZeros");
      Data_Is ([1 => Word_Low, others => Bank_Port], "w", "WriteAsZeros");
   end Exercise;
begin
   Exercise (1); Exercise (2);
   Ada.Text_IO.Put_Line ("Bank width checks" & Natural'Image (Checks));
end Bank_Width_Tests;
