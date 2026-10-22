with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
with AML_Table_Backing;
procedure Namespace_Notify_Tests is
   use type AML_Decode.Bytes;
   procedure Clock (Value : out Integer_Value; Available : out Boolean);
   package N is new AML_Namespace
     (Capacity => 32, Perform_Delay => AML_Delays.Unavailable_Provider,
      Read_Microseconds => Clock, Max_Pending_Notifications => 2);
   package V is new N.Owned.Collecting;
   use type Integer_Value;
   use type N.Lookup_Status;
   use type V.Access_Status;
   use type N.Load_Status;
   use type N.Notification_State;
   use type AML_Execute.Execution_Status;
   use type N.Notification_Target_Kind;
   A : V.Arena;
   Input : aliased AML_Table_Backing.State (1, 1);
   Checks : Natural := 0;
   Probe : Boolean := False;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Clock (Value : out Integer_Value; Available : out Boolean) is
      Status : V.Access_Status;
      Item : N.Notification_Observation;
      Found : Boolean;
      Before : constant N.Notification_Count := V.Pending_Notifications (A);
      Before_Model : constant N.Notification_State := V.Notification_Model (A) with Ghost;
   begin
      Value := 1; Available := True;
      if not Probe then return; end if;
      V.Configure_Notifications (A, False, Status);
      Check (Status = V.Busy and then V.Pending_Notifications (A) = Before, "busy configure unchanged");
      pragma Assert (V.Notification_Model (A) = Before_Model);
      V.Take_Notification (A, Item, Found, Status);
      Check (Status = V.Busy and then not Found and then V.Pending_Notifications (A) = Before, "busy take unchanged");
      pragma Assert (V.Notification_Model (A) = Before_Model);
   end Clock;
   function Enc (Text : String) return Bytes is
      B : Bytes (1 .. Text'Length);
   begin
      for I in B'Range loop B (I) := Character'Pos (Text (Text'First + I - 1)); end loop;
      return B;
   end Enc;
   function Method (Name : String; Code : Bytes) return Bytes is
     (Bytes'[16#14#, Byte (6 + Code'Length)] & Enc (Name) & [0] & Code);
   Notify_One : constant Bytes := [16#86#] & Enc ("DEV0") & [16#0A#, 16#80#];
   Notify_Two : constant Bytes := [16#86#] & Enc ("DEV0") & [16#0A#, 16#81#];
   Code : constant Bytes := [16#5B#, 16#82#, 5] & Enc ("DEV0") &
     [16#08#] & Enc ("NUM0") & [1] &
     Method ("ONCE", Notify_One & [16#A4#, 1]) &
     Method ("CHLD", Notify_Two & [16#A4#, 1]) &
     Method ("NEST", Notify_One & Enc ("CHLD") & [16#A4#, 1]) &
     Method ("VALU", [16#70#, 16#11#, 4, 16#0A#, 8, 0, 16#60#, 16#A4#, 16#0A#, 16#82#]) &
     Method ("ALOC", [16#86#] & Enc ("DEV0") & Enc ("VALU") & [16#A4#, 1]) &
     Method ("FULL", Notify_One & Notify_Two & Notify_One & [16#A4#, 1]) &
     Method ("BAD0", [16#86#] & Enc ("NUM0") & [1, 16#A4#, 1]) &
     Method ("FAIL", Notify_One & [16#A4#] & Enc ("MISS")) &
     Method ("BUSY", Notify_One & [16#A4#, 16#5B#, 16#33#]);
   Status : V.Access_Status;
   Loaded : N.Load_Status;
   Report : N.Initialization_Report;
   Result : V.Result;
   Item : N.Notification_Observation;
   Found : Boolean;
   procedure Call (Name : String) is
      Located : N.Lookup_Result;
   begin
      V.Find_Object (A, N.Root, Name, Located, Status);
      Check (Status = V.Available and then Located.Status = N.Found, "find");
      V.Invoke (A, Input, Located.Node, [others => (V.Immediate_Argument, 0)], 0, 200, Result, Status);
      Check (Status = V.Available, "invoke access");
   end Call;
   procedure Take (Number : Integer_Value) is
   begin
      V.Take_Notification (A, Item, Found, Status);
      Check (Status = V.Available and then Found and then Item.Number = Number, "FIFO payload");
      Check (Item.Current_Target and then Item.Path_Length = 1 and then Item.Path (1) = "DEV0"
        and then Item.Target_Kind = N.Device_Notification, "descriptive target snapshot");
   end Take;
begin
   V.Configure_Notifications (A, True, Status);
   Check (Status = V.Wrong_Phase and then V.Pending_Notifications (A) = 0, "uninitialized configure");
   V.Take_Notification (A, Item, Found, Status);
   Check (Status = V.Wrong_Phase and then not Found, "uninitialized take");
   V.Initialize (A, Status); Check (Status = V.Available, "initialize");
   V.Configure_Notifications (A, True, Status); Check (Status = V.Wrong_Phase, "loading configure");
   V.Load (A, Code, Bits_64, Loaded, Status);
   Check (Status = V.Available and then Loaded = N.Loaded, "load");
   V.Seal (A, Report, Status); Check (Status = V.Available, "seal");
   Call ("ONCE"); Check (Result.Status = AML_Execute.Returned and then V.Pending_Notifications (A) = 0, "default disabled");
   Call ("BAD0"); Check (Result.Status = AML_Execute.Unsupported_Value and then V.Pending_Notifications (A) = 0, "disabled still validates kind");
   V.Configure_Notifications (A, True, Status); Check (Status = V.Available, "enable");
   Call ("FULL"); Check (Result.Status = AML_Execute.Notify_Limit and then V.Pending_Notifications (A) = 2, "bounded admission");
   Call ("BAD0"); Check (Result.Status = AML_Execute.Unsupported_Value and then V.Pending_Notifications (A) = 2, "full still validates kind first");
   V.Configure_Notifications (A, False, Status); Check (Status = V.Available and then V.Pending_Notifications (A) = 2, "disable preserves");
   Take (16#80#); Take (16#81#);
   V.Configure_Notifications (A, True, Status);
   Call ("NEST"); Check (Result.Status = AML_Execute.Returned and then V.Pending_Notifications (A) = 2, "nested admission");
   Take (16#80#); Take (16#81#);
   Call ("ALOC"); Check (Result.Status = AML_Execute.Returned and then V.Pending_Notifications (A) = 1, "value method allocation with held target");
   Take (16#82#);
   Call ("FAIL"); Check (Result.Status = AML_Execute.Unknown_Name and then V.Pending_Notifications (A) = 1, "later error preserves effect");
   Take (16#80#);
   Probe := True; Call ("BUSY"); Probe := False;
   Check (Result.Status = AML_Execute.Returned and then V.Pending_Notifications (A) = 1, "busy probe outcome");
   Take (16#80#);
   Call ("ONCE");
   V.Reset (A, Status); Check (Status = V.Available and then V.Pending_Notifications (A) = 0, "reset clears");
   V.Load (A, Code, Bits_64, Loaded, Status); Check (Loaded = N.Loaded, "reload");
   V.Seal (A, Report, Status); Check (Status = V.Available, "reseal");
   Call ("ONCE"); Check (Result.Status = AML_Execute.Returned and then V.Pending_Notifications (A) = 0, "reset disables");
   Ada.Text_IO.Put_Line ("Namespace Notify checks" & Natural'Image (Checks));
end Namespace_Notify_Tests;
