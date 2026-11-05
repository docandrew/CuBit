with AML_Identity;
with AML_Frame_Handles;
with AML_Object_Identifiers;

generic
   Max_Explicit_Locks : Positive;
   Max_Method_Locks : AML_Frame_Handles.Frame_Capacity;
   Max_Method_Entries : AML_Frame_Handles.Frame_Capacity :=
     AML_Frame_Handles.Max_Active_Frames;
   Max_Acquisition_Depth : Positive := Positive'Last;
   Max_Token_Stamp : Positive := Positive'Last;
   type Admission is private;
   type Method_Key is private;
   No_Method : Method_Key;
   with function Domain_Approved
     (C : Admission; Owner : AML_Identity.Identity;
      Domain : AML_Frame_Handles.Invocation_Domain) return Boolean;
   with function Mutex_Approved
     (C : Admission; Owner : AML_Identity.Identity;
      Address : AML_Object_Identifiers.Object_Address;
      Level : Natural) return Boolean;
   with function Method_Approved
     (C : Admission; Owner : AML_Identity.Identity;
      Key : Method_Key; Level : Natural) return Boolean;
   with function Frame_Approved
     (C : Admission; Frame : AML_Frame_Handles.Frame_Handle) return Boolean;
   with function Epoch_Approved
     (C : Admission; Old_Owner, New_Owner : AML_Identity.Identity) return Boolean;
package AML_Mutex_Invocations with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- An arena owns one instance of State, outside rollback/copyable Tree.
   -- These transitions model SINGLE runnable, uncontended ordinary locks only.
   -- A caller must NOT use success here as an OS contention/global-lock result.
   -- Approval projections inspect opaque OWNER-VALIDATED admission records.
   -- They must not access Arena or perform live validation. Actual Namespace
   -- wrappers authenticate the full live registry plus operation target first,
   -- create private admission, then call here without any mutation/yield gap.
   -- Valid below is STRUCTURAL, never a claim of live namespace authority.
   -- The instance and admission constructors remain private to that owner.
   subtype Explicit_Capacity is Positive range 1 .. AML_Object_Identifiers.Max_Objects;
   -- Invalid host configuration fails at elaboration, never grows at runtime.
   Explicit_Limit : constant Explicit_Capacity := Max_Explicit_Locks;
   subtype Sync_Level is Natural range 0 .. 15;
   subtype Acquisition_Depth is Natural range 0 .. Max_Acquisition_Depth;
   subtype Root_Position is Natural range 0 .. Explicit_Limit;
   type Outcome is
     (Succeeded, Invalid_State, Invalid_Domain, Invalid_Object,
      Invalid_Method, Invalid_Frame, Stale_Token, Order_Error,
      Capacity_Limit, Depth_Limit, Token_Limit, Not_Acquired, Busy,
      Unsupported_Global);
   type State is limited private;
   type Snapshot is private;
   type Method_Token is private;
   No_Token : constant Method_Token;
   function Token_Frame (Token : Method_Token) return AML_Frame_Handles.Frame_Handle;
   function Observe (S : State) return Snapshot;
   function Valid (S : State) return Boolean;
   function Active (S : State) return Boolean;
   function Current_Level (S : State) return Sync_Level;
   function Held_Count (S : State) return Root_Position;
   function Issued_Tokens (S : State) return Natural;
   -- Enumerates acquired-list order, one exact address per distinct object.
   -- Namespace must authenticate each returned owner/address before GC seeding.
   function Held_Address
     (S : State; Position : Root_Position)
      return AML_Object_Identifiers.Object_Address;
   function Owner_Of (S : State) return AML_Identity.Identity;
   function Depth_Of
     (S : State; Address : AML_Object_Identifiers.Object_Address)
      return Acquisition_Depth;
   function Held_Level (S : State; Position : Root_Position) return Sync_Level;
   subtype Method_Position is Natural range 0 .. Max_Method_Locks;
   function Method_Count (S : State) return Method_Position;
   function Method_At (S : State; Position : Method_Position) return Method_Key;
   function Method_Level (S : State; Position : Method_Position) return Sync_Level;
   procedure Rotate_Epoch
     (S : in out State; C : Admission; New_Owner : AML_Identity.Identity;
      Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old)
       and then (if Result = Succeeded then not Active (S)
         and then AML_Identity."=" (Owner_Of (S), New_Owner))
       and then Issued_Tokens (S) = Issued_Tokens (S)'Old;
   procedure Begin_Invocation
     (S : in out State; C : Admission; Owner : AML_Identity.Identity;
      Serial : AML_Frame_Handles.Invocation_Serial;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old)
       and then (if Result = Succeeded then Active (S) and then AML_Identity."=" (Owner_Of (S), Owner) and then Current_Level (S) = 0);
   procedure Acquire_Uncontended
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Address : AML_Object_Identifiers.Object_Address; Level : Sync_Level;
      Global_Lock : Boolean; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old);
   procedure Release
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Address : AML_Object_Identifiers.Object_Address; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old);
   procedure Enter_Method
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Key : Method_Key;
      Level : Sync_Level; Frame : AML_Frame_Handles.Frame_Handle;
      Token : out Method_Token; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old)
       and then (if Result = Succeeded then Token /= No_Token else Token = No_Token);
   procedure Leave_Method
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain;
      Token : Method_Token; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old);
   -- End refuses unclosed implicit entries; explicit depths are force-released.
   -- Force_Exit first unwinds all implicit entries then all explicit locks.
   -- Neither clears owner binding, invocation high-water mark or token stamps.
   -- Owner adapter performs authentication before this pure transition.
   -- No AML allocation/provider IO occurs here.
   procedure End_Invocation
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old)
       and then (if Result = Succeeded then not Active (S) and then Held_Count (S) = 0 and then Current_Level (S) = 0);
   procedure Force_Exit
     (S : in out State; C : Admission;
      Domain : AML_Frame_Handles.Invocation_Domain; Result : out Outcome)
     with Post =>
       (if Result = Succeeded or else Valid (S)'Old then Valid (S))
       and then (if Result /= Succeeded then Observe (S) = Observe (S)'Old)
       and then (if Result = Succeeded then not Active (S) and then Held_Count (S) = 0 and then Current_Level (S) = 0);
private
   subtype Lock_Index is Natural range 0 .. Explicit_Limit;
   subtype Method_Index is Natural range 0 .. Max_Method_Locks;
   subtype Entry_Index is Natural range 0 .. Max_Method_Entries;
   subtype Token_Stamp is Natural range 0 .. Max_Token_Stamp;
   type Lock_Record is record
      Address : AML_Object_Identifiers.Object_Address :=
        AML_Object_Identifiers.No_Address;
      Level, Original : Sync_Level := 0;
      Depth : Acquisition_Depth := 0;
      Previous, Next : Lock_Index := 0;
   end record;
   type Lock_Array is array (Positive range 1 .. Explicit_Limit) of Lock_Record;
   type Method_Record is record
      Key : Method_Key := No_Method;
      Level, Original : Sync_Level := 0;
      Depth : Acquisition_Depth := 0;
   end record;
   type Method_Array is array (Positive range 1 .. Max_Method_Locks) of Method_Record;
   type Entry_Record is record
      Frame : AML_Frame_Handles.Frame_Handle := AML_Frame_Handles.No_Frame;
      Stamp : Token_Stamp := 0;
      Method : Method_Index := 0;
   end record;
   type Entry_Array is array (Positive range 1 .. Max_Method_Entries) of Entry_Record;
   type Method_Token is record
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Position : Entry_Index := 0;
      Stamp : Token_Stamp := 0;
      Frame : AML_Frame_Handles.Frame_Handle := AML_Frame_Handles.No_Frame;
   end record;
   No_Token : constant Method_Token := (others => <>);
   type Snapshot is record
      Owner : AML_Identity.Identity := AML_Identity.No_Identity;
      Last_Serial : AML_Frame_Handles.Invocation_Serial := 0;
      Domain : AML_Frame_Handles.Invocation_Domain := AML_Frame_Handles.No_Domain;
      Running : Boolean := False;
      Level : Sync_Level := 0;
      Head, Count : Lock_Index := 0;
      Top : Entry_Index := 0;
      Issued : Token_Stamp := 0;
      Locks : Lock_Array := [others => <>];
      Methods : Method_Array := [others => <>];
      Entries : Entry_Array := [others => <>];
   end record;
   type State is limited record
      Data : Snapshot;
   end record;
end AML_Mutex_Invocations;
