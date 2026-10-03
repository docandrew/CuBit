with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Timer;
with CCL.Sessions;
with CCL.Streams;
with CCL.Host_Values;
with CCL_Config_Bindings;
with CCL_Image_Bindings;
with CCL_Stream_Table;

package body Control_Host is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Streams.Handle;
   type Host_Binding is (Clock_Monotonic, Timer_Every);
   for Host_Binding use (Clock_Monotonic => 1, Timer_Every => 2);
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Initialized : Boolean := False;
   Periodic : CCL.Periodic_Programs.Program;

   --  The browser tabs' sessions. Slot 0 is the fresh session of a request
   --  without one, discarded afterwards; the others belong to the most
   --  recently seen tabs (the least recently used is replaced). A slot keeps
   --  its tab's definitions, kept values and streams.
   NO_SESSION : constant Unsigned_64 := 0;
   MAX_SESSIONS : constant := 4;
   subtype Slot_Index is Natural range 0 .. MAX_SESSIONS;
   subtype Tab_Slot is Slot_Index range 1 .. MAX_SESSIONS;
   type Session_Slot is record
      Owner : Unsigned_64 := NO_SESSION;
      Last_Used : Unsigned_64 := 0;
      Session : CCL.Sessions.Session;
      Streams : CCL_Stream_Table.Table;
   end record;
   type Slot_Array is array (Slot_Index) of Session_Slot;
   Slots : Slot_Array;
   --  The session whose expression the live monitor runs.
   Monitor_Slot : Slot_Index := 0;

   type Context_Type is record
      Slot : Slot_Index := 0;
   end record;

   function Now_Ms return Unsigned_64 is (syscall (SYSCALL_GETTIME));

   procedure Read_Clock (Available : out Boolean; Value : out Unsigned_64) is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Request.tag := (label => CuBit.Protocols.CLOCK_OP_MONOTONIC_MS,
                      length => 1, flags => 0, reserved => 0);
      Tag := capCall (CAP_SLOT_CLOCK, Request);
      Available := Tag.label = 16#F000# and Tag.length = 1;
      Value := (if Available then Request.words (0) else 0);
   end Read_Clock;

   procedure Invoke
     (Context : in out Context_Type; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      Milliseconds : Unsigned_64;
      Handle : CCL.Streams.Handle;
   begin
      Reply.Value := CCL.Host_Values.Integer_Constant (0); Reply.Success := False;
      if CCL_Config_Bindings.Handles (Binding) then
         CCL_Config_Bindings.Invoke (Binding, Argument, Reply);
         return;
      elsif CCL_Image_Bindings.Handles (Binding) then
         --  Drawing data as images needs no authority (interfaces/image.schema).
         CCL_Image_Bindings.Invoke (Binding, Argument, Reply);
         return;
      elsif Binding = Host_Binding'Enum_Rep (Timer_Every) then
         --  timer.every: a stream in this session's own table.
         if Argument.Kind = CCL.Host_Values.Integer_Value and then
           Argument.Integer in CCL_Stream_Table.MIN_PERIOD_MS .. CCL_Stream_Table.MAX_PERIOD_MS
         then
            CCL_Stream_Table.Open_Timer
              (Slots (Context.Slot).Streams, CCL_Stream_Table.Period_Ms (Argument.Integer),
               Now_Ms, Handle);
            Reply.Success := Handle /= CCL.Streams.No_Handle;
            if Reply.Success then
               Reply.Value := CCL.Host_Values.Integer_Constant (Integer_64 (Handle));
            end if;
         end if;
         return;
      end if;
      if Binding /= Host_Binding'Enum_Rep (Clock_Monotonic) or else
        Argument.Kind /= CCL.Host_Values.Integer_Value or else Argument.Integer /= 0 then return; end if;
      Read_Clock (Reply.Success, Milliseconds);
      if Reply.Success and then Milliseconds <= Unsigned_64 (Integer_64'Last) then
         Reply.Value := CCL.Host_Values.Integer_Constant (Integer_64 (Milliseconds));
      else Reply.Success := False;
      end if;
   end Invoke;

   procedure Read_Stream
     (Context : in out Context_Type; Request : CCL.Streams.View_Request;
      Reply : in out CCL.Streams.View_Reply) is
   begin
      CCL_Stream_Table.Read (Slots (Context.Slot).Streams, Request, Reply);
   end Read_Stream;

   procedure Submit_Live is new CCL.Sessions.Submit_With_Values
     (Context_Type, Invoke, Read_Stream => Read_Stream);
   function Now (Context : Context_Type) return Unsigned_64 is
      pragma Unreferenced (Context);
   begin
      return Now_Ms;
   end Now;
   procedure Pump_Periodic is new CCL.Periodic_Programs.Evaluate_Values_Due
     (Context_Type, Now, Invoke, Read_Stream => Read_Stream);

   --  Deliver what is due on Slot's streams.
   procedure Pump_Streams (Slot : Slot_Index) is
      Arrived : Boolean;
   begin
      CCL_Stream_Table.Pump (Slots (Slot).Streams, Now_Ms, Arrived);
   end Pump_Streams;

   --  Close Slot's streams that its session no longer binds.
   procedure Retain_Streams (Slot : Slot_Index) is
      function Held (Handle : CCL.Streams.Handle) return Boolean is
        (CCL.Sessions.Holds_Stream (Slots (Slot).Session, Handle));
      procedure Retain is new CCL_Stream_Table.Retain (Held);
   begin
      Retain (Slots (Slot).Streams);
   end Retain_Streams;

   procedure Reset (Slot : Slot_Index; Owner : Unsigned_64) is
   begin
      if Slot = Monitor_Slot and then
        CCL.Periodic_Programs.State (Periodic) in CCL.Periodic_Programs.Waiting | CCL.Periodic_Programs.Executing
      then
         --  The monitor's session is going: so is the monitor.
         CCL.Periodic_Programs.Stop (Periodic);
      end if;
      CCL.Sessions.Initialize (Slots (Slot).Session, Catalog);
      CCL_Stream_Table.Clear (Slots (Slot).Streams);
      Slots (Slot).Owner := Owner;
      Slots (Slot).Last_Used := Now_Ms;
   end Reset;

   --  The slot of Session: its own, a free one, or the least recently used.
   function Slot_For (Session : Unsigned_64) return Slot_Index is
      Oldest : Tab_Slot := Tab_Slot'First;
   begin
      if Session = NO_SESSION then
         Reset (0, NO_SESSION);
         return 0;
      end if;
      for S in Tab_Slot loop
         if Slots (S).Owner = Session then
            Slots (S).Last_Used := Now_Ms;
            return S;
         end if;
      end loop;
      for S in Tab_Slot loop
         if Slots (S).Owner = NO_SESSION then
            Oldest := S; exit;
         elsif Slots (S).Last_Used < Slots (Oldest).Last_Used then
            Oldest := S;
         end if;
      end loop;
      Reset (Oldest, Session);
      return Oldest;
   end Slot_For;

   procedure Initialize (Success : out Boolean) is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Operation : CCL.Catalog.Resolved_Operation;
      Found, Available : Boolean;
      Sample : Unsigned_64;
   begin
      Initialized := False; Success := False;
      CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
      CCL_Config_Bindings.Install (Catalog, Grants, Available);
      if not Available then return; end if;
      CCL_Image_Bindings.Install (Catalog, Grants, Available);
      if not Available then return; end if;
      --  Session timers: streams in each session's own table.
      CCL.Interfaces.Timer.Publish (Catalog, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      CCL.Interfaces.Timer.Resolve_Every (Catalog, Operation, Found);
      if not Found then return; end if;
      CCL.Catalog.Install (Grants, Operation, Host_Binding'Enum_Rep (Timer_Every), Grant);
      if Grant /= CCL.Catalog.Grant_Added then return; end if;
      CCL.Interfaces.Clock.Publish (Catalog, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms (Catalog, Operation, Found);
      if not Found then return; end if;
      -- Metadata is visible, but install no binding without a successful
      -- invocation of the manifest-granted kernel endpoint. Every later call
      -- still uses that endpoint; this probe does not cache kernel authority.
      Read_Clock (Available, Sample);
      if Available then
         CCL.Catalog.Install (Grants, Operation, Host_Binding'Enum_Rep (Clock_Monotonic), Grant);
         if Grant /= CCL.Catalog.Grant_Added then return; end if;
      end if;
      for S in Slot_Index loop
         CCL.Sessions.Initialize (Slots (S).Session, Catalog);
      end loop;
      Initialized := True; Success := True;
   end Initialize;

   procedure Evaluate
     (Session : Unsigned_64; Source : String; Result : out CCL.Language.Interpretation_Result)
   is
      Slot : Slot_Index;
      Context : Context_Type;
   begin
      if not Initialized then
         Result := (Status => CCL.Language.Host_Authority_Denied, others => <>); return;
      end if;
      Slot := Slot_For (Session);
      Context.Slot := Slot;
      Pump_Streams (Slot);
      Submit_Live (Slots (Slot).Session, Source, CCL.Sessions.Default_Fuel, Grants, Context, Result);
      Retain_Streams (Slot);
   end Evaluate;

   procedure Complete
     (Session : Unsigned_64; Before : String; Result : out CCL.Completions.Result) is
   begin
      --  The tab's own definitions complete too.
      CCL.Sessions.Complete_At (Slots (Slot_For (Session)).Session, Before, ' ', Result);
   end Complete;

   function Monitor return CCL.Periodic_Programs.Program is (Periodic);
   function Next_Deadline return Unsigned_64 is
     (CCL.Periodic_Programs.Next_Deadline (Periodic));

   procedure Start_Monitor (Session : Unsigned_64; Source : String; Accepted : out Boolean) is
      Status : CCL.Periodic_Programs.Load_Result;
      use type CCL.Periodic_Programs.Load_Result;
      Slot : Slot_Index;
      Program : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
      Length : Natural;
      Plain : Boolean;
   begin
      Accepted := False;
      if not Initialized then return; end if;
      --  The tab's expression with its definitions and kept values (its
      --  streams included), run without changing the session.
      Slot := Slot_For (Session);
      CCL.Sessions.Expression_Program (Slots (Slot).Session, Source, Program, Length, Plain);
      if not Plain then return; end if;
      CCL.Periodic_Programs.Load
        (Periodic, Program (1 .. Length), Now_Ms, 1_000,
         CCL.Periodic_Programs.Default_Fuel, Status);
      Accepted := Status = CCL.Periodic_Programs.Loaded;
      if Accepted then Monitor_Slot := Slot; end if;
   end Start_Monitor;

   procedure Stop_Monitor (Identity : Unsigned_64; Accepted : out Boolean) is
   begin
      -- One lab-owned slot, with generation checking so a stale UI action
      -- cannot stop its replacement. This is not remote identity or authority.
      Accepted := Identity /= 0 and then
        Identity = CCL.Periodic_Programs.Identity (Periodic);
      if Accepted then CCL.Periodic_Programs.Stop (Periodic); end if;
   end Stop_Monitor;

   procedure Pump is
      Context : Context_Type;
      Updated : Boolean;
   begin
      if not Initialized then return; end if;
      Context.Slot := Monitor_Slot;
      Pump_Streams (Monitor_Slot);
      Pump_Periodic (Periodic, Catalog, Grants, Context, Updated);
   end Pump;
end Control_Host;
