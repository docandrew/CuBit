with CuBit.Messages; use CuBit.Messages;
with CuBit.Protocols;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with CCL.Sessions;
with CCL.VM;

package body Control_Host is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.VM.Value_Kind;
   type Host_Binding is (Clock_Monotonic);
   for Host_Binding use (Clock_Monotonic => 1);
   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Initialized : Boolean := False;
   Periodic : CCL.Periodic_Programs.Program;
   type Context_Type is null record;

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
      Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean)
   is
      pragma Unreferenced (Context);
      Milliseconds : Unsigned_64;
   begin
      Value := CCL.VM.Integer_Constant (0); Success := False;
      if Binding /= Host_Binding'Enum_Rep (Clock_Monotonic) or else
        Argument.Kind /= CCL.VM.Integer_Value or else Argument.Integer /= 0 then return; end if;
      Read_Clock (Success, Milliseconds);
      if Success and then Milliseconds <= Unsigned_64 (Integer_64'Last) then
         Value := CCL.VM.Integer_Constant (Integer_64 (Milliseconds));
      else Success := False;
      end if;
   end Invoke;
   procedure Interpret_Live is new CCL.Language.Interpret_With_Host (Context_Type, Invoke);
   function Now (Context : Context_Type) return Unsigned_64 is
      pragma Unreferenced (Context);
   begin
      return syscall (SYSCALL_GETTIME);
   end Now;
   procedure Pump_Periodic is new CCL.Periodic_Programs.Evaluate_Due
     (Context_Type, Now, Invoke);

   procedure Initialize (Success : out Boolean) is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Operation : CCL.Catalog.Resolved_Operation;
      Found, Available : Boolean;
      Sample : Unsigned_64;
   begin
      Initialized := False; Success := False;
      CCL.Catalog.Initialize (Catalog); CCL.Catalog.Initialize (Grants);
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
      Initialized := True; Success := True;
   end Initialize;

   procedure Evaluate (Source : String; Result : out CCL.Language.Interpretation_Result) is
      Context : Context_Type;
   begin
      if not Initialized then
         Result := (Status => CCL.Language.Host_Authority_Denied, others => <>); return;
      end if;
      Interpret_Live (Source, CCL.Sessions.Default_Fuel, Catalog, Grants, Context, Result);
   end Evaluate;

   function Monitor return CCL.Periodic_Programs.Program is (Periodic);
   function Next_Deadline return Unsigned_64 is
     (CCL.Periodic_Programs.Next_Deadline (Periodic));

   procedure Start_Monitor (Source : String; Accepted : out Boolean) is
      Status : CCL.Periodic_Programs.Load_Result;
      use type CCL.Periodic_Programs.Load_Result;
   begin
      CCL.Periodic_Programs.Load
        (Periodic, Source, syscall (SYSCALL_GETTIME), 1_000,
         CCL.Sessions.Default_Fuel, Status);
      Accepted := Status = CCL.Periodic_Programs.Loaded;
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
      Pump_Periodic (Periodic, Catalog, Grants, Context, Updated);
   end Pump;
end Control_Host;
