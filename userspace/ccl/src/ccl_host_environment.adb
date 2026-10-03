with CCL.Interfaces.Clock;
with CCL.Interfaces.Timer;
with CCL_Stream_Table;
with CCL_Config_Bindings;
with CCL_Execution;
with CCL_Log_Bindings;
with CCL_Image_Bindings;
with CCL_Image_Loading;
with CCL_File_Bindings;
with CCL_Process_Bindings;
with CuBit.Failures;

package body CCL_Host_Environment is
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Integer_64;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;

   CLOCK_BINDING : constant Interfaces.Unsigned_32 := 16#0001_0001#;
   TIMER_EVERY_BINDING : constant Interfaces.Unsigned_32 := 16#0001_0002#;

   Streams : CCL_Stream_Table.Table;

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      CCL_Config_Bindings.Install (Catalog, Grants, Success);
      if Success then CCL_Execution.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Log_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Image_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Image_Loading.Install (Catalog, Grants, Success); end if;
      if Success then CCL_File_Bindings.Install (Catalog, Grants, Success); end if;
      if Success then CCL_Process_Bindings.Install (Catalog, Grants, Success); end if;
      if not Success then return; end if;
      CCL.Interfaces.Clock.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms (Catalog, Resolved, Found);
      Success := Found;
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, CLOCK_BINDING, Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
      if not Success then return; end if;
      CCL.Interfaces.Timer.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Interfaces.Timer.Resolve_Every (Catalog, Resolved, Found);
      Success := Found;
      if not Success then return; end if;
      CCL.Catalog.Install (Grants, Resolved, TIMER_EVERY_BINDING, Grant);
      Success := Grant = CCL.Catalog.Grant_Added;
   end Install;

   function Handles (Binding : Interfaces.Unsigned_32) return Boolean is
     (Binding in CLOCK_BINDING | TIMER_EVERY_BINDING or else CCL_Config_Bindings.Handles (Binding) or else
      CCL_Log_Bindings.Handles (Binding) or else CCL_Image_Bindings.Handles (Binding) or else
      CCL_Image_Loading.Handles (Binding) or else CCL_File_Bindings.Handles (Binding) or else
      CCL_Process_Bindings.Handles (Binding));

   CLOCK_UNAVAILABLE : constant CuBit.Failures.Failure := CuBit.Failures.Failed
     (CuBit.Failures.Unavailable, "the system clock did not answer",
      "the program's manifest must request the clock service (request-service clock read-write clock)");

   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Available : Boolean;
      Milliseconds : Interfaces.Unsigned_64;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if CCL_Config_Bindings.Handles (Binding) then
         CCL_Config_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Log_Bindings.Handles (Binding) then
         CCL_Log_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Image_Bindings.Handles (Binding) then
         CCL_Image_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Image_Loading.Handles (Binding) then
         CCL_Image_Loading.Invoke (Binding, Argument, Reply);
      elsif CCL_File_Bindings.Handles (Binding) then
         CCL_File_Bindings.Invoke (Binding, Argument, Reply);
      elsif CCL_Process_Bindings.Handles (Binding) then
         CCL_Process_Bindings.Invoke (Binding, Argument, Reply);
      elsif Binding = CLOCK_BINDING and then
        Argument.Kind = CCL.Host_Values.Integer_Value and then Argument.Integer = 0
      then
         --  clock.monotonic-ms takes the Integer zero of a parameterless call.
         Milliseconds := Monotonic_Ms (Available);
         Reply.Success := Available and then
           Milliseconds <= Interfaces.Unsigned_64 (Interfaces.Integer_64'Last);
         if Reply.Success then
            Reply.Value := CCL.Host_Values.Integer_Constant (Interfaces.Integer_64 (Milliseconds));
         else
            Reply.Why := CLOCK_UNAVAILABLE;
         end if;
      elsif Binding = TIMER_EVERY_BINDING and then Argument.Kind = CCL.Host_Values.Integer_Value and then
        Argument.Integer not in CCL_Stream_Table.MIN_PERIOD_MS .. CCL_Stream_Table.MAX_PERIOD_MS
      then
         Reply.Why := CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument,
            "a period of" & Interfaces.Integer_64'Image (Argument.Integer) & " ms is outside" &
            Natural'Image (CCL_Stream_Table.MIN_PERIOD_MS) & " ms .." &
            Natural'Image (CCL_Stream_Table.MAX_PERIOD_MS) & " ms",
            "choose a period from a hundred ticks a second to one an hour");
      elsif Binding = TIMER_EVERY_BINDING and then Argument.Kind = CCL.Host_Values.Integer_Value then
         --  timer.every: a new stream in the session's table.
         Milliseconds := Monotonic_Ms (Available);
         if not Available then
            Reply.Why := CLOCK_UNAVAILABLE;
         else
            declare
               Handle : CCL.Streams.Handle;
            begin
               CCL_Stream_Table.Open_Timer
                 (Streams, CCL_Stream_Table.Period_Ms (Argument.Integer), Milliseconds, Handle);
               Reply.Success := CCL.Streams."/=" (Handle, CCL.Streams.No_Handle);
               if Reply.Success then
                  Reply.Value := CCL.Host_Values.Integer_Constant (Interfaces.Integer_64 (Handle));
               else
                  Reply.Why := CuBit.Failures.Failed
                    (CuBit.Failures.Exhausted,
                     "this session already has" & Natural'Image (CCL_Stream_Table.MAX_STREAMS) &
                     " streams open",
                     "rebind a name that holds a stream, or :reset the session");
               end if;
            end;
         end if;
      end if;
   end Invoke;

   procedure Read_Stream
     (Request : CCL.Streams.View_Request; Reply : in out CCL.Streams.View_Reply) is
   begin
      CCL_Stream_Table.Read (Streams, Request, Reply);
   end Read_Stream;

   procedure Pump_Streams (Arrived : out Boolean) is
      Available : Boolean;
      Now : constant Interfaces.Unsigned_64 := Monotonic_Ms (Available);
   begin
      Arrived := False;
      if Available then
         CCL_Stream_Table.Pump (Streams, Now, Arrived);
      end if;
   end Pump_Streams;

   procedure Retain_Streams is
      procedure Retain is new CCL_Stream_Table.Retain (Held);
   begin
      Retain (Streams);
   end Retain_Streams;

   procedure Reset_Streams is
   begin
      CCL_Stream_Table.Clear (Streams);
   end Reset_Streams;

   function Open_Streams return Natural is (CCL_Stream_Table.Open_Count (Streams));

   function Next_Stream_Delay return Interfaces.Unsigned_64 is
      Available : Boolean;
      Now : constant Interfaces.Unsigned_64 := Monotonic_Ms (Available);
      Due : constant Interfaces.Unsigned_64 := CCL_Stream_Table.Next_Due (Streams);
   begin
      if Due = Interfaces.Unsigned_64'Last or else not Available then
         return Interfaces.Unsigned_64'Last;
      end if;
      return (if Now >= Due then 0 else Due - Now);
   end Next_Stream_Delay;
end CCL_Host_Environment;
