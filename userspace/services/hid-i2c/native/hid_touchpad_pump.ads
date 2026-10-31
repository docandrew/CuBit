pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with HID_Touchpad_Native;
with HID_Touchpad_Publication;
generic
   with package Native is new HID_Touchpad_Native (<>);
   with package Output is new HID_Touchpad_Publication.Publisher
     (App => Native.App, others => <>);
   Rounds_Per_Turn : Positive;
   with procedure Sample_Time (Now : out Native.W.Word; Available : out Boolean);
   -- Called after monotonicity checking; updates Reset.Clock's cache to the
   -- exact time/availability passed to all state machines in this turn.
   with procedure Cache_Time (Now : Native.W.Word; Available : Boolean);
   with procedure Poll_Completion (Item : out CuBit.Messages.CompletionEntry;
      Found : out Boolean);
   with procedure Poll_Event (Publisher : out CuBit.Process_IDs.Process_ID;
      Item : out CuBit.Messages.Message; Found : out Boolean);
   -- Consumes at most one service request and disposes of its reply/offered
   -- capability. Must not wait, call a peer, or leave dequeued resources owned.
   with procedure Drain_Request (Found : out Boolean);
   -- Handles authenticated kernel lifecycle/channel/grant notices. False lets
   -- the native GPIO decoder inspect the event. No independent clock reads.
   with procedure System_Event (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean);
package HID_Touchpad_Pump is
   package W renames Native.W;
   type Wait_Kind is (Immediate, Activity, Until_Time, Clock_Backoff);
   type Report is record
      Wait_For : Wait_Kind := Clock_Backoff;
      Deadline : W.Word := 0;
      Completions, Events, Requests : Natural := 0;
      Clock_Available, Stopping, Ready, Quiescent : Boolean := False;
      Publication : Output.Report;
   end record;
   No_Startup_Deadline : constant W.Word := W.Word'Last;
   procedure Turn (Startup_Deadline : W.Word; Result : out Report);
   -- Explicit outer startup cutoff, checked after sampling/cache normalization
   -- but BEFORE dispatch/advancement. No_Startup_Deadline disables this cutoff
   -- after readiness selection; individual operation deadlines still apply.
   -- One cached time per turn; each round polls each queue at most once and
   -- advances the shared device/owner once. A saturated turn never sleeps.
   -- Clock failure/reversal latches stop, never fabricates time or cancellation.
   -- Caller implements Clock_Backoff with an explicit bounded relative delay.
private
   Last_Time : W.Word := 0;
   Seen_Time, Stopping : Boolean := False;
end HID_Touchpad_Pump;
