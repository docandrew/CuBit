with Interfaces; use Interfaces;
with CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
with Intel_GPU_Broker_Launches;
with Intel_GPU_Render_Control;
with Intel_GPU_Render_Sessions;
with Intel_Render_Broker;
with Intel_Render_Launch_Client;
package body GPU_Launch_Probe is
   use CuBit.Messages;
   package G renames CuBit.Capability_Grants;
   package R renames Intel_GPU_Broker_Request;
   package L renames Intel_GPU_Broker_Launches;
   package GPU renames Intel_GPU_Render_Control;
   use type L.Phase;
   function Reply_Slot (ID : Positive) return CapabilitySlot is (16);
   package B is new Intel_Render_Broker (31, 16#BC10#, 16#BC40#, Reply_Slot);
   Broker : B.Broker;
   Controller : GPU.Controller;
   package C is new Intel_Render_Launch_Client (61, 16#BC50#, 16#BC60#);
   Launcher : C.Launcher;
   use type C.Phase;

   procedure Client is
      Child : constant G.Recipient := G.Capture (37);
      ID : C.Ticket;
      Receipt : aliased CompletionEntry;
      Used : Boolean;
      Activity : Activity_Result;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 5_000;
   begin
      -- Earlier admission probes retain GPU recipient slots40..44. Consume
      -- failed reservations using a non-grantable self endpoint, so this
      -- synthetic co-located broker uses55 without replacing those caps.
      for I in 1 .. 15 loop
         C.Start (Launcher, True, Child, 0, CapabilitySlot (I - 1), ID);
         if ID /= I or else C.State (Launcher, ID) /= C.Rejected then
            debugPrint ("GPU-LAUNCH-IPC FAIL negative delegation" & ASCII.LF);
            return;
         end if;
      end loop;
      C.Start (Launcher, True, Child, 37, 38, ID);
      while C.State (Launcher, ID) = C.Pending loop
         if syscall (SYSCALL_GETTIME) >= Deadline then
            debugPrint ("GPU-LAUNCH-IPC FAIL watchdog" & ASCII.LF);
            return;
         end if;
         if Poll_Completion (Receipt'Address) = 1 then
            C.Complete (Launcher, Receipt, Used);
            if not Used then
               debugPrint ("GPU-LAUNCH-IPC FAIL unexpected receipt" & ASCII.LF);
               return;
            end if;
         else
            Activity := Wait_For_Activity_Until (Deadline);
         end if;
      end loop;
      if C.State (Launcher, ID) = C.Admitted and then
        G.Endpoint_Matches (38, G.Incarnation (G.Capture (61))) and then
        G.Delegate_Endpoint (Child, 38, 39, 1, 0) /= 0
      then
         debugPrint ("GPU-LAUNCH-IPC PASS saved reply and reciprocal admission" & ASCII.LF);
      else
         debugPrint ("GPU-LAUNCH-IPC FAIL result" & ASCII.LF);
      end if;
   end Client;

   procedure Server (Sender : ProcessID; Request : Message) is
      ID : B.Ticket;
      Receipt : aliased CompletionEntry;
      Used, Found : Boolean;
      From : ProcessID;
      Msg, Response : Message := NULL_MESSAGE;
      Payload : GPU.Words;
      Identity, Ignore : Unsigned_64;
      Self : constant Unsigned_64 := syscall (SYSCALL_GETPID);
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 4_000;
      Activity : Activity_Result;
   begin
      -- Fixture boot policy supplies self control31 and broad CSPACE58.
      -- Only this synthetic test binds its launcher from the first request.
      GPU.Bind (Controller, Self, 16#BC01#);
      -- This synthetic GPU shares the server CSPACE with earlier probes.
      -- Reserve (but never activate) their five occupied recipient indices;
      -- our actual admission then derives into the fresh slot45.
      for I in 1 .. 5 loop
         GPU.Handle (Controller, Self, 16#BC01#, True, GPU.Label, 4, 0, 0,
           [1, G.Incarnation (G.Capture (31)), 0, 0], Payload);
      end loop;
      B.Begin_Request (Broker, Sender, Sender, Request.authorityTag, Request,
                       syscall (SYSCALL_GETTIME), Deadline, ID);
      if ID = 0 then
         debugPrint ("GPU-LAUNCH-IPC FAIL save/auth" & ASCII.LF);
         Ignore := reply (Sender, NULL_MESSAGE);
         return;
      end if;
      while B.State (Broker, ID) in L.Pending | L.Reply_Ready loop
         if syscall (SYSCALL_GETTIME) >= Deadline + 500 then
            debugPrint ("GPU-LAUNCH-IPC FAIL server watchdog" & ASCII.LF);
            return;
         end if;
         if Poll_Completion (Receipt'Address) = 1 then
            B.Complete (Broker, Receipt, syscall (SYSCALL_GETTIME), Used);
         end if;
         B.Step (Broker, syscall (SYSCALL_GETTIME));
         Poll_Service_Request (From, Msg, Found);
         if Found then
            Identity := GPU.Activation_Identity (Controller, From,
              Msg.authorityTag, Msg.tag.label, Msg.tag.length, Msg.tag.flags,
              Msg.tag.reserved, [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)]);
            GPU.Handle (Controller, From, Msg.authorityTag, True,
              Msg.tag.label, Msg.tag.length, Msg.tag.flags, Msg.tag.reserved,
              [Msg.words (0), Msg.words (1), Msg.words (2), Msg.words (3)], Payload,
              Recipient_Ready => Identity /= 0 and then Msg.words (2) >
                Intel_GPU_Render_Sessions.Tag_Base and then Msg.words (2) <=
                Intel_GPU_Render_Sessions.Tag_Base + 16 and then
                G.Endpoint_Matches (CapabilitySlot (39 + Msg.words (2) -
                  Intel_GPU_Render_Sessions.Tag_Base), Identity));
            Response.tag := Msg.tag;
            Response.words := [Payload (0), Payload (1), Payload (2), Payload (3)];
            Ignore := reply (From, Response);
         elsif B.State (Broker, ID) = L.Pending and then not B.Runnable (Broker) then
            Activity := Wait_For_Activity_Until (Deadline + 500);
         end if;
      end loop;
   end Server;
end GPU_Launch_Probe;
