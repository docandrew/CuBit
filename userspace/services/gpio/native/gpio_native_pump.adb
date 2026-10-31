pragma Ada_2022;
with CuBit.Messages;
with CuBit.Channel_Bells;
with GPIO_Admin_Wire;
with GPIO_Event_Policy;
with GPIO_Native_Clock;
package body GPIO_Native_Pump is
   package M renames CuBit.Messages;
   package I renames Driver.Controller.I;
   package A renames GPIO_Admin_Wire;
   package EP renames GPIO_Event_Policy;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_16;
   use type M.Protocol_Flags, M.IPC_Flags, I.Tick;
   Counts : Statistics;
   function Observe return Statistics is (Counts);
   procedure Increment (Value : in out Interfaces.Unsigned_64) is
   begin if Value < Interfaces.Unsigned_64'Last then Value := Value + 1; end if; end Increment;
   function Deadline (Now, Window : I.Tick) return I.Tick is
      Last_Finite : constant I.Tick := I.Tick'Last - 1;
   begin
      if Window > Last_Finite or else Now >= Last_Finite or else Window > Last_Finite - Now
      then return Last_Finite; end if;
      return Now + Window;
   end Deadline;
   procedure Limits (Setup, Cleanup : out I.Tick) is
      Now : I.Tick;
      Available : Boolean;
   begin
      GPIO_Native_Clock.Read (Now, Available);
      Setup := 0; Cleanup := 0;
      if Available then Setup := Deadline (Now, Setup_Window); Cleanup := Deadline (Now, Cleanup_Window); end if;
   end Limits;
   procedure Poll_Event (S : in out Driver.Admin.State; Found : out Boolean) is
      Event : M.Message;
      Publisher : M.Process_ID;
      Action : EP.Action := EP.Ignore;
      Noted : Boolean;
      Setup, Cleanup : I.Tick;
   begin
      Found := M.Poll_Event (Event, Publisher); if not Found then return; end if;
      Increment (Counts.Events);
      if Event.tag.protocol = 0 and then Event.tag.ipc = M.No_IPC_Flags and then Event.tag.reserved = 0 then
         Action := EP.Classify (Publisher, Event.tag.label, Event.tag.length,
            Event.words (0), Event.words (1), Event.words (2));
      end if;
      case Action is
         when EP.Route_Channel_Wake =>
            CuBit.Channel_Bells.Note_Notice (Event.tag.label, Event.tag.length,
               [Event.words (0), Event.words (1), Event.words (2), Event.words (3)], Noted);
            if not Noted then Increment (Counts.Ignored_Events); end if;
         when EP.Disable_Logging => Disable_Logging ("grant-lifetime-event");
         when EP.Stop_After_Drain =>
            Limits (Setup, Cleanup);
            Driver.Registry.Request_Stop (Cleanup); Driver.Admin.Request_Stop (S);
         when EP.Ignore => Increment (Counts.Ignored_Events);
      end case;
   end Poll_Event;
   procedure Poll_Request (S : in out Driver.Admin.State; Found : out Boolean) is
      Caller : M.Process_ID;
      Message : M.Message;
      Result : Driver.Client.Report;
      Setup, Cleanup : I.Tick;
   begin
      M.Poll_Service_Request (Caller, Message, Found); if not Found then return; end if;
      Increment (Counts.Requests);
      if Message.tag.label in A.OP_GPIO_ADMIT_SINK | A.OP_GPIO_RETIRE_SINK then
         Limits (Setup, Cleanup);
         Driver.Native_Admin.Handle (S, Caller, Message, Setup, Cleanup);
      else
         Driver.Native_Client.Handle (Caller, Message, Result);
      end if;
   end Poll_Request;
end GPIO_Native_Pump;
