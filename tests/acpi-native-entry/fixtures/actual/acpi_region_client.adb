pragma Ada_2022;
with AML_Identity.Issuer;
package body ACPI_Region_Client with SPARK_Mode is
   use ACPI_Region_Protocol;
   function State_Of (IO : Context) return State_View is
     ((IO.Installed, IO.Identity, IO.Destination, IO.High_Water,
       IO.In_Flight, IO.Captured, IO.Confirmed));
   function Internal_Coherent (IO : Context) return Boolean is
     ((if IO.Installed then IO.Identity /= AML_Identity.No_Identity
         and then IO.Destination /= No_Endpoint
       else IO.Identity = AML_Identity.No_Identity and then IO.Destination = No_Endpoint
         and then IO.High_Water = No_Request and then IO.Captured = No_Observation
         and then not IO.In_Flight and then not IO.Confirmed)
      and then (if IO.Captured.Present then IO.Installed
        and then IO.Captured.Owner = IO.Identity and then IO.Captured.Operation = IO.High_Water
        and then IO.Captured.Operation /= No_Request and then Valid_Request (IO.Captured.Item)
       else IO.Captured = No_Observation and then not IO.In_Flight and then not IO.Confirmed)
      and then (if IO.Confirmed then IO.Captured.Present and then not IO.In_Flight));
   function Coherent (IO : Context) return Boolean is (Internal_Coherent (IO));
   function Dispatchable
     (IO : Context; Now, Deadline : Time_Instant; Item : Register_Request) return Boolean is
     (IO.Installed and then not IO.In_Flight and then not IO.Captured.Present
      and then IO.High_Water < Request_ID'Last and then Deadline > Now
      and then Deadline /= No_Finite_Deadline and then Valid_Request (Item));
   function Initialize_Transition
     (Before, After : State_View; Destination : Endpoint; Accepted : Boolean) return Boolean is
      (if Accepted then not Before.Installed and then Destination /= No_Endpoint
        and then After.Identity /= AML_Identity.No_Identity
        and then After = (Before with delta Installed => True,
          Identity => After.Identity, Destination => Destination)
        else After = Before);
   function Dispatch_Transition
     (Before, After : State_View; Now, Deadline : Time_Instant;
      Item : Register_Request; Reply : Result) return Boolean is
     ((if Before.Installed and then not Before.In_Flight
        and then not Before.Captured.Present and then Before.High_Water < Request_ID'Last
        and then Deadline > Now and then Deadline /= No_Finite_Deadline and then Valid_Request (Item)
        then Reply.Operation = Before.High_Water + 1
        and then After = (Before with delta High_Water => Before.High_Water + 1,
          In_Flight => False, Confirmed => False,
          Captured => (if Reply.Status = Indeterminate then Observation'
            (Present => True, Owner => Before.Identity, Operation => Reply.Operation, Item => Item)
            else No_Observation))
        else After = Before and then Reply = Result'(others => <>))
        and then (if Reply.Status /= Completed or else Item.For_Write then Reply.Value = 0));
   function Confirm_Transition
     (Before, After : State_View; Expected : Observation; Evidence, Accepted : Boolean) return Boolean is
     (Accepted = (not Before.In_Flight and then Evidence
        and then Expected.Present and then Expected = Before.Captured)
        and then After = (if Accepted then (Before with delta Confirmed => True) else Before));
   function Retire_Transition
     (Before, After : State_View; Expected : Observation; Consumed, Accepted : Boolean) return Boolean is
     (Accepted = (Consumed and then not Before.In_Flight and then Before.Confirmed
        and then Expected.Present and then Expected = Before.Captured)
        and then After = (if Accepted then
          (Before with delta Captured => No_Observation, Confirmed => False) else Before));
   procedure Initialize
     (IO : in out Context; Destination : Endpoint; Accepted : out Boolean) is
      Issued : Boolean;
   begin
      Accepted := False;
      if IO.Installed or else Destination = No_Endpoint then return; end if;
      AML_Identity.Issuer.Issue (IO.Identity, Issued);
      if not Issued then return; end if;
      IO.Destination := Destination;
      IO.Installed := True;
      Accepted := True;
   end Initialize;
   function Pending (IO : Context) return Boolean is
     (IO.In_Flight or else (IO.Captured.Present and then not IO.Confirmed));
   function Uncertain (IO : Context) return Observation is
     (if Pending (IO) then IO.Captured else No_Observation);
   function Reconciled (IO : Context; Expected : Observation) return Boolean is
     (not IO.In_Flight and then IO.Confirmed and then Expected.Present
      and then Expected = IO.Captured);
   procedure Dispatch
     (IO : in out Context; Transport : in out Transport_State;
      Now, Deadline : Time_Instant; Item : Register_Request; Reply : out Result) is
      Packet_Reply : Packet;
      Delivered : Delivery;
   begin
      Reply := (others => <>);
      if not IO.Installed or else IO.In_Flight or else IO.Captured.Present
        or else IO.High_Water = Request_ID'Last or else Deadline <= Now
        or else Deadline = No_Finite_Deadline
        or else not Valid_Request (Item)
      then return; end if;
      IO.High_Water := IO.High_Water + 1;
      -- Reserve the exact receipt before any possible external effect.
      IO.Captured := (Present => True, Owner => IO.Identity,
        Operation => IO.High_Water, Item => Item);
      IO.In_Flight := True;
      Reply.Operation := IO.High_Water;
      Exchange (Transport, IO.Destination, Deadline, Encode (Item), Packet_Reply, Delivered);
      IO.In_Flight := False;
      Reply.Status := Indeterminate;
      case Delivered is
         when Not_Dispatched => Reply.Status := Rejected;
         when Completion_Unknown => return;
         when Reply_Received =>
            declare
               Decoded : constant Decoded_Reply := Decode (Packet_Reply, Item.For_Write);
            begin
               if not Decoded.Valid then return; end if;
               case Decoded.Status is
                  when Denied | Malformed => Reply.Status := Rejected;
                  when Done => Reply.Status := Completed; Reply.Value := Decoded.Value;
                  when Backend_Fault => Reply.Status := Failed_Completed;
                  when ACPI_Region_Protocol.Indeterminate => return;
               end case;
            end;
      end case;
      IO.Captured := No_Observation;
   end Dispatch;
   procedure Confirm_Completion
     (IO : in out Context; Expected : Observation;
      Terminal_Evidence : Boolean; Accepted : out Boolean) is
   begin
      Accepted := not IO.In_Flight and then Terminal_Evidence and then Expected.Present
        and then Expected = IO.Captured;
      if Accepted then IO.Confirmed := True; end if;
   end Confirm_Completion;
   procedure Retire_Confirmation
     (IO : in out Context; Expected : Observation;
      Consumer_Reconciled : Boolean; Accepted : out Boolean) is
   begin
      Accepted := Consumer_Reconciled and then Reconciled (IO, Expected);
      if Accepted then
         IO.Captured := No_Observation;
         IO.Confirmed := False;
      end if;
   end Retire_Confirmation;
end ACPI_Region_Client;
