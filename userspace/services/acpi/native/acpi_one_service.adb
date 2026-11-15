pragma Ada_2022;
with ACPI_Power_Retirement_Replies; with ACPI_Endpoint;
with CuBit.Messages;
with CuBit.Monotonic;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Kernel_Handles;
with ACPI_Platform_Pool_Native;
with ACPI_SCI_Events;
with ACPI_SCI_Quiesce;
with ACPI_Native_Events;
with ACPI_Native_Power_Entry;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Button_Discovery;
with ACPI_Native_Pre_Device;with ACPI_Native_Regions;
with ACPI_Region_Stop_Policy;with ACPI_System_Events;with ACPI_Native_Log;
package body ACPI_One_Service is
   package M renames CuBit.Messages;
   package B renames ACPI_Backend_Binding_Types;
   use type M.Activity_Result, Runtime.Status.Phase, Runtime.Status.Outcome;

   -- Copied once by Configure_Hardware; the handles inside stay owned by the
   -- root (Root.Evaluation keeps its own copy). Only page ranges are read here.
   Saved_Grant : CuBit.ACPI_Windows.Grant;
   Grant_Saved : Boolean := False;
   Setup_Until : Word := 0;          -- GETTIME milliseconds; 0 = closed
   Consumers_Closed : Boolean := False;
   Authorized_Stop_Seen : Boolean := False;
   Final_Core_View : Core_Retirement_State := Unobserved;
   Final_SCI_View : SCI_Retirement_State := SCI_Unobserved;

   function Compatible_Envelope (First_Page, Pages : Word) return Boolean is
   begin
      if not Grant_Saved or else Pages = 0
        or else First_Page > Memory_Classes.Page_Limit
        or else Pages > Memory_Classes.Page_Limit - First_Page
      then
         return False;
      end if;
      for I in 1 .. Saved_Grant.Memory_Used loop
         declare
            W : CuBit.ACPI_Windows.Memory_Window renames Saved_Grant.Memory (I);
         begin
            -- Half-open page ranges overlap iff each starts before the other ends.
            if First_Page < W.First_Page + W.Pages and then W.First_Page < First_Page + Pages then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Compatible_Envelope;

   procedure Begin_Window (Deadline_Microseconds : out Word;
      Budget : out ACPI_Global_Lock_Protocol.Attempt_Budget; Available : out Boolean) is
      Now : B.Tick;
      Good : Boolean;
   begin
      Deadline_Microseconds := 0;
      Budget := ACPI_Global_Lock_Protocol.Attempt_Budget'Last;
      Available := False;
      -- The same clock Hardware's Global Lock owner reads (lock clock).
      ACPI_Backend_Kernel_Handles.Read_Clock (Now, Good);
      if not Good or else Word (Now) >= Word'Last - Field_Lease_Microseconds then
         return;
      end if;
      Deadline_Microseconds := Word (Now) + Field_Lease_Microseconds;
      Available := True;
   end Begin_Window;

   procedure PCI_Setup_Deadline (Deadline_Milliseconds : in out Word;
      Available : in out Boolean) is
      Now : constant Word := CuBit.Monotonic.Milliseconds;
   begin
      if Setup_Until /= 0 and then Now < Setup_Until then
         Deadline_Milliseconds := Setup_Until; Available := True;
      else
         Deadline_Milliseconds := 0; Available := False;
      end if;
   end PCI_Setup_Deadline;

   procedure Close_Consumers (Accepted : out Boolean) is
   begin
      Accepted := Consumers_Closed;
   end Close_Consumers;

   procedure Other_Owners_View (Quiescent : out Boolean;
      Wake_Deadline_Milliseconds : out Word) is
   begin
      Quiescent := ACPI_Native_Button_Discovery.Quiet and then ACPI_Native_Power_Buttons.Quiet and then ACPI_Native_Power_Entry.Quiet and then Consumers_Closed and then Final_Core_View in No_Core|Clear_Core
        and then Final_SCI_View in Not_Armed|Clear_SCI
        and then Runtime.Current = Runtime.Status.Stopped
        and then Runtime.Observation /= Runtime.Status.Stop_Pending;
      Wake_Deadline_Milliseconds := M.Wait_Forever;
   end Other_Owners_View;

   procedure Configure_Hardware
     (Input : CuBit.ACPI_Windows.Grant;
      Hardware_Deadline_Microseconds : Word;
      Setup_Deadline_Milliseconds : Word;
      Pool : Root.Backing.Pool_Configuration;
      PCI : Root.Backing.PCI_Configuration;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Grant_Saved or else Setup_Deadline_Milliseconds = 0
        or else Setup_Deadline_Milliseconds = M.Wait_Forever then
         return;
      end if;
      Saved_Grant := Input; Grant_Saved := True;
      Setup_Until := Setup_Deadline_Milliseconds;
      Root.Evaluation.Configure
        (Input, Root.Hardware.B.Tick (Hardware_Deadline_Microseconds), Pool, PCI,
         Field_Symbol, Accepted);
   end Configure_Hardware;

   procedure Finish_Startup (Accepted : out Boolean) is
   begin
      Setup_Until := 0;
      Root.Evaluation.Finish_Startup (Accepted);
   end Finish_Startup;

   procedure Mark_Consumers_Closed is
   begin
      Consumers_Closed := True;
      Setup_Until := 0;
   end Mark_Consumers_Closed;

   procedure Serving_Maintenance (Wake_Deadline_Milliseconds : out Word;
      May_Continue : out Boolean) is
      Hardware_Wake : Word;
   begin
      Root.Maintain_Owner (Hardware_Wake, May_Continue);
      -- Other root-owned serving deadlines join here with Earliest, in the
      -- same kernel wait-clock domain. None exists in this cohort; the
      -- ACPI-FIELD-WINDOW follow-up adds the delay window's deadline.
      Wake_Deadline_Milliseconds := Hardware_Wake;
      May_Continue := May_Continue and then Runtime.Current = Runtime.Status.Idle;
   end Serving_Maintenance;

   -- Dispatch has stopped: send typed refusals and drain late events, so a
   -- queued message cannot turn the next wait into a spin.
   function Region_Stop_Evidence (Authenticated : Boolean) return ACPI_Region_Stop_Policy.Evidence is
      View : constant Root.Shutdown.Control.Report := Root.Shutdown.Observe;
      Regions : constant ACPI_Native_Regions.Report := ACPI_Native_Regions.Observe;
      use type Root.Shutdown.Control.Phase;
   begin
      return [ACPI_Region_Stop_Policy.Authenticated_Stop => Authenticated,
         ACPI_Region_Stop_Policy.Region_State_Retained => Final_Core_View = Retained_Regions,
         ACPI_Region_Stop_Policy.Shutdown_Closed => View.Stage = Root.Shutdown.Control.Retained and then not View.Working
           and then View.Consumers_Closed and then View.Local_Closed and then View.Hardware_Closing,
         ACPI_Region_Stop_Policy.SCI_Quiesced => Final_SCI_View in Not_Armed | Clear_SCI,
         ACPI_Region_Stop_Policy.Runtime_Stopped => Runtime.Current = Runtime.Status.Stopped
           and then Runtime.Observation /= Runtime.Status.Stop_Pending,
         ACPI_Region_Stop_Policy.Fields_Quiet => View.Fields_Cleaned and then Root.Fields.Control_Idle
           and then not Root.Fields.Uncertain.Present,
         ACPI_Region_Stop_Policy.EC_Quiet => Root.Fields.EC_Commands.Quiescent,
         ACPI_Region_Stop_Policy.Hardware_Quiet => View.Hardware_Idle and then Root.Hardware.May_Exit
           and then not Root.Hardware.Pending,
         ACPI_Region_Stop_Policy.Backing_Quiet => Root.Backing_Quiet,
         ACPI_Region_Stop_Policy.Other_Actors_Quiet => Consumers_Closed and then ACPI_Native_Pre_Device.Quiet
           and then ACPI_Native_Button_Discovery.Quiet and then ACPI_Native_Power_Buttons.Quiet
           and then ACPI_Native_Power_Entry.Quiet,
         ACPI_Region_Stop_Policy.Region_Work_Quiet => not Regions.Working and then not Regions.Pending];
   end Region_Stop_Evidence;
   procedure Release_Region_State_On_Stop is
      Facts : constant ACPI_Region_Stop_Policy.Evidence := Region_Stop_Evidence(Authorized_Stop_Seen);
   begin
      if not ACPI_Region_Stop_Policy.Allowed(Facts) then return;end if;
      ACPI_Native_Log.Copied
        ("acpi: stage replacement-exit ok REGION_STATE_DISCARDED sci="&Final_SCI_View'Image&
         " physical=QUIET reg0=NOT_RUN resources=KERNEL_EXIT",False,True);
      raise Region_State_Released_On_Stop;
   end Release_Region_State_On_Stop;
   procedure Refuse_Pending (Retained : Boolean := False) is
      Request, Event, Response : M.Message;
      package Replies renames ACPI_Power_Retirement_Replies;
      Press_Count : Replies.Word;
      Source : Replies.E.Press_Source;
      From, Publisher : M.Process_ID;
      Found : Boolean;
      Ignore : Word;
      Ack : ACPI_SCI_Quiesce.Acknowledgement;
      use type ACPI_SCI_Quiesce.Acknowledgement, ACPI_System_Events.Action;
   begin
      ACPI_Native_Power_Buttons.Observe_Presses (Press_Count, Source);
      for I in 1 .. Retirement_Drain_Budget loop
         exit when not M.Poll_Event (Event, Publisher);
         if ACPI_System_Events.Classify(Publisher,Event)=ACPI_System_Events.Stop_Requested then
            Authorized_Stop_Seen:=True;
         end if;
         if Retained then
            Release_Region_State_On_Stop;
            if ACPI_System_Events.Classify(Publisher,Event)=ACPI_System_Events.Stop_Requested then
               declare
                  Facts : constant ACPI_Region_Stop_Policy.Evidence := Region_Stop_Evidence(True);
                  Why : constant ACPI_Region_Stop_Policy.Refusal := ACPI_Region_Stop_Policy.First_Missing(Facts);
               begin
                  ACPI_Native_Events.Note_Retained_Stop(Publisher,Event,
                    "core="&Final_Core_View'Image&" sci="&Final_SCI_View'Image&
                    " blocked="&(if Why.Missing then Why.Item'Image else "STATE_CHANGED"));
               end;
            end if;
            ACPI_Native_Events.Note_Retained_Stop (Publisher, Event,
              "core=" & Final_Core_View'Image & " sci=" & Final_SCI_View'Image &
              " runtime=" & Runtime.Current'Image);
         end if;
         -- Dispatch has stopped. Keep a retained old generation from holding
         -- a shared SCI line masked; no consumer or AML is called. The sole
         -- optional copied note above never opens a logging channel.
         ACPI_SCI_Events.Acknowledge_Only (Publisher,Event,Ack);
         if Ack = ACPI_SCI_Quiesce.Acknowledge_Refused then
            Final_SCI_View := Retained_SCI;
         end if;
      end loop;
      for I in 1 .. Retirement_Drain_Budget loop
         M.Poll_Service_Request (From, Request, Found);
         exit when not Found;
         declare
            Reply : constant Replies.Response := Replies.Refuse
              ((Label => Request.tag.label, Length => Request.tag.length,
                Flags => M.Flag_Byte (Request.tag), Reserved => Request.tag.reserved,
                Authority => Request.authorityTag, Data => Replies.Words (Request.words)),
               Press_Count, Source);
         begin
            Response := M.NULL_MESSAGE;
            if Reply.Handled then
               Response.tag.label := Reply.Label; Response.tag.length := Reply.Length;
               Response.words := M.MessageWords (Reply.Data);
            else
               -- No Core/revision inspection is possible during retirement.
               -- The service exists but cannot admit a new observer operation.
               declare Error : constant ACPI_Requests.Packet := ACPI_Endpoint.Encode
                 ((Status => ACPI_Requests.Wrong_Order, Data => [others => 0]));
               begin
                  Response.tag.label := Error.Label; Response.tag.length := Error.Length;
                  Response.words := M.MessageWords (Error.Data);
               end;
            end if;
            Ignore := M.reply (From, Response);
         end;
      end loop;
   end Refuse_Pending;

   procedure Park is
      Activity : M.Activity_Result;
      Yielded : Word;
      pragma Unreferenced (Yielded);
   begin
      loop
         -- Stop may have arrived during the preceding retirement drain.
         -- Its authenticated provenance survives that transition to Park.
         Release_Region_State_On_Stop;
         Refuse_Pending (Retained => True);
         Activity := M.Wait_For_Activity_Until (M.Wait_Forever);
         if Activity = M.Unavailable then
            Yielded := M.syscall (M.SYSCALL_YIELD);
         end if;
      end loop;
   end Park;

   procedure Begin_Retirement (Core_View : Core_Retirement_State;
      SCI_View : SCI_Retirement_State) is
      Accepted : Boolean;
      Runtime_Stop : Runtime.Status.Outcome;
   begin
      Mark_Consumers_Closed;
      Final_Core_View := Core_View;
      Final_SCI_View := SCI_View;
      Runtime.Request_Stop (Runtime_Stop);
      if Runtime_Stop not in Runtime.Status.Stopped_By_Owner|Runtime.Status.Already_Stopped then
         Final_Core_View := Unobserved;
      end if;
      -- Fresh/unclaimed startup failures still own their startup delay domain.
      -- Stop is idempotent and never cancels an outstanding wait.
      ACPI_Native_Delays.Close_Startup;
      Root.Shutdown.Request_Stop (Accepted);
   end Begin_Retirement;
   procedure Try_Retire (Core_View : Core_Retirement_State;
      SCI_View : SCI_Retirement_State; Retired : out Boolean) is
   begin
      Begin_Retirement(Core_View,SCI_View);
      -- Exactly one local retirement advance. Never poll, drain or wait on
      -- incoming service/control traffic before registration/serving.
      Root.Shutdown.Advance;
      Retired:=Root.Shutdown.May_Retire;
   end Try_Retire;
   procedure Retire (Core_View : Core_Retirement_State;
      SCI_View : SCI_Retirement_State; Retired : out Boolean) is
      package S renames Root.Shutdown;
      use type S.Control.Phase;
      use type ACPI_Platform_Pool_Native.Owner.Phase;
      Next_Disposal : Word := 0;
      Completed : Boolean;
      Wake : Word;
      Activity : M.Activity_Result;
   begin
      Retired := False;
      Begin_Retirement(Core_View,SCI_View);
      -- Refused or repeated stop admission keeps the owner exactly as is;
      -- Advance below only observes and the loop ends as retained.
      loop
         S.Advance;
         if S.May_Retire then
            Retired := True; return;
         end if;
         declare
            View : constant S.Control.Report := S.Observe;
            Now : constant Word := CuBit.Monotonic.Milliseconds;
            Pool_Refused : constant Boolean :=
              Root.Backing.Observe.Pool.Stage = ACPI_Platform_Pool_Native.Owner.Cleanup_Required;
         begin
            if View.Stage in S.Control.Refused then
               return;
            end if;
            Wake := View.Wake_Deadline_Milliseconds;
            if Pool_Refused then
               -- Explicit, paced disposal of a refused pool carriage only.
               if Now >= Next_Disposal then
                  Root.Retry_Pool_Disposal (Completed);
                  Next_Disposal := (if Now < Word'Last - Disposal_Retry_Milliseconds
                                    then Now + Disposal_Retry_Milliseconds else M.Wait_Forever);
               end if;
               Wake := Earliest (Wake, Next_Disposal);
            elsif View.Stage = S.Control.Retained then
               -- Nothing will make progress: keep every owner alive.
               return;
            end if;
         end;
         Refuse_Pending;
         Activity := M.Wait_For_Activity_Until (Wake);
         if Activity = M.Unavailable then return; end if;
      end loop;
   end Retire;
end ACPI_One_Service;
