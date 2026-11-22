pragma Ada_2022;
with Interfaces;
with ACPI_Requests.Power_Preparation.With_Endpoint;
with ACPI_Requests.Cached_S5; with CuBit.Power_Event_Exchange; with ACPI_Native_Log;
with ACPI_One_Service; with ACPI_Sleep_Entry;
package body ACPI_Native_Power_Preparation is
   package Q renames ACPI_Requests; package One renames ACPI_One_Service;
   package P renames ACPI_One_Service.Power_Preparation;
   use type Q.Phase, One.Runtime.Status.Phase;
   Runtime_Mode, Static_Mode : Boolean := False;
   package Static renames Q.Cached_S5;
   package E renames CuBit.Power_Event_Exchange;
   Cached : Static.Snapshot;
   Static_Query_Budget : constant Natural := 2; -- At most two used sleep-type fields.
   use type Interfaces.Unsigned_8, Interfaces.Unsigned_16, Interfaces.Unsigned_32, Interfaces.Unsigned_64, Static.Cache.Objects.Register_Model, Static.Cache.Objects.Query_Outcome;
   Item : P.Requests.Run;
   function Owner_Ready (Server : Q.State) return Boolean is
     (Runtime_Mode and then Q.Current (Server) = Q.Complete
      and then not Q.Notifications_Pending (Server) and then not Q.Retained_Results_Pending (Server)
      and then (One.Runtime.Current = One.Runtime.Status.Idle
        or else (Static_Mode and then One.Runtime.Current = One.Runtime.Status.Stopped))
      and then One.Root.Hardware.Admitting
      and then One.Root.Fields.Control_Idle and then One.Root.Fields.EC_Commands.Quiescent);
   function Register_Model return P.Objects.Register_Model is
     (case One.Root.Hardware.Sleep_Register_Model is
        when ACPI_Sleep_Entry.PM1A_Only => P.Objects.PM1A_Only,
        when ACPI_Sleep_Entry.PM1A_And_B => P.Objects.PM1A_And_B,
        when ACPI_Sleep_Entry.Hardware_Reduced => P.Objects.Hardware_Reduced,
        when ACPI_Sleep_Entry.No_Registers => P.Objects.No_Sleep_Registers);
   package Endpoint is new P.Requests.With_Endpoint (P.Read_Clock, Register_Model, Owner_Ready);
   procedure Dispatch_Static (Server : in out Q.State; Request : CuBit.Messages.Message;
      Reply : out CuBit.Messages.Message; Handled : out Boolean) is
      Now, After_Query : Interfaces.Unsigned_64;
      Available : Boolean;
      Result : Static.Cache.Objects.Query_Report;
      Model : Static.Cache.Objects.Register_Model;
      Window_Microseconds : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (E.Prepare_Budget_Milliseconds) * 1_000;
      procedure Status (Value : E.Prepare_Status) is
      begin Reply.words (E.Prepare_Status_Word) := Interfaces.Unsigned_64 (E.Prepare_Status'Enum_Rep (Value)); end Status;
   begin
      Reply := CuBit.Messages.NULL_MESSAGE; Handled := Request.tag.label = E.Prepare_To_Sleep;
      if not Handled then return; end if;
      Reply.tag.label := E.Prepare_To_Sleep; Reply.tag.length := E.Prepare_Reply_Words;
      Reply.words (E.Reply_Request_Word) := Request.words (E.Request_Word);
      if Request.authorityTag /= E.Authority_Tag then Status (E.Not_Approved); return; end if;
      if CuBit.Messages.Flag_Byte (Request.tag) /= 0 or else Request.tag.reserved /= 0 or else Request.words (3) /= 0
        or else not E.Sleep_Request_Valid (Interfaces.Unsigned_64 (Request.tag.length), Request.words (E.Request_Word),
          Request.words (E.State_Word), Request.words (E.Confirm_Word)) then Status (E.Malformed); return; end if;
      if Request.words (E.State_Word) /= Interfaces.Unsigned_64 (E.Sleep_State'Enum_Rep (E.S5)) then
         Status (E.Unsupported_State); return; end if;
      Static.Invalidate (Cached); P.Requests.Invalidate (Item);
      if not Owner_Ready (Server) then
         ACPI_Native_Log.Info ("acpi: stage power-prepare fail CACHED_STATIC_S5 reason=OWNER_NOT_READY" &
            " runtime=" & One.Runtime.Current'Image & " hardware=" & Boolean'Image (One.Root.Hardware.Admitting) &
            " fields=" & Boolean'Image (One.Root.Fields.Control_Idle) &
            " ec-quiet=" & Boolean'Image (One.Root.Fields.EC_Commands.Quiescent));
         Status (E.Not_Ready); return;
      end if;
      Model := (case One.Root.Hardware.Sleep_Register_Model is
         when ACPI_Sleep_Entry.PM1A_Only => Static.Cache.Objects.PM1A_Only,
         when ACPI_Sleep_Entry.PM1A_And_B => Static.Cache.Objects.PM1A_And_B,
         when ACPI_Sleep_Entry.Hardware_Reduced => Static.Cache.Objects.Hardware_Reduced,
         when ACPI_Sleep_Entry.No_Registers => Static.Cache.Objects.No_Sleep_Registers);
      if Model = Static.Cache.Objects.No_Sleep_Registers then Status (E.Not_Ready); return; end if;
      P.Read_Clock (Now, Available);
      if not Available or else Now >= Interfaces.Unsigned_64'Last - Window_Microseconds then Status (E.Not_Ready); return; end if;
      Static.Prepare (Server, Cached, Model, Static_Query_Budget, Result);
      P.Read_Clock (After_Query, Available);
      if not Available or else After_Query < Now or else After_Query >= Now + Window_Microseconds
        or else not Owner_Ready (Server) then Static.Invalidate (Cached); Status (E.Not_Ready); return; end if;
      declare Copy : constant Static.Cache.Copied_Types := Static.Current_Types (Server, Cached); begin
         if Copy.Available then
            Reply.words (E.Sleep_Types_Word) := E.Sleep_Types_Of (Interfaces.Unsigned_16 (Copy.Type_A), Interfaces.Unsigned_16 (Copy.Type_B));
            Status (E.Prepared);
            ACPI_Native_Log.Info ("acpi: stage power-prepare ok CACHED_STATIC_S5 methods=SKIPPED");
         elsif Result.Outcome in Static.Cache.Objects.Not_Declared | Static.Cache.Objects.Wrong_Result_Type
           | Static.Cache.Objects.Wrong_Element_Type | Static.Cache.Objects.Invalid_Type_Value
           | Static.Cache.Objects.Missing_Elements | Static.Cache.Objects.Evaluation_Failed then Status (E.No_Sleep_Package);
         else Status (E.Not_Ready); end if;
      end;
   end Dispatch_Static;
   procedure Dispatch (Server : in out Q.State; Request : CuBit.Messages.Message;
      Runtime_Selected : Boolean; Reply : out CuBit.Messages.Message; Handled : out Boolean;
      Cached_Shutdown_Selected : Boolean := False) is
      Result : Q.Packet;
   begin
      Runtime_Mode := Runtime_Selected or Cached_Shutdown_Selected;
      if Request.tag.label = E.Prepare_To_Sleep then Static_Mode := not Runtime_Selected and then Cached_Shutdown_Selected; end if;
      if Static_Mode then Dispatch_Static (Server, Request, Reply, Handled); return; end if;
      Endpoint.Dispatch (Server, Item, Request.authorityTag,
        (Label => Request.tag.label, Length => Request.tag.length, Flags => CuBit.Messages.Flag_Byte (Request.tag),
         Reserved => Request.tag.reserved, Data => Q.Words (Request.words)), Result, Handled);
      Reply := CuBit.Messages.NULL_MESSAGE;
      if not Handled then return; end if;
      Reply.tag := (label => Result.Label, length => Result.Length, protocol => 0, ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
   procedure Invalidate is begin P.Requests.Invalidate (Item); Static.Invalidate (Cached); end Invalidate;
   procedure Retire is begin P.Requests.Retire (Item); Static.Retire (Cached); end Retire;
   function Current_Types (Server : Q.State) return P.Controller.Copied_Types is
   begin
      if not Owner_Ready (Server) then return P.Controller.Empty_Types; end if;
      if Static_Mode then
         declare Copy : constant Static.Cache.Copied_Types := Static.Current_Types (Server, Cached); begin
            return (Available => Copy.Available,
               Registers => (case Copy.Registers is when Static.Cache.Objects.PM1A_Only => P.Objects.PM1A_Only,
                  when Static.Cache.Objects.PM1A_And_B => P.Objects.PM1A_And_B,
                  when Static.Cache.Objects.Hardware_Reduced => P.Objects.Hardware_Reduced,
                  when Static.Cache.Objects.No_Sleep_Registers => P.Objects.No_Sleep_Registers),
               Type_A => P.Objects.Sleep_Type (Copy.Type_A), Type_B => P.Objects.Sleep_Type (Copy.Type_B));
         end;
      end if;
      return P.Requests.Current_Types (Server, Item);
   end Current_Types;
end ACPI_Native_Power_Preparation;
