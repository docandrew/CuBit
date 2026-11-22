pragma Ada_2022;
with ACPI_Requests.Device_Enumeration;
with ACPI_Requests.Host_Bridge_Publication;
with ACPI_Fallible_Heap;
with System;
with CuBit.Channel_Contracts;
package body ACPI_Host_Bridge_Owners is
   package body Managers is
      package Q renames ACPI_Requests;
      package C renames ACPI_Service;
      use type W.Word, System.Address, B.Phase, B.Failure;
      use type B.M.Failure_Reason;
      use type G.H.R.Byte_Count, B.S.Snapshot_State;
      use type CuBit.Protocols.Protocol_Version, CuBit.Protocols.Wire_Size_Kind, Interfaces.Unsigned_32;
      Heap_Alignment : constant W.Word := 16;
      Retirement_Poll_Milliseconds : constant W.Word := 10;
      Maximum_Wire_Rows : constant Natural :=
        ((CuBit.Channel_Contracts.Ring_Pages'Last - B.RC.Client_Ring_Pages) * B.RC.Page_Bytes - B.S.Header_Bytes) / B.S.Row_Bytes;
      View : Report;
      Owner : Server_Access := null;
      Target : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Expected_Peer : B.P.Process_ID := B.P.No_Process;
      Element : CuBit.Protocols.Schema_Contract;
      Connector_Number : Interfaces.Unsigned_16 := 0;
      Batch : Positive := 1;
      Fuel : Natural := 0;
      Item_Limit : G.H.R.Item_Count := 0;
      Have_Time : Boolean := False;
      Last_Us, Last_Ms : W.Word := 0;
      function Metadata_Admitted return Boolean is (View.Metadata_Claim);
      function Query_Admitted return Boolean is
        (View.Window_Open and then not View.Stop_Requested and then Query_Window_Current);
      procedure End_Query (Healthy : Boolean; Closed : out Boolean; Now_Milliseconds : out W.Word) is
      begin
         End_Window (Owner.all, Healthy, Closed, Now_Milliseconds);
         if Closed and Healthy then View.Window_Open := False; end if;
      end End_Query;
      package Publisher is new G.Producers (C, Evaluate_Named, End_Query, Wire_Reasons);
      package Access_Path is new Q.Host_Bridge_Publication (Publisher, Query_Admitted);
      package A renames Access_Path.Controller;
      package Metadata is new Q.Device_Enumeration (Metadata_Admitted);
      package E renames Metadata.Controller.Cursor;
      use type A.Admission, Metadata.Controller.Admission, E.Step_Result, Publisher.Phase;
      use type Publisher.Q.Outcome;
      Scan : Metadata.Run;
      type Work_Node;
      type Work_Access is access all Work_Node;
      type Work_Node is limited record
         Device : C.Node_Descriptor := C.Values.No_Node;
         Work : Access_Path.Run;
         Query_Started, Counted, Close_Known : Boolean := False;
         Root_ID, Sequence, Open_Token, Data_Token : W.Word := 0;
         Raw, Descriptors, Rows : System.Address := System.Null_Address;
         Raw_Bytes, Row_Count : Natural := 0;
         Next : Work_Access := null;
      end record;
      Head, Tail, Held : Work_Access := null;
      function Observe return Report is (View);
      function Blocks_AML return Boolean is (View.Metadata_Claim or View.Window_Open);
      function Current_Transport return B.Report is
        (if Head /= null then A.Transport (Head.Work)
         elsif Held /= null then A.Transport (Held.Work) else (others => <>));
      procedure Fail (Why : Failure; Keep : Boolean := False) is
      begin
         View.Why := Why; View.Had_Errors := True; View.Stop_Requested := True;
         View.Metadata_Claim := False;
         View.Stage := (if Keep then Retained else Stopping);
      end Fail;
      function Sample return Boolean is
         Us, Ms : W.Word; Available : Boolean;
      begin
         Clock (Us, Ms, Available);
         if not Available or else Us = CuBit.Messages.Wait_Forever or else Ms = CuBit.Messages.Wait_Forever
           or else (Have_Time and then (Us < Last_Us or else Ms < Last_Ms))
         then return False; end if;
         Have_Time := True; Last_Us := Us; Last_Ms := Ms; return True;
      end Sample;
      function Allocate_Work (Device : C.Node_Descriptor) return Work_Access is
         Bits : constant W.Word := Work_Node'Object_Size;
         Bytes : constant W.Word := (Bits + System.Storage_Unit - 1) / System.Storage_Unit;
         Base : constant System.Address := ACPI_Fallible_Heap.Reserve (Bytes, W.Word'Max (Heap_Alignment, Work_Node'Alignment));
      begin
         if Base = System.Null_Address then return null; end if;
         declare
            -- Default initialization constructs FRESH limited state in its
            -- own stable allocation, as in the existing snapshot loader.
            pragma Warnings (Off, "default initialization of * may modify overlaid storage");
            Item : aliased Work_Node with Address => Base;
            pragma Warnings (On, "default initialization of * may modify overlaid storage");
         begin Item.Device := Device; return Item'Unchecked_Access; end;
      end Allocate_Work;
      procedure Free_Scratch is
      begin
         ACPI_Fallible_Heap.Release (Head.Raw); Head.Raw := System.Null_Address;
         ACPI_Fallible_Heap.Release (Head.Descriptors); Head.Descriptors := System.Null_Address;
         ACPI_Fallible_Heap.Release (Head.Rows); Head.Rows := System.Null_Address;
      end Free_Scratch;
      procedure Remove_Head is
         Old : constant Work_Access := Head;
      begin
         Free_Scratch; Head := Old.Next;
         if Head = null then Tail := null; end if;
         ACPI_Fallible_Heap.Release (Old.all'Address);
      end Remove_Head;
      procedure Hold_Head is
         Old : constant Work_Access := Head;
      begin
         Free_Scratch;
         Head := Old.Next;
         if Head = null then Tail := null; end if;
         Old.Next := Held; Held := Old;
         View.Held_Sessions := View.Held_Sessions + 1;
      end Hold_Head;
      procedure Continue_Discovery is
      begin
         if Head /= null then View.Stage := Starting_Query;
         else
            View.Discovery_Finished := True;
            if Held = null then View.Stage := Finished; Owner := null;
            else View.Stage := Holding; end if;
         end if;
      end Continue_Discovery;
      procedure Start (Owner : Server_Access; Endpoint : CuBit.Handles.Endpoint;
         Peer : B.P.Process_ID; Schema : CuBit.Protocols.Schema_Contract;
         Connector : Interfaces.Unsigned_16;
         Deadline_Microseconds, Deadline_Milliseconds : W.Word;
         Metadata_Budget : Natural; Metadata_Batch : Positive; AML_Budget : Natural;
         Maximum_Items : G.H.R.Item_Count) is
         Allowed : Metadata.Controller.Admission;
      begin
         if View.Working then Fail (Reentered, True); return; end if;
         if View.Attempted or View.Stop_Requested then return; end if;
         View.Attempted := True; View.Working := True;
         View.Deadline_Microseconds := Deadline_Microseconds;
         View.Deadline_Milliseconds := Deadline_Milliseconds;
         if Owner = null or else not Q.Valid (Owner.all) or else not Q.Device_Walk_Finished (Owner.all)
           or else not CuBit.Handles.Valid (Endpoint) or else not B.P.Is_Process (Peer)
           or else not CuBit.Protocols.Valid (Schema) or else Schema.Version /= 1
           or else Schema.Sizing /= CuBit.Protocols.Fixed_Size or else Schema.Wire_Size /= B.M.Message_Bytes
           or else Deadline_Microseconds = 0 or else Deadline_Milliseconds = 0
           or else Deadline_Microseconds = CuBit.Messages.Wait_Forever
           or else Deadline_Milliseconds = CuBit.Messages.Wait_Forever
           or else (for some Reason in G.Failure_Kind => Wire_Reasons (Reason) = B.M.No_Failure)
         then Fail (Invalid_Setup); View.Working := False; return; end if;
         Managers.Owner := Owner; Target := Endpoint; Expected_Peer := Peer;
         Element := Schema; Connector_Number := Connector; Batch := Metadata_Batch;
         Fuel := AML_Budget; Item_Limit := G.H.R.Item_Count'Min (Maximum_Items, G.H.R.Item_Count (Maximum_Wire_Rows));
         if not Sample then Fail (Clock_Failed); View.Working := False; return; end if;
         if Last_Us >= Deadline_Microseconds or else Last_Ms >= Deadline_Milliseconds then
            Fail (Deadline_Expired); View.Working := False; return;
         end if;
         if not Metadata_Boundary_Ready then Fail (Metadata_Refused); View.Working := False; return; end if;
         View.Metadata_Claim := True;
         Metadata.Start (Scan, Owner.all, Metadata_Budget, Allowed);
         if Allowed /= Metadata.Controller.Admitted then Fail (Metadata_Refused);
         else View.Stage := Capturing; end if;
         View.Working := False;
      exception when others => Fail (Unexpected_Fault, True); raise;
      end Start;
      function Allocate_Scratch return Boolean is
         type Descriptor_Array is array (Positive range <>) of G.H.Descriptor;
         Descriptor_Bytes : constant W.Word := (Descriptor_Array'Component_Size + System.Storage_Unit - 1) / System.Storage_Unit;
         Row_Bytes : constant W.Word := W.Word (B.S.Row_Bytes);
         Required : constant G.H.R.Byte_Count := A.Observe (Head.Work).Query.Copy.Required;
      begin
         if Required = 0 then return False; end if;
         Head.Raw_Bytes := Natural (Required);
         Head.Row_Count := Natural'Min (Head.Raw_Bytes, Natural (Item_Limit));
         Head.Raw := ACPI_Fallible_Heap.Reserve (W.Word (Head.Raw_Bytes), Heap_Alignment);
         if Head.Raw = System.Null_Address then return False; end if;
         if Head.Row_Count > 0 then
            Head.Descriptors := ACPI_Fallible_Heap.Reserve (W.Word (Head.Row_Count) * Descriptor_Bytes,
               W.Word'Max (Heap_Alignment, G.H.Descriptor'Alignment));
            if Head.Descriptors = System.Null_Address then return False; end if;
            Head.Rows := ACPI_Fallible_Heap.Reserve (W.Word (Head.Row_Count) * Row_Bytes,
               W.Word'Max (Heap_Alignment, W.Row'Alignment));
            if Head.Rows = System.Null_Address then return False; end if;
         end if;
         return True;
      end Allocate_Scratch;
      procedure Close_Unstarted_Window is
         Closed : Boolean; Now : W.Word;
      begin
         End_Query (not Q.Retained_Results_Pending (Owner.all), Closed, Now);
         if not Closed then Fail (Query_Uncertain, True); end if;
      end Close_Unstarted_Window;
      procedure Count_Receipt is
      begin
         if A.Transport (Head.Work).Acknowledged and then not Head.Counted then
            Head.Counted := True;
            View.Acknowledged := View.Acknowledged + 1;
            if not A.Transport (Head.Work).Peer_Accepted then
               View.Had_Errors := True; View.Why := Peer_Refused;
            end if;
         end if;
      end Count_Receipt;
      procedure Cleanup_Current (Result : out Step_Result) is
         Allowed : A.Admission; Closed, Submitted : Boolean;
      begin
         Result := Idle;
         if Head = null and then Held /= null and then View.Stop_Requested then
            Head := Held; Held := Held.Next;
            Head.Next := null; Tail := Head;
            View.Held_Sessions := View.Held_Sessions - 1;
         end if;
         if Head = null then
            View.Stage := Finished; Owner := null; return;
         end if;
         if View.Window_Open then
            if Head.Query_Started then
               Access_Path.Cancel (Head.Work, Owner.all, Allowed);
               if Allowed /= A.Admitted or else not A.Observe (Head.Work).Window_Closed then
                  Fail (Query_Uncertain, True); Result := Failed; return;
               end if;
            else Close_Unstarted_Window;
            end if;
         end if;
         if View.Stage = Retained then Result := Failed; return; end if;
         if not Head.Query_Started then Remove_Head; Result := Runnable; return; end if;
         if A.Transport (Head.Work).Stage = B.Awaiting_Receipt then
            -- Published requests may drain during stop. A Ready channel is
            -- CLOSED instead; never call the publishing advance on it here.
            A.Advance (Head.Work, Last_Ms, Submitted);
         end if;
         if A.Transport (Head.Work).Poisoned
           and then not (A.Transport (Head.Work).Acknowledged and then A.Transport (Head.Work).Why = B.Peer_Ended)
         then Fail (Transport_Uncertain, True); Result := Failed; return; end if;
         if A.Transport (Head.Work).Stage = B.Setup_Refused then
            View.Had_Errors := True; View.Why := Transport_Refused;
         end if;
         Count_Receipt;
         A.Close (Head.Work, Closed); Head.Close_Known := Closed;
         if Closed and then A.Retired (Head.Work) then Remove_Head; Result := Runnable; end if;
      end Cleanup_Current;
      procedure Advance (Result : out Step_Result) is
         Device : C.Node_Descriptor; Reads : Natural; Step : E.Step_Result;
         Meta_Allowed : Metadata.Controller.Admission;
         Allowed : A.Admission; Window : Window_Result;
         Available, Submitted : Boolean;
         Node : Work_Access;
         Empty_Data : B.S.Bytes (1 .. 0); Empty_Descriptors : G.H.Descriptor_List (1 .. 0); Empty_Rows : W.Rows (1 .. 0);
         Info : Publisher.Report;
      begin
         Result := Idle;
         if View.Working then Fail (Reentered, True); Result := Failed; return; end if;
         if View.Stage = Retained then Result := Failed; return; end if;
         if not View.Attempted or else View.Stage = Finished then return; end if;
         -- Discovery's finite allowance never limits the lifetime of an
         -- acknowledged producer session. No clock/AML/poll work in Holding.
         if View.Stage = Holding and then not View.Stop_Requested then return; end if;
         View.Working := True;
         if not Sample then
            Fail (Clock_Failed, View.Window_Open or else (Head /= null and then Head.Query_Started));
         elsif Last_Us >= View.Deadline_Microseconds or else Last_Ms >= View.Deadline_Milliseconds then
            if not View.Stop_Requested then Fail (Deadline_Expired); end if;
         end if;
         if View.Stage = Retained then Result := Failed; View.Working := False; return; end if;
         if View.Stop_Requested then
            View.Stage := Stopping; Cleanup_Current (Result); View.Working := False; return;
         end if;
         case View.Stage is
            when Capturing =>
               Metadata.Advance (Scan, Owner.all, Batch, Device, Reads, Step, Meta_Allowed);
               if Meta_Allowed /= Metadata.Controller.Admitted then Fail (Metadata_Refused);
               elsif Step = E.Device_Found then
                  Node := Allocate_Work (Device);
                  if Node = null then Fail (Allocation_Refused);
                  else
                     if Tail = null then Head := Node; else Tail.Next := Node; end if;
                     Tail := Node; View.Candidates := View.Candidates + 1;
                  end if;
               elsif Step = E.Range_Exhausted then View.Metadata_Claim := False; View.Stage := Starting_Query;
               elsif Step /= E.Yielded then Fail (Enumeration_Incomplete);
               end if;
               Result := Runnable;
            when Starting_Query =>
               if Head = null then
                  Continue_Discovery;
               elsif Query_Boundary_Ready then
                  Correlation (Head.Device, Head.Root_ID, Head.Sequence, Head.Open_Token, Head.Data_Token, Available);
                  if not Available or else Head.Root_ID = 0 or else Head.Sequence = 0
                    or else Head.Open_Token = 0 or else Head.Data_Token = 0
                    or else Head.Open_Token = Head.Data_Token
                    or else Head.Open_Token = CuBit.Messages.NO_COMPLETION_TOKEN
                    or else Head.Data_Token = CuBit.Messages.NO_COMPLETION_TOKEN
                  then Fail (Correlation_Refused);
                  else
                     Begin_Window (Owner.all, View.Deadline_Microseconds, Window);
                     case Window is
                        when Quiet_Refusal => View.Had_Errors := True; View.Why := Window_Refused; Remove_Head;
                        when Uncertain => View.Window_Open := True; Fail (Window_Refused, True);
                        when Opened =>
                           View.Window_Open := True;
                           if View.Stop_Requested then Close_Unstarted_Window;
                           else
                              Access_Path.Start (Head.Work, Owner.all, Head.Device, Head.Root_ID, Head.Sequence, Fuel, Item_Limit, Allowed);
                              if Allowed /= A.Admitted then
                                 View.Had_Errors := True; View.Why := Wrong_Owner;
                                 Close_Unstarted_Window;
                                 if View.Stage /= Retained then Remove_Head; end if;
                              else Head.Query_Started := True; View.Stage := Copying_Query; end if;
                           end if;
                     end case;
                  end if;
                  Result := Runnable;
               end if;
            when Copying_Query =>
               if Head.Raw = System.Null_Address then
                  Access_Path.Copy_And_Open (Head.Work, Owner.all, Empty_Data, Empty_Descriptors, Empty_Rows,
                    Target, Expected_Peer, Element, Connector_Number, Head.Open_Token, Head.Data_Token,
                    View.Deadline_Milliseconds, Allowed, Submitted);
                  if Allowed = A.Admitted and then A.Observe (Head.Work).Stage = Publisher.Awaiting_Buffer then
                     if not Allocate_Scratch then
                        View.Had_Errors := True; View.Why := Allocation_Refused;
                        Access_Path.Cancel (Head.Work, Owner.all, Allowed);
                        if Allowed /= A.Admitted or else not A.Observe (Head.Work).Window_Closed then Fail (Query_Uncertain, True);
                        else View.Stage := Retiring; Free_Scratch; end if;
                     end if;
                     Result := Runnable; View.Working := False; return;
                  end if;
               else
                  declare
                     --  SEC-025: own private scratch allocation retained through Copy_And_Open.
                     Data : B.S.Bytes (1 .. Head.Raw_Bytes) with Import, Address => Head.Raw;
                     --  SEC-025: own private descriptor allocation; released after Copy_And_Open.
                     Descriptors : G.H.Descriptor_List (1 .. Head.Row_Count) with Import, Address => Head.Descriptors;
                     --  SEC-025: own private row allocation; released after Copy_And_Open.
                     Rows : W.Rows (1 .. Head.Row_Count) with Import, Address => Head.Rows;
                  begin
                     Access_Path.Copy_And_Open (Head.Work, Owner.all, Data, Descriptors, Rows,
                       Target, Expected_Peer, Element, Connector_Number, Head.Open_Token, Head.Data_Token,
                       View.Deadline_Milliseconds, Allowed, Submitted);
                  end;
               end if;
               Info := A.Observe (Head.Work);
               if Allowed /= A.Admitted or else not Info.Window_Closed or else Info.Recovery_Needed then Fail (Query_Uncertain, True);
               else
                  Free_Scratch; View.Visited := View.Visited + 1;
                  if Submitted then
                     View.Reports_Opened := View.Reports_Opened + 1; View.Stage := Waiting_Open;
                     case Info.Header.State is
                        when B.S.M.Complete => View.Complete_Reports := View.Complete_Reports + 1;
                        when B.S.M.Incomplete => View.Incomplete_Reports := View.Incomplete_Reports + 1; View.Had_Errors := True;
                        when B.S.M.Failed => View.Failed_Reports := View.Failed_Reports + 1; View.Had_Errors := True;
                     end case;
                  else
                     if Info.Stage = Publisher.No_Report then
                        View.Skipped := View.Skipped + 1;
                        if Info.Query.Status /= Publisher.Q.Not_A_Root then View.Had_Errors := True; end if;
                     else View.Had_Errors := True; end if;
                     View.Stage := Retiring;
                  end if;
               end if;
               Result := Runnable;
            when Waiting_Open | Waiting_Receipt =>
               A.Advance (Head.Work, Last_Ms, Submitted);
               if A.Transport (Head.Work).Poisoned then Fail (Transport_Uncertain, True);
               elsif A.Transport (Head.Work).Acknowledged then
                  Count_Receipt;
                  if A.Transport (Head.Work).Peer_Accepted then
                     Hold_Head; Continue_Discovery;
                  else View.Stage := Retiring; end if;
                  Result := Runnable;
               elsif A.Transport (Head.Work).Stage = B.Awaiting_Receipt then View.Stage := Waiting_Receipt;
               elsif A.Transport (Head.Work).Stage = B.Setup_Refused then
                  View.Had_Errors := True; View.Why := Transport_Refused;
                  View.Stage := Retiring; Result := Runnable;
               end if;
            when Retiring =>
               Cleanup_Current (Result);
               if View.Stage /= Retained and then Result = Runnable then Continue_Discovery; end if;
            when Fresh | Holding | Stopping | Finished | Retained => null;
         end case;
         if View.Stage = Retained then Result := Failed; end if;
         View.Working := False;
      exception when others => Fail (Unexpected_Fault, True); raise;
      end Advance;
      procedure Complete_Open (Item : CuBit.Messages.CompletionEntry; Handled : out Boolean) is
      begin
         Handled := False;
         if View.Working then Fail (Reentered, True); return; end if;
         if Head /= null and then Head.Query_Started then A.Complete_Open (Head.Work, Item, Handled); end if;
      end Complete_Open;
      procedure Note_Event (Item : CuBit.Control_Events.Event; Ends : out Boolean) is
         Node : Work_Access := Held;
         Ended : Boolean;
      begin
         Ends := False;
         if View.Working then Fail (Reentered, True); return; end if;
         if Head /= null and then Head.Query_Started then A.Note_Event (Head.Work, Item, Ends); end if;
         -- Each retained candidate was charged to the finite metadata budget.
         -- This bounded scan changes no links and evaluates no AML.
         while Node /= null loop
            A.Note_Event (Node.Work, Item, Ended);
            Ends := Ends or Ended;
            Node := Node.Next;
         end loop;
         if Ends then
            View.Had_Errors := True; View.Why := Transport_Ended;
            Request_Stop;
         end if;
      end Note_Event;
      procedure Prepare_Wait (Armed : out Boolean) is
      begin
         Armed := False;
         if View.Working then Fail (Reentered, True); return; end if;
         if Head /= null and then Head.Query_Started then A.Prepare_Wait (Head.Work, Armed); end if;
      end Prepare_Wait;
      procedure Disarm is
      begin
         if View.Working then Fail (Reentered, True); return; end if;
         if Head /= null and then Head.Query_Started then A.Disarm (Head.Work); end if;
      end Disarm;
      procedure Request_Stop is
      begin
         View.Stop_Requested := True; View.Metadata_Claim := False;
         if not View.Attempted and then View.Stage = Fresh then View.Stage := Finished; end if;
      end Request_Stop;
      function Wake_Deadline return W.Word is
      begin
         if View.Stage in Fresh | Finished | Retained then return CuBit.Messages.Wait_Forever; end if;
         if View.Stage = Holding and then not View.Stop_Requested then return CuBit.Messages.Wait_Forever; end if;
         if View.Stop_Requested or else View.Stage = Retiring then
            return (if Have_Time and then Last_Ms < W.Word'Last - Retirement_Poll_Milliseconds
              then Last_Ms + Retirement_Poll_Milliseconds else CuBit.Messages.Wait_Forever);
         end if;
         return View.Deadline_Milliseconds;
      end Wake_Deadline;
      function Retired return Boolean is
        (View.Stage = Finished and then Head = null and then Held = null and then not View.Working
         and then not View.Window_Open and then not View.Metadata_Claim);
   end Managers;
end ACPI_Host_Bridge_Owners;
