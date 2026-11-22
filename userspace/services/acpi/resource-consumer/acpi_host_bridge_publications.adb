pragma Ada_2022;
with CuBit.Channel_Contracts;
package body ACPI_Host_Bridge_Publications is
   package body Producers is
      package P renames ACPI_Host_Bridge_Projection;
      use type M.Failure_Reason;
   use type W.Word, H.R.Template_Status;
      use type S.Snapshot_State;
      use type Q.Outcome, Q.Action_Result, Q.Roots.Outcome, Q.Roots.Root_Kind;
      use type Q.Reader.Copy_Status, H.Collection_Status, P.Outcome, B.Phase;
      Maximum_Payload : constant Natural :=
        (CuBit.Channel_Contracts.Ring_Pages'Last - B.RC.Client_Ring_Pages) * B.RC.Page_Bytes;
      function Observe (Run : Session) return Report is (Run.Info);
      function Transport (Run : Session) return B.Report is (B.Observe (Run.Channel));
      procedure Refresh (Run : in out Session) is
      begin
         Run.Info.Query := Q.Observe (Run.Query);
         Run.Info.Recovery_Needed := Run.Info.Recovery_Needed or Run.Info.Query.Recovery_Needed;
      end Refresh;
      procedure Finish_Window (Run : in out Session; Now : out W.Word) is
         Healthy, Closed : Boolean;
      begin
         Now := 0;
         if Run.Info.Window_End_Attempted then return; end if;
         Run.Info.Window_End_Attempted := True;
         Healthy := not Run.Info.Recovery_Needed and then not Q.Pending (Run.Query);
         End_Query_Window (Healthy, Closed, Now);
         Run.Info.Window_Closed := Healthy and then Closed and then not Run.Info.Recovery_Needed;
         if not Healthy or else Run.Info.Recovery_Needed then Run.Info.Stage := Recovery_Required;
         elsif not Closed then Run.Info.Stage := Window_Refused;
         end if;
      end Finish_Window;
      procedure Start (Run : in out Session; Service : aliased in out Core.State;
         Device : Core.Node_Descriptor; Root_ID, Sequence : W.Word;
         Budget : Natural; Maximum_Items : H.R.Item_Count) is
         Action : Q.Action_Result;
      begin
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if Run.Info.Started then return; end if;
         Run.Info.Started := True; Run.Info.Working := True;
         Run.Info.Header.Root_ID := Root_ID; Run.Info.Header.Sequence := Sequence;
         if Root_ID = 0 or else Sequence = 0
           or else (for some Reason in Failure_Kind => Wire_Reasons (Reason) = M.No_Failure)
         then Run.Info.Stage := Setup_Refused; Run.Info.Working := False; return; end if;
         Q.Begin_Query (Run.Query, Service, Device, Budget, Maximum_Items, Action);
         Refresh (Run);
         if Action /= Q.Accepted then Run.Info.Recovery_Needed := True; end if;
         Run.Info.Stage := (if Run.Info.Query.Status = Q.Awaiting_Buffer then Awaiting_Buffer else Ready_For_Report);
         Run.Info.Working := False;
      exception when others => Run.Info.Recovery_Needed := True; raise;
      end Start;
      procedure Copy_And_Open (Run : in out Session; Service : aliased in out Core.State;
         Data : out S.Bytes; Descriptors : out H.Descriptor_List; Rows : out W.Rows;
         Endpoint : CuBit.Handles.Endpoint; Peer : B.P.Process_ID;
         Schema : CuBit.Protocols.Schema_Contract; Connector : Interfaces.Unsigned_16;
         Open_Token, Data_Token, Deadline : W.Word; Submitted : out Boolean) is
         Action : Q.Action_Result;
         Now : W.Word;
         Count : Natural := 0;
         None_Rows : W.Rows (1 .. 0);
         None_Data : S.Bytes (1 .. 0);
         procedure Failure_Header (State : S.Snapshot_State; Reason : Failure_Kind) is
         begin
            Run.Info.Header.State := State; Run.Info.Header.Reason := Wire_Reasons (Reason);
            Run.Info.Has_Snapshot := True;
         end Failure_Header;
      begin
         Submitted := False; Data := [others => 0]; Rows := [others => W.Empty_Row];
         Descriptors := [others => (Form => H.Empty, others => <>)];
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if not Run.Info.Started or else Run.Info.Window_End_Attempted
           or else Run.Info.Stage not in Awaiting_Buffer | Ready_For_Report | Setup_Refused then return; end if;
         Run.Info.Working := True;
         if Run.Info.Stage = Setup_Refused then
            Finish_Window (Run, Now); Run.Info.Working := False; return;
         end if;
         if Run.Info.Query.Status = Q.Awaiting_Buffer then
            Q.Copy_And_Finish (Run.Query, Service, Data, Action); Refresh (Run);
            if Action = Q.Wrong_Context then Run.Info.Working := False; return; end if;
            if Run.Info.Query.Copy.Status = Q.Reader.Insufficient_Capacity then
               Run.Info.Working := False; return;
            end if;
            if Action /= Q.Accepted then Run.Info.Recovery_Needed := True; end if;
         end if;
         if Run.Info.Recovery_Needed or else Q.Pending (Run.Query) then
            Run.Info.Recovery_Needed := True;
         elsif Run.Info.Query.Identity.Kind = Q.Roots.Unknown then
            Run.Info.Stage := No_Report;
         elsif Run.Info.Query.Status = Q.Copied then
            Run.Info.Source_Bytes := Natural (Run.Info.Query.Copy.Copied);
            if Run.Info.Source_Bytes = 0 then Failure_Header (S.M.Failed, Invalid_Resources);
            elsif (Run.Info.Query.Segment.Status = Q.Roots.Observed and then W.Word (Run.Info.Query.Segment.Raw) > 65_535)
              or else (Run.Info.Query.Base_Bus.Status = Q.Roots.Observed and then W.Word (Run.Info.Query.Base_Bus.Raw) > 255)
            then Failure_Header (S.M.Failed, Coordinate_Range);
            else
               H.Collect (Data (Data'First .. Data'First + Run.Info.Source_Bytes - 1), Descriptors, Run.Info.Collection);
               Run.Info.Header.Required_Descriptors := W.Word (Run.Info.Collection.Required);
               case Run.Info.Collection.Status is
                  when H.Complete =>
                     Count := Natural (Run.Info.Collection.Count);
                     P.Project (Descriptors, Run.Info.Collection.Count, H.R.Byte_Count (Run.Info.Source_Bytes), Rows, Run.Info.Projection);
                     if Run.Info.Projection = P.Insufficient_Capacity then Failure_Header (S.M.Incomplete, Descriptor_Capacity);
                     elsif Run.Info.Projection /= P.Complete then Failure_Header (S.M.Failed, Projection_Failed);
                     elsif Run.Info.Source_Bytes > Maximum_Payload - S.Header_Bytes
                       or else Count > (Maximum_Payload - S.Header_Bytes - Run.Info.Source_Bytes) / S.Row_Bytes
                     then Failure_Header (S.M.Incomplete, Transport_Capacity);
                     else
                        Run.Info.Header.State := S.M.Complete; Run.Info.Header.Reason := M.No_Failure;
                        Run.Info.Header.Segment :=
                          (Run.Info.Query.Segment.Status = Q.Roots.Observed, W.Word (Run.Info.Query.Segment.Raw));
                        Run.Info.Header.Base_Bus :=
                          (Run.Info.Query.Base_Bus.Status = Q.Roots.Observed, W.Word (Run.Info.Query.Base_Bus.Raw));
                        Run.Info.Has_Snapshot := True;
                     end if;
                  when H.Insufficient_Capacity => Failure_Header (S.M.Incomplete, Descriptor_Capacity);
                  when H.Unsupported_Descriptor => Failure_Header (S.M.Incomplete, Unsupported_Descriptor);
                  when H.Invalid_Descriptor => Failure_Header (S.M.Failed, Invalid_Descriptor);
                  when H.Invalid_Template => Failure_Header (S.M.Failed, Invalid_Resources);
               end case;
            end if;
         else
            case Run.Info.Query.Status is
               when Q.Missing_Resources => Failure_Header (S.M.Failed, Missing_Resources);
               when Q.Wrong_Result_Type => Failure_Header (S.M.Failed, Wrong_Result_Type);
               when Q.Budget_Exhausted => Failure_Header (S.M.Failed, Budget_Exhausted);
               when Q.Copy_Failed =>
                  if Run.Info.Query.Copy.Framing.Status = H.R.Item_Limit then
                     Failure_Header (S.M.Incomplete, Descriptor_Quota);
                  else Failure_Header (S.M.Failed, Invalid_Resources); end if;
               when others => Failure_Header (S.M.Failed, Evaluation_Failed);
            end case;
         end if;
         Finish_Window (Run, Now);
         if not Run.Info.Window_Closed or else Run.Info.Recovery_Needed or else not Run.Info.Has_Snapshot then
            Data := [others => 0]; Rows := [others => W.Empty_Row];
            Descriptors := [others => (Form => H.Empty, others => <>)]; Run.Info.Working := False; return;
         end if;
         if Run.Info.Header.State = S.M.Complete then
            B.Begin_Open (Run.Channel, Endpoint, Peer, Schema, Connector, Open_Token, Data_Token, Deadline, Now,
              Run.Info.Header, Rows (Rows'First .. Rows'First + Count - 1),
              Data (Data'First .. Data'First + Run.Info.Source_Bytes - 1), Submitted);
         else
            Data := [others => 0]; Rows := [others => W.Empty_Row];
            Descriptors := [others => (Form => H.Empty, others => <>)];
            B.Begin_Open (Run.Channel, Endpoint, Peer, Schema, Connector, Open_Token, Data_Token, Deadline, Now,
              Run.Info.Header, None_Rows, None_Data, Submitted);
         end if;
         Run.Info.Stage := (if Submitted then Opening else Transport_Refused); Run.Info.Working := False;
      exception when others => Run.Info.Recovery_Needed := True; raise;
      end Copy_And_Open;
      procedure Cancel (Run : in out Session; Service : aliased in out Core.State) is
         Action : Q.Action_Result;
         Now : W.Word;
      begin
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if not Run.Info.Started or else Run.Info.Window_End_Attempted then return; end if;
         Run.Info.Working := True;
         if Q.Pending (Run.Query) then
            Q.Cancel (Run.Query, Service, Action); Refresh (Run);
            if Action = Q.Wrong_Context then Run.Info.Working := False; return; end if;
         end if;
         Run.Info.Recovery_Needed := Run.Info.Recovery_Needed or Q.Pending (Run.Query);
         Run.Info.Stage := No_Report; Finish_Window (Run, Now); Run.Info.Working := False;
      exception when others => Run.Info.Recovery_Needed := True; raise;
      end Cancel;
      procedure Complete_Open (Run : in out Session; Item : CuBit.Messages.CompletionEntry; Handled : out Boolean) is
      begin
         Handled := False;
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if Run.Info.Window_Closed and then not Run.Info.Recovery_Needed then B.Complete_Open (Run.Channel, Item, Handled); end if;
      end Complete_Open;
      procedure Advance (Run : in out Session; Now : W.Word; Submitted : out Boolean) is
      begin
         Submitted := False;
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if not Run.Info.Window_Closed or else Run.Info.Recovery_Needed then return; end if;
         B.Note_Time (Run.Channel, Now); B.Poll (Run.Channel); B.Publish (Run.Channel, Now, Submitted);
      end Advance;
      procedure Prepare_Wait (Run : in out Session; Armed : out Boolean) is
      begin
         Armed := False;
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if Run.Info.Window_Closed and then not Run.Info.Recovery_Needed then B.Prepare_Wait (Run.Channel, Armed); end if;
      end Prepare_Wait;
      procedure Disarm (Run : in out Session) is
      begin
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if Run.Info.Window_Closed and then not Run.Info.Recovery_Needed then B.Disarm (Run.Channel); end if;
      end Disarm;
      procedure Note_Event (Run : in out Session; Event : CuBit.Control_Events.Event; Ends : out Boolean) is
      begin
         Ends := False;
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         B.Note_Event (Run.Channel, Event, Ends);
      end Note_Event;
      procedure Close (Run : in out Session; Completed : out Boolean) is
      begin
         Completed := False;
         if Run.Info.Working then Run.Info.Recovery_Needed := True; return; end if;
         if not Run.Info.Window_Closed or else Run.Info.Recovery_Needed or else Q.Pending (Run.Query) then return; end if;
         if Transport (Run).Stage = B.Not_Started then Completed := True;
         else B.Close (Run.Channel, Completed); end if;
      end Close;
      function Retired (Run : Session) return Boolean is
        (Run.Info.Window_Closed and then not Run.Info.Working and then not Run.Info.Recovery_Needed
         and then not Q.Pending (Run.Query)
         and then (Transport (Run).Stage = B.Not_Started or else B.Retired (Run.Channel)));
   end Producers;
end ACPI_Host_Bridge_Publications;
