pragma Ada_2022;
with Ada.Text_IO;
with System.Storage_Elements;
with Firmware_Tables;
with Fixture;
with ACPI_Host_Bridge_Resources;
with ACPI_Host_Bridge_Projection;
with ACPI_Host_Bridge_Channel;
with CuBit.Channels;
with CuBit.Channel_Bells;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Protocols;
with CuBit.Control_Events;
procedure Channel_Tests is
   package B renames ACPI_Host_Bridge_Channel;
   package W renames B.W; package S renames B.S; package M renames B.M;
   package R renames B.RC; package C renames CuBit.Channels;
   package H renames ACPI_Host_Bridge_Resources;
   package P renames ACPI_Host_Bridge_Projection;
   package F renames Firmware_Tables;
   use type W.Word, F.Byte, B.Phase, B.Failure, S.Outcome, H.Collection_Status, P.Outcome;
   use type W.Row, R.Token;
   use type F.Bytes;
   use type M.Acceptance, CuBit.Channel_Bells.Watch_Result;
   Unrelated_Schema : constant CuBit.Protocols.Schema_Id := 16#4F54_4845_5200_0001#;
   Root : constant W.Word := 16#F123_4567_ABCD_0123#;
   Serial : constant W.Word := W.Word'Last;
   Open_Tag : constant W.Word := 16#FEDC_BA98_7654_3210#;
   Data_Tag : constant W.Word := Open_Tag + 1;
   Peer : constant B.P.Process_ID := B.P.From_Word (16#DEAD_BEEF_1234_0001#);
   Owner : constant B.P.Process_ID := CuBit.Messages.Model_Owner;
   Items : H.Descriptor_List (1 .. 20); Rows : W.Rows (1 .. 20);
   Collected : H.Collection_Report; Projected : P.Outcome;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   function Base (At_Address : System.Address) return W.Word is
     (W.Word (System.Storage_Elements.To_Integer (At_Address)));
begin
   H.Collect (Fixture.Data, Items, Collected); Check (Collected.Status = H.Complete, "real fixture collected");
   P.Project (Items, 20, H.R.Byte_Count (Fixture.Data'Length), Rows, Projected);
   Check (Projected = P.Complete, "real fixture projected");
   for Mode in 1 .. 43 loop
      declare
         Client_Memory : aliased F.Bytes (1 .. 128 * R.Page_Bytes) := [others => 0] with Alignment => 4096;
         Server_Memory : aliased F.Bytes (1 .. R.Server_Pages * R.Page_Bytes) := [others => 0] with Alignment => 4096;
         IO : B.Session; Server : R.Server;
         Info : S.Header := (State => S.M.Complete, Root_ID => Root, Sequence => Serial,
            Required_Descriptors => 20, others => <>);
         Schema : CuBit.Protocols.Schema_Contract :=
           (Identity => B.M.Schema, Version => 1, Sizing => CuBit.Protocols.Fixed_Size,
            Wire_Size => M.Message_Bytes);
         Endpoint : CuBit.Handles.Endpoint := CuBit.Handles.Model_Endpoint;
         Target : B.P.Process_ID := Peer;
         Tag : W.Word := Open_Tag;
         Initial_Now : W.Word := 10;
         Completion : CuBit.Messages.CompletionEntry := CuBit.Messages.NULL_COMPLETION;
         Got, Submitted, Handled, Armed, Rung, Done, Ends : Boolean;
         Request : R.Queues.Submission;
         Answer : M.Result;
         View : B.Report;
      begin
         C.Mode := C.Good; C.Open_Calls := 0; C.Finish_Calls := 0; C.Close_Calls := 0; C.Signals := 0;
         C.Submit_OK := True; C.Signal_Answer := C.Signalled;
         C.Own_Base := Base (Client_Memory'Address); C.Peer_Base := Base (Server_Memory'Address);
         CuBit.Messages.Model_Owner := Owner;
         CuBit.Memory_Grants.Model_Retired := False;
         CuBit.Channel_Bells.Clear_Noted;
         case Mode is
            when 13 => C.Mode := C.Refuse_Open;
            when 14 => C.Mode := C.Fail_Open;
            when 15 => C.Mode := C.Bad_Alignment;
            when 16 => C.Mode := C.Bad_Contract;
            when 17 => C.Mode := C.Missing_Peer;
            when 18 => C.Mode := C.Bad_Own_Length;
            when 19 => C.Mode := C.Aliased_Regions;
            when 20 => C.Mode := C.Overlapping_Regions;
            when 21 => C.Submit_OK := False;
            when 22 => Info.Root_ID := 0;
            when 23 => Tag := Data_Tag;
            when 24 => Initial_Now := 75;
            when 25 => Schema.Identity := CuBit.Protocols.NO_SCHEMA;
            when 26 => Target := B.P.No_Process;
            when 43 => Schema.Identity := Unrelated_Schema;
            when 27 => Endpoint := CuBit.Handles.No_Endpoint;
            when 33 =>
               declare Route : CuBit.Channel_Bells.Watch_Result; begin
                  CuBit.Channel_Bells.Watch (C.Peer_Reference, Route);
                  Check (Route = CuBit.Channel_Bells.Unrouted and then CuBit.Channel_Bells.Take (C.Peer_Reference, True),
                    "option-B wake requires every channel to look");
               end;
            when others => null;
         end case;
         if Mode = 28 then
            declare Huge : constant F.Bytes (1 .. 128 * R.Page_Bytes) := [others => 0]; Empty : W.Rows (1 .. 0); begin
               Info.Required_Descriptors := 0;
               B.Begin_Open (IO, Endpoint, Target, Schema, 0, Tag, Data_Tag, 75, Initial_Now,
                 Info, Empty, Huge, Submitted);
            end;
         else
            B.Begin_Open (IO, Endpoint, Target, Schema, 0, Tag, Data_Tag, 75, Initial_Now,
              Info, Rows, Fixture.Data, Submitted);
         end if;
         if Mode in 22 .. 28 | 43 then
            Check (not Submitted and C.Open_Calls = 0, "invalid setup never allocates/submits");
            B.Close (IO, Done); Check (Done and B.Retired (IO), "pre-effect refusal has no region debt");
         elsif Mode = 21 then
            Check (not Submitted and C.Open_Calls = 1 and C.Close_Calls = 1, "failed submit is not dispatched");
            B.Close (IO, Done); Check (Done and not B.Retired (IO), "failed-open retired grant remains owned");
            CuBit.Memory_Grants.Model_Retired := True; Check (B.Retired (IO), "failed-open retirement queried");
         else
            Check (Submitted and C.Open_Calls = 1 and B.Observe (IO).Stage = B.Opening, "one async open");
            Check (C.Last_Token = Open_Tag and B.Observe (IO).Deadline = 75, "original token and finite deadline");
            Check (B.Observe (IO).Packet_Bytes = S.Header_Bytes + 20 * S.Row_Bytes + Fixture.Data'Length,
               "payload measured from descriptors and copied CRS");
            B.Close (IO, Done); Check (not Done and C.Close_Calls = 0, "pending open cannot close");
            if Mode = 36 then
               B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Returned,
                  Slot => C.Own_Reference.slot, Generation => C.Own_Reference.generation, Peer => Peer), Ends);
               Check (Ends and B.Observe (IO).Poisoned, "own grant return during pending setup retains");
               B.Close (IO, Done); Check (not Done, "unknown pending setup is not retired by an event");
            else
               if Mode = 2 then B.Note_Time (IO, 75); end if;
               Completion := (token => Open_Tag, from => B.P.To_Word (Peer), valid => True, others => <>);
               if Mode in 29 .. 34 and Mode /= 33 then
                  case Mode is
                     when 29 => Completion.from := Completion.from xor 16#0100_0000_0000_0000#;
                     when 30 => Completion.token := Completion.token + 1;
                     when 31 => Completion.valid := False;
                     when 32 => Completion.status := W.Word'Last;
                     when 34 => CuBit.Messages.Model_Owner := B.P.From_Word (16#A123_4567_ABCD_0124#);
                     when others => null;
                  end case;
                  B.Complete_Open (IO, Completion, Handled);
                  Check (not Handled and C.Finish_Calls = 0, "wrong completion does not consume original setup");
                  B.Close (IO, Done); Check (not Done, "unrecognized setup completion remains owned");
                  CuBit.Messages.Model_Owner := Owner;
                  Completion := (token => Open_Tag, from => B.P.To_Word (Peer), valid => True, others => <>);
               end if;
               B.Complete_Open (IO, Completion, Handled); Check (Handled and C.Finish_Calls = 1, "original open completion consumed once");
               B.Complete_Open (IO, Completion, Handled); Check (not Handled and C.Finish_Calls = 1, "duplicate setup completion refused");
               if Mode in 13 .. 14 then
                  Check (B.Observe (IO).Stage = B.Setup_Refused, "peer open refusal/failure explicit");
                  B.Close (IO, Done); Check (Done and not B.Retired (IO), "refused open waits for region retirement");
               elsif Mode in 15 .. 20 then
                  Check (B.Observe (IO).Poisoned and B.Observe (IO).Why = B.Mapping_Refused, "invalid mapping never attaches rings");
                  B.Publish (IO, 10, Submitted); Check (not Submitted, "no request on invalid mapping");
                  B.Close (IO, Done); Check (not Done, "invalid mapping retained");
               elsif Mode = 2 then
                  Check (B.Observe (IO).Expired and B.Observe (IO).Stage = B.Ready, "late setup reply still drained");
                  B.Publish (IO, 75, Submitted); Check (not Submitted, "expiry before publication prevents effects");
                  B.Close (IO, Done); Check (Done, "known unused channel can close after late setup");
               else
                  Check (B.Observe (IO).Stage = B.Ready, "real request rings attached after checked open");
                  R.Attach_Server (Server, Server_Memory'Address, Client_Memory'Address);
                  Armed := R.Arm_For_Requests (Server); Check (Armed, "server armed before publish");
                  if Mode = 9 then C.Signal_Answer := C.Ended; end if;
                  if Mode = 11 then
                     Info.Sequence := 7;
                     B.Begin_Open (IO, Endpoint, Target, Schema, 0, 1, 2, 200, 10, Info, Rows, Fixture.Data, Submitted);
                     Check (not Submitted and C.Open_Calls = 1 and B.Observe (IO).Sequence = Serial, "no session reset or renewed deadline");
                  end if;
                  B.Publish (IO, 10, Submitted); Check (Submitted and B.Observe (IO).Published, "one snapshot published");
                  if Mode = 9 then Check (B.Observe (IO).Why = B.Wake_Failed, "failed wake retains published request"); end if;
                  B.Publish (IO, 11, Submitted); Check (not Submitted, "published snapshot never replayed");
                  B.Close (IO, Done); Check (not Done, "payload retained until original receipt");
                  if Mode = 3 then B.Note_Time (IO, 75); Check (B.Observe (IO).Expired, "original expiry recorded"); end if;
                  if Mode = 37 then
                     B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Revoked,
                       Slot => C.Peer_Reference.slot, Generation => C.Peer_Reference.generation,
                       Peer => B.P.From_Word (B.P.To_Word (Peer) xor 16#0100_0000_0000_0000#)), Ends);
                     Check (not Ends and not B.Observe (IO).Poisoned, "event must match original full-width peer as well as grant");
                  end if;
                  if Mode = 10 then
                     declare Before : constant F.Bytes := Client_Memory; begin
                        B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Revoked,
                          Slot => C.Peer_Reference.slot, Generation => C.Peer_Reference.generation, Peer => Peer), Ends);
                        Check (Ends, "authenticated grant end invalidates binding");
                        B.Disarm (IO); B.Poll (IO); B.Prepare_Wait (IO, Armed);
                        Check (not Armed, "ended binding is never armed");
                        for I in Client_Memory'Range loop Check (Client_Memory (I) = Before (I), "no mapped writes after grant end"); end loop;
                        B.Close (IO, Done); Check (not Done, "ended unknown publication remains retained");
                     end;
                  else
                     if Mode = 35 then
                        CuBit.Messages.Model_Owner := B.P.From_Word (16#A123_4567_ABCD_0124#);
                        B.Disarm (IO); B.Poll (IO); B.Prepare_Wait (IO, Armed);
                        Check (not Armed, "wrong owner cannot access mapped wait words");
                        CuBit.Messages.Model_Owner := Owner;
                     end if;
                     R.Take (Server, Request, Got, Rung); Check (Got and Request.Tag = R.Token (Data_Tag), "actual server takes original token");
                     Check (Request.Item (M.Root_ID) = Root and Request.Item (M.Sequence) = Serial,
                        "full-width root and serial copied through real rings");
                     declare
                        Back : W.Rows (1 .. 20); Source_Back : F.Bytes (1 .. Fixture.Data'Length);
                        Decoded : S.Header; Result : S.Report;
                        Size : constant Natural := Natural (Request.Item (M.Packet_Bytes));
                     begin
                        S.Decode (Client_Memory (Client_Memory'First + R.Client_Payload_At .. Client_Memory'First + R.Client_Payload_At + Size - 1), Decoded, Back, Source_Back, Result);
                        Check (Result.Status = S.Done and Decoded.Root_ID = Root and Decoded.Sequence = Serial,
                           "actual published payload decodes all-or-none");
                        for I in Rows'Range loop Check (Back (I) = Rows (I), "descriptor facts preserved"); end loop;
                        for I in Source_Back'Range loop Check (Source_Back (I) = Fixture.Data (Fixture.Data'First + I - Source_Back'First), "CRS copied before async open returns"); end loop;
                     end;
                     Answer := [M.Root_ID => Root, M.Sequence => Serial,
                        M.Disposition => M.Acceptance_Code
                          (if Mode = 4 then M.Malformed elsif Mode = 40 then M.Identity_Mismatch
                           elsif Mode = 41 then M.Stale_Sequence elsif Mode = 42 then M.Too_Large else M.Accepted)];
                     if Mode = 6 then Answer (M.Root_ID) := 0; end if;
                     if Mode = 7 then Answer (M.Sequence) := 0; end if;
                     if Mode = 8 then Answer (M.Disposition) := 99; end if;
                     B.Prepare_Wait (IO, Armed); Check (Armed, "completion wait armed before server receipt");
                     R.Complete (Server, (if Mode = 5 then Request.Tag + 1 else Request.Tag), Answer, Rung);
                     B.Disarm (IO); B.Poll (IO); View := B.Observe (IO);
                     if Mode in 5 .. 8 then
                        Check (View.Poisoned and not View.Acknowledged, "malformed receipt does not release payload");
                        if Mode = 8 then
                           B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Revoked,
                             Slot => C.Peer_Reference.slot, Generation => C.Peer_Reference.generation, Peer => Peer), Ends);
                           Check (Ends and B.Observe (IO).Why = B.Receipt_Refused,
                             "later grant end cannot replace an earlier uncertain receipt reason");
                        end if;
                        B.Close (IO, Done); Check (not Done, "ring reaping alone does not reconcile unknown outcome");
                     else
                        Check (View.Stage = B.Receipt_Known and View.Acknowledged, "original receipt known, including after expiry or failed wake");
                        Check (View.Peer_Accepted = (Mode not in 4 | 40 .. 42), "peer refusal distinct from accepted snapshot");
                        Check (View.Acceptance = M.Acceptance_From_Code (Answer (M.Disposition)),
                          "all canonical typed acceptance outcomes are preserved");
                        if Mode in 38 | 39 then
                           declare
                              Before_Own : constant F.Bytes := Client_Memory;
                              Before_Peer : constant F.Bytes := Server_Memory;
                           begin
                              if Mode = 38 then
                                 B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Revoked,
                                   Slot => C.Peer_Reference.slot, Generation => C.Peer_Reference.generation, Peer => Peer), Ends);
                              else
                                 B.Note_Event (IO, (Kind => CuBit.Control_Events.Grant_Returned,
                                   Slot => C.Own_Reference.slot, Generation => C.Own_Reference.generation, Peer => Peer), Ends);
                              end if;
                              Check (Ends and B.Observe (IO).Why = B.Peer_Ended, "known acknowledged channel end");
                              B.Disarm (IO); B.Poll (IO); B.Prepare_Wait (IO, Armed);
                              Check (not Armed and Client_Memory = Before_Own and Server_Memory = Before_Peer,
                                "ended acknowledged channel never accesses mapped wait words");
                              B.Close (IO, Done);
                              Check (Done and Client_Memory = Before_Own and Server_Memory = Before_Peer,
                                "settled channel closes with private-only ring detachment");
                           end;
                        end if;
                        B.Close (IO, Done); Check (Done and not B.Retired (IO), "known receipt closes but region retirement still owed");
                        CuBit.Memory_Grants.Model_Retired := True; Check (B.Retired (IO), "exact own-region retirement required");
                     end if;
                     R.Detach_Server (Server);
                  end if;
               end if;
            end if;
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Host bridge native channel checks" & Checks'Image);
end Channel_Tests;
