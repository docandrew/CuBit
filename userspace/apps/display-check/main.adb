pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Display_Protocol; use CuBit.Display_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
with CuBit.Output_Discovery;
with Presentation_Test_Policy;
with GPU_Test_Policy;

--  Dedicated fixture: never starts alongside a desktop session. Only authority
--  is an endpoint to display.svc (including grants addressed to that service).
procedure Main is
   package MG renames CuBit.Memory_Grants;
   package OD renames CuBit.Output_Discovery;
   use type OD.Output_Role, OD.Query;
   use type DP.Wire_Message;
   Initial_Catalog : OD.Summary_Decoding;
   Passed : Boolean := True;
   First, Second, Reused, Rebinding : MG.Grant_Reference;
   Ok : Boolean;
   Wire : Wire_Message;
   Raw : Unsigned_64;
   Ignored : Unsigned_64;
   Frame_Token : Unsigned_64 := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
         debugPrint ("TEST: FAIL display grants " & Name & ASCII.LF);
      end if;
   end Check;
   function Send (Request : Wire_Message) return Wire_Message is
      Msg : Message := From_Wire (Request);
   begin
      Msg.tag := capCall (CAP_SLOT_DISPLAY, Msg);
      return To_Wire (Msg);
   end Send;
   procedure Expect (Request : Wire_Message; Status : DP.Status_Code;
                     Name : String) is
      Response : constant Wire_Message := Send (Request);
   begin
      Check (Response.Label = Request.Label and then Response.Length = 1 and then
             Response.Flags = 0 and then Response.Reserved = 0 and then
             Response.Words (0) = DP.Status_Code'Enum_Rep (Status), Name);
   end Expect;
   function Source_Name (Source : OD.Output_Source) return String is
     (case Source is
        when OD.Boot_Framebuffer => "BOOT_FRAMEBUFFER",
        when OD.Virtio_GPU => "VIRTIO_GPU");
   function Role_Name (Role : OD.Output_Role) return String is
     (case Role is
        when OD.Detected_Only => "DETECTED_ONLY",
        when OD.Backend_Ready => "BACKEND_READY",
        when OD.Selected_For_Desktop => "SELECTED_FOR_DESKTOP");
   procedure Discover is
      Item : OD.Description_Decoding;
      Selected : Natural := 0;
   begin
      Initial_Catalog := OD.Decode_Summary (OD.Display_Broker,
        Send (OD.Catalog_Request (OD.Display_Broker)));
      Check (Initial_Catalog.Valid and then Initial_Catalog.Value.Count > 0,
             "catalog before display lease");
      if not Initial_Catalog.Valid then
         return;
      end if;
      for Index in 1 .. Initial_Catalog.Value.Count loop
         Item := OD.Decode_Description (OD.Display_Broker,
           Send (OD.Encode_Query (OD.Display_Broker,
             (Initial_Catalog.Value.Revision, Index))));
         Check (Item.Valid and then Item.Value.Requested =
           (Initial_Catalog.Value.Revision, Index), "catalog identity");
         if not Item.Valid then
            return;
         end if;
         if Item.Value.Item.Role = OD.Selected_For_Desktop then
            Selected := Selected + 1;
         end if;
         debugPrint ("display-check: output " &
           Source_Name (Item.Value.Item.Source) &
           Item.Value.Item.Native_Number'Image & " " &
           Role_Name (Item.Value.Item.Role) & " advertised" &
           Item.Value.Item.Advertised_Width'Image & " x" &
           Item.Value.Item.Advertised_Height'Image & ASCII.LF);
         if Item.Value.Item.Role /= OD.Detected_Only then
            debugPrint ("display-check: active " &
              Source_Name (Item.Value.Item.Source) &
              Item.Value.Item.Native_Number'Image &
              Item.Value.Item.Current_Width'Image & " x" &
              Item.Value.Item.Current_Height'Image & ASCII.LF);
         end if;
      end loop;
      Check (Selected = 1, "exactly one desktop-selected output");
      Wire := OD.Catalog_Request (OD.Display_Broker);
      Wire.Words (3) := 1;
      Expect (Wire, DP.Bad_Object, "reserved discovery field rejected");
      Expect (OD.Encode_Query (OD.Display_Broker,
        ((if Initial_Catalog.Value.Revision = 1 then 2 else 1), 1)),
        DP.Bad_State, "wrong catalog revision rejected");
      if Initial_Catalog.Value.Count < OD.Output_Count'Last then
         Expect (OD.Encode_Query (OD.Display_Broker,
           (Initial_Catalog.Value.Revision, Initial_Catalog.Value.Count + 1)),
           DP.Bad_Object, "missing output rejected");
      end if;
      if Passed then
         debugPrint ("DISPLAY-DISCOVERY-CHECK: PASS" & ASCII.LF);
      end if;
   end Discover;
   function Generation (Ref : MG.Grant_Reference) return Unsigned_64 is
     (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Ref.slot));
   procedure Frame (Session, ID : Live_ID; Outcome : Frame_Outcome;
                    Disposition : Buffer_Disposition;
                    Area : DP.Rectangle := (0, 0, 4, 2);
                    Output : Output_Number := 0) is
      Msg : constant Message := From_Wire
        (With_Output (Encode_Frame ((Session, ID, Area)), Output));
      Completion : CompletionEntry := NULL_COMPLETION;
      Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 5000;
      Event : Message;
      Found : Boolean;
      Activity : Activity_Result;
   begin
      Frame_Token := Frame_Token + 1;
      if not capSubmit (CAP_SLOT_DISPLAY, Msg, Frame_Token) then
         Check (False, "frame transport admission"); return;
      end if;
      loop
         Ignored := Poll_Completion (Completion'Address);
         exit when Ignored = 1;
         loop
            Found := Poll_Event (Event);
            exit when not Found;
         end loop;
         Activity := Wait_For_Activity_Until (Deadline);
         if Activity /= Work_Available or else syscall (SYSCALL_GETTIME) >= Deadline then
            Check (False, "frame completion deadline"); return;
         end if;
      end loop;
      declare
         Result : constant Frame_Result_Decoding := Decode_Frame_Result (To_Wire (Completion.msg));
      begin
         Check (Completion.valid and then Completion.status = COMPLETION_OK and then
                Completion.token = Frame_Token and then Result.Valid and then
                Result.Value = (Session, ID, Outcome, Disposition), "frame result identity/lifetime");
      end;
   end Frame;
   procedure Exercise is
   begin
      Discover;
      Raw := syscall (SYSCALL_SBRK, 8192);
      Check (Raw /= Unsigned_64'Last, "allocate pixels");
      if Raw = Unsigned_64'Last then return; end if;
      declare
         Address : constant Integer_Address :=
           Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
         Pixels : array (Natural range 0 .. 1023) of Unsigned_32
           with Address => To_Address (Address), Volatile;
      begin
         Pixels := [others => 16#FF80_4020#];
         MG.Create_Via_Capability
           (CAP_SLOT_DISPLAY, To_Address (Address), 1, False, First, Ok);
         Check (Ok, "create read-only grant");
         if not Ok then return; end if;
         Expect (Encode_Attachment ((First, (4, 2, 16))), DP.Denied,
                 "attachment requires display lease");
         Expect (Encode_Lease_Request (Acquire_Display), DP.Success, "acquire lease");
         -- Kernel acquisition capacity is 127. Replacement must return the
         -- old pin even when the grant reference is unchanged.
         for Attempt in 1 .. 140 loop
            Expect (Encode_Attachment ((First, (4, 2, 16))), DP.Success,
                    "balanced repeated attachment");
         end loop;
         -- Geometry fits the screen; byte span deliberately exceeds one page.
         Expect (Encode_Attachment ((First, (4, 2, 4096))), DP.Bad_Object,
                 "grant byte range checked");
         for Fault in 1 .. 5 loop
            Wire := Encode_Attachment ((First, (4, 2, 16)));
            case Fault is
               when 1 => Wire.Length := 3;
               when 2 => Wire.Flags := 1;
               when 3 => Wire.Reserved := 16; -- Outside output routing range.
               when 4 => Wire.Words (1) := 0;
               when 5 => Wire.Words (3) := Unsigned_64'Last;
            end case;
            Expect (Wire, DP.Bad_Object, "malformed attachment rejected");
         end loop;
         declare
            Opened : Wire_Message := Send (Encode_Open_Session);
            Session, Old_Session : Live_ID;
         begin
            Check (Opened.Length = 4 and then Opened.Words (0) = 0 and then
                   Opened.Words (1) /= 0, "open presentation session");
            if Opened.Words (1) = 0 then return; end if;
            Session := Opened.Words (1);
            for ID in Live_ID range 1 .. 140 loop
               Frame (Session, ID, Published, Released);
               Pixels (0) := Pixels (0) + 1;
            end loop;
            Frame (Session, 140, Rejected, Not_Acquired);
            Frame (Session, 141, Rejected, Not_Acquired, (0, 0, 5, 2));
            Frame (Session, 141, Rejected, Not_Acquired, (0, 0, 0, 2));
            Frame (Session, 141, Rejected, Not_Acquired, (65_535, 65_535, 65_535, 65_535));
            Wire := Encode_Frame ((Session, 141, (0, 0, 4, 2)));
            Wire.Words (3) := 1;
            Expect (Wire, DP.Bad_Object, "noncanonical frame rejected");
            Frame (Session, 141, Published, Released);
            Expect ((Code (Present_Rectangle), 4, 0, 0, [0, 0, 4, 2]), DP.Bad_State,
                    "session excludes untracked legacy presentation");
            Old_Session := Session;
            Opened := Send (Encode_Open_Session);
            Check (Opened.Words (0) = 0 and then Opened.Words (1) > Old_Session,
                   "new session never reuses identity");
            if Opened.Words (1) = 0 then return; end if;
            Session := Opened.Words (1);
            Frame (Old_Session, 142, Rejected, Not_Acquired);
            Frame (Session, 1, Published, Released);
            if Presentation_Test_Policy.Rebind_Enabled then
               Old_Session := Session;
               Frame (Session, Presentation_Test_Policy.Rebind_After_Frame,
                      Published, Released);
               if Initial_Catalog.Valid then
                  Expect (OD.Encode_Query (OD.Display_Broker,
                    (Initial_Catalog.Value.Revision, 1)), DP.Bad_State,
                    "old discovery revision rejected after output rebind");
                  declare
                     Updated : constant OD.Summary_Decoding :=
                       OD.Decode_Summary (OD.Display_Broker,
                         Send (OD.Catalog_Request (OD.Display_Broker)));
                  begin
                     Check (Updated.Valid and then Updated.Value.Revision >
                       Initial_Catalog.Value.Revision, "new catalog revision");
                  end;
               end if;
               Frame (Session, Presentation_Test_Policy.Rebind_After_Frame + 1,
                      Rejected, Not_Acquired);
               Opened := Send (Encode_Open_Session);
               Check (Opened.Words (0) = DP.Status_Code'Enum_Rep (DP.Denied),
                      "stale output lease cannot reopen");
               Expect (Encode_Lease_Request (Acquire_Display), DP.Success,
                       "reacquire new output generation");
               -- A fresh lease alone must not revive the old session.
               Frame (Session, Presentation_Test_Policy.Rebind_After_Frame + 1,
                      Rejected, Not_Acquired);
               MG.Revoke (First, Ok);
               Check (Ok, "revoke source after output invalidation");
               -- Only a remaining acquisition can retain a revoked grant.
               Check (Generation (First) = First.generation,
                      "output invalidation does not release attachment pin");
               Opened := Send (Encode_Open_Session);
               Check (Opened.Words (0) = DP.Status_Code'Enum_Rep (DP.Bad_State),
                      "stale attachment cannot reopen");
               MG.Create_Via_Capability
                 (CAP_SLOT_DISPLAY, To_Address (Address), 1, False, Rebinding, Ok);
               Check (Ok, "replacement grant after output invalidation");
               if not Ok then return; end if;
               Expect (Encode_Attachment ((Rebinding, (4, 2, 16))), DP.Success,
                       "reattach to new output generation");
               Check (Generation (First) = 0,
                      "reattachment returns invalidated output's source pin");
               First := Rebinding;
               Opened := Send (Encode_Open_Session);
               Check (Opened.Words (0) = 0 and then Opened.Words (1) > Old_Session,
                      "rebound output gets fresh session");
               if Opened.Words (1) = 0 then return; end if;
               Session := Opened.Words (1);
               Frame (Old_Session, Presentation_Test_Policy.Rebind_After_Frame + 2,
                      Rejected, Not_Acquired);
               Frame (Session, 1, Published, Released);
               if Passed then
                  debugPrint ("DISPLAY-OUTPUT-REBIND-CHECK: PASS" & ASCII.LF);
               end if;
            end if;
            -- A new attachment invalidates the session, including queued old
            -- requests, while retaining the existing legacy grant tests below.
            Expect (Encode_Attachment ((First, (4, 2, 16))), DP.Success,
                    "replace attachment closes presentation session");
            Frame (Session, 2, Rejected, Not_Acquired);
         end;
         MG.Revoke (First, Ok);
         Check (Ok, "revoke attached grant");
         Check (Generation (First) = First.generation, "revoked mapping remains pinned");
         Expect (Encode_Attachment ((First, (4, 2, 16))), DP.Bad_Object,
                 "revocation denies new acquisition");
         Expect ((Code (Present_Rectangle), 4, 0, 0, [0, 0, 4, 2]), DP.Success,
                 "old attachment still readable after rejection and revocation");
         MG.Create_Via_Capability
           (CAP_SLOT_DISPLAY, To_Address (Address), 1, False, Second, Ok);
         Check (Ok, "create replacement");
         if not Ok then return; end if;
         Expect (Encode_Attachment ((Second, (4, 2, 16))), DP.Success, "replace buffer");
         declare
            Opened : constant Wire_Message := Send (Encode_Open_Session);
         begin
            Check (Opened.Words (0) = 0 and then Opened.Words (1) /= 0, "replacement session");
            if Opened.Words (1) = 0 then return; end if;
            Frame (Opened.Words (1), 1, Published, Released);
            MG.Revoke (Second, Ok);
            Check (Ok, "revoke between frames");
            Frame (Opened.Words (1), 2, Rejected, Not_Acquired);
            Wire := Send (Encode_Open_Session);
            Check (Wire.Words (0) /= 0, "revoked grant cannot reopen session");
         end;
         Check (Generation (First) = 0, "replacement returns old pin");
         MG.Create_Via_Capability
           (CAP_SLOT_DISPLAY, To_Address (Address), 1, False, Reused, Ok);
         Check (Ok, "create reused slot");
         if Ok then
            Check (Reused.slot = First.slot and then Reused.generation /= First.generation,
                   "slot reused with new generation");
            Expect (Encode_Attachment ((First, (4, 2, 16))), DP.Bad_Object,
                    "stale generation rejected");
            MG.Revoke (Reused, Ok);
            Check (Ok, "revoke unused grant");
         end if;
         for Fault in 1 .. 4 loop
            Wire := Encode_Lease_Request (Release_Display);
            case Fault is
               when 1 => Wire.Length := 0;
               when 2 => Wire.Flags := 1;
               when 3 => Wire.Reserved := 16;
               when 4 => Wire.Words (0) := 1;
            end case;
            Expect (Wire, DP.Bad_Object, "malformed release rejected");
            Check (Generation (Second) = Second.generation, "malformed release retains pin");
         end loop;
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "release lease");
         Check (Generation (Second) = 0, "release returns final pin");
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "idempotent release");
         Expect (Encode_Lease_Request (Acquire_Display), DP.Success, "lease reusable");
         Expect ((Code (Present_Rectangle), 4, 0, 0, [0, 0, 4, 2]), DP.Bad_State,
                 "released source pointer cleared");
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "final release");
         if Passed then debugPrint ("DISPLAY-ASYNC-CHECK: PASS" & ASCII.LF); end if;
      end;
   end Exercise;

   procedure Exercise_Outputs is
      Info : constant Wire_Message := Send
        (With_Output ((Code (Get_Information), 4, 0, 0, [others => 0]), 1));
      subtype Head is Output_Number range 0 .. 1;
      Grants : array (Head) of MG.Grant_Reference;
      Sessions : array (Head) of Live_ID := [others => 1];
      Addresses : array (Head) of Integer_Address;
      procedure Paint (Output : Head; Color : Unsigned_32) is
         Pixels : array (0 .. 1023) of Unsigned_32
           with Address => To_Address (Addresses (Output)), Volatile;
      begin
         Pixels := [others => Color];
      end Paint;
      procedure Concurrent_Frames is
         Tokens : array (Head) of Unsigned_64;
         Finished : array (Head) of Boolean := [others => False];
         Completion : aliased CompletionEntry;
         Started : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
         Deadline : constant Unsigned_64 := Started + 5000;
         Event : Message;
         Output : Head;
         Activity : Activity_Result;
      begin
         for H in Head loop
            Frame_Token := Frame_Token + 1;
            Tokens (H) := Frame_Token;
            Check (capSubmit (CAP_SLOT_DISPLAY, From_Wire
              (With_Output (Encode_Frame ((Sessions (H), 1, (0, 0, 32, 32))), H)),
               Tokens (H)), "concurrent frame admission");
         end loop;
         --  A synchronous query must return while head 0 is pending; neither
         --  the broker nor the shared GPU service may block behind that head.
         Wire := Send (With_Output
           ((Code (Get_Information), 4, 0, 0, [others => 0]), 1));
         Check (Wire.Length = 4 and then Wire.Words (0) = 1024,
                "broker query while frames outstanding");
         if GPU_Test_Policy.Delay_First_Output_Ms /= 0 then
            Check (syscall (SYSCALL_GETTIME) < Started + GPU_Test_Policy.Delay_First_Output_Ms,
                   "query returned before delayed output");
            Expect (Encode_Lease_Request (Release_Display), DP.Bad_State,
                    "pending output cannot release lease");
            Expect (Encode_Attachment ((Grants (0), (32, 32, 128))),
                    DP.Bad_State, "pending output cannot replace attachment");
         end if;
         while not (Finished (0) and Finished (1)) loop
            if Poll_Completion (Completion'Address) = 1 then
               if Completion.token = Tokens (0) then Output := 0;
               elsif Completion.token = Tokens (1) then Output := 1;
               else
                  Check (False, "unrelated concurrent completion");
                  return;
               end if;
               declare
                  Result : constant Frame_Result_Decoding :=
                    Decode_Frame_Result (To_Wire (Completion.msg));
               begin
                  Check (not Finished (Output) and then Completion.valid and then
                    Completion.status = COMPLETION_OK and then Result.Valid and then
                    Result.Value = (Sessions (Output), 1, Published, Released),
                    "concurrent result identity and acquisition return");
               end;
               if Output = 0 and then GPU_Test_Policy.Delay_First_Output_Ms /= 0 then
                  Check (Finished (1), "other output completed before stalled output");
               end if;
               Finished (Output) := True;
            else
               if Poll_Event (Event) then null; end if;
               Activity := Wait_For_Activity_Until (Deadline);
               if Activity /= Work_Available or else syscall (SYSCALL_GETTIME) >= Deadline then
                  Check (False, "concurrent completion deadline");
                  return;
               end if;
            end if;
         end loop;
         if Passed then
            debugPrint ("DISPLAY-CONCURRENT-CHECK: PASS" & ASCII.LF);
            if GPU_Test_Policy.Delay_First_Output_Ms /= 0 then
               debugPrint ("DISPLAY-STALLED-OUTPUT-CHECK: PASS" & ASCII.LF);
            end if;
         end if;
      end Concurrent_Frames;
   begin
      if Info.Length /= 4 or else Info.Words (0) /= 1024 or else
        Info.Words (1) /= 768 or else Presentation_Test_Policy.Rebind_Enabled
      then
         return;
      end if;
      for Output in Head loop
         Raw := syscall (SYSCALL_SBRK, 8192);
         Check (Raw /= Unsigned_64'Last, "per-output pixels");
         if Raw = Unsigned_64'Last then return; end if;
         Addresses (Output) :=
           Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
         Paint (Output, (if Output = 0 then 16#00E0_3020# else 16#0020_50E0#));
         MG.Create_Via_Capability
           (CAP_SLOT_DISPLAY, To_Address (Addresses (Output)), 1, False,
            Grants (Output), Ok);
         Check (Ok, "per-output grant");
         if not Ok then return; end if;
         Expect (With_Output (Encode_Attachment
           ((Grants (Output), (32, 32, 128))), Output), DP.Denied,
           "output lease required independently");
         Expect (With_Output (Encode_Lease_Request (Acquire_Display), Output),
                 DP.Success, "per-output lease");
         Expect (With_Output (Encode_Attachment
           ((Grants (Output), (32, 32, 128))), Output), DP.Success,
           "per-output attachment");
         Wire := Send (With_Output (Encode_Open_Session, Output));
         Check (Wire.Length = 4 and then Wire.Words (0) = 0 and then
                Wire.Words (1) /= 0, "per-output session");
         if not Passed then return; end if;
         Sessions (Output) := Wire.Words (1);
      end loop;
      Check (Sessions (0) /= Sessions (1), "output-bound session identity");
      Frame (Sessions (0), 1, Rejected, Not_Acquired, (0, 0, 32, 32), 1);
      Frame (Sessions (1), 1, Rejected, Not_Acquired, (0, 0, 32, 32), 0);
      Concurrent_Frames;
      if not Passed then return; end if;
      debugPrint ("DISPLAY-DUAL: phase-a" & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, 2000);
      Paint (1, 16#0030_D050#);
      Frame (Sessions (1), 2, Published, Released, (0, 0, 32, 32), 1);
      Expect (Encode_Lease_Request (Release_Display), DP.Success,
              "release first output alone");
      MG.Revoke (Grants (0), Ok);
      Check (Ok and then Generation (Grants (0)) = 0, "first output unpinned");
      if not Passed then return; end if;
      debugPrint ("DISPLAY-DUAL: phase-b" & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, 2000);
      Frame (Sessions (0), 2, Rejected, Not_Acquired, (0, 0, 32, 32), 0);
      Frame (Sessions (1), 1, Rejected, Not_Acquired, (0, 0, 32, 32), 1);
      Paint (1, 16#00E0_C030#);
      Frame (Sessions (1), 3, Published, Released, (0, 0, 32, 32), 1);
      if not Passed then return; end if;
      debugPrint ("DISPLAY-DUAL: phase-c" & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, 2000);
      Expect (With_Output (Encode_Lease_Request (Release_Display), 1),
              DP.Success, "release second output");
      MG.Revoke (Grants (1), Ok);
      Check (Ok and then Generation (Grants (1)) = 0, "second output unpinned");
      if Passed then debugPrint ("DISPLAY-DUAL-CHECK: PASS" & ASCII.LF); end if;
   end Exercise_Outputs;

   procedure Exercise_Backend_Failure is
      Info, Status : Wire_Message;
   begin
      Discover;
      for Output in Output_Number range 0 .. 1 loop
         Info := Send (With_Output ((Label => Code (Get_Information), others => <>), Output));
         Check (Info.Length = 4 and then Info.Words (0) = 1024 and then
                Info.Words (1) = 768, "ready output before backend failure");
         Expect (With_Output (Encode_Lease_Request (Acquire_Display), Output),
                 DP.Success, "acquire before injected failure");
         Expect (With_Output ((Label => Code (Clear), Length => 1,
                               Words => [16#00123456#, 0, 0, 0], others => <>), Output),
                 DP.Bad_State, "GPU clear failure must not fall back to firmware");
         Status := Send (With_Output ((Label => Code (Get_Status), others => <>), Output));
         Check (Status.Label = Code (Get_Status) and then Status.Length = 4 and then
                Status.Words (0) = 3, "failed GPU retains native backend identity");
         Expect (With_Output ((Label => Code (Clear), Length => 1,
                               Words => [0, 0, 0, 0], others => <>), Output),
                 DP.Bad_State, "backend fault is sticky");
         Expect (With_Output (Encode_Lease_Request (Acquire_Display), Output),
                 DP.Bad_State, "cannot reacquire failed backend");
         Check (Send (With_Output ((Label => Code (Get_Information), others => <>), Output)) = Info,
                "failure cannot change output geometry");
      end loop;
      if Passed then debugPrint ("DISPLAY-BACKEND-FAILURE-CHECK: PASS" & ASCII.LF); end if;
   end Exercise_Backend_Failure;
begin
   if GPU_Test_Policy.Reject_Client_Clear then
      Exercise_Backend_Failure;
   else
      Exercise;
      if Passed then Exercise_Outputs; end if;
   end if;
   if Passed then
      debugPrint ("DISPLAY-GRANTS-CHECK: PASS" & ASCII.LF);
   end if;
   Ignored := syscall (SYSCALL_EXIT, (if Passed then 0 else 1));
end Main;
