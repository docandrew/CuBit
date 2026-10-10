pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Display_Protocol; use CuBit.Display_Protocol;
with CuBit.Display_Pool_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
with CuBit.Output_Discovery;
with CuBit.Display_Planes;
with CuBit.Display_Plane_Protocol;
with Presentation_Test_Policy;
with GPU_Test_Policy;

--  Dedicated fixture: never starts alongside a desktop session. Only authority
--  is an endpoint to display.svc (including grants addressed to that service).
procedure Main is
   package MG renames CuBit.Memory_Grants;
   use type MG.Grant_Reference;
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
      Msg.tag := capCall (CAP_SLOT_DISPLAY, Msg, CuBit.Messages.Wait_Forever);
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
            --  One global grant table: the slot may go to another process
            --  first, but a reused slot always carries a new generation.
            Check (Reused /= First, "replacement is a new reference");
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

   procedure Exercise_Pool is
      package P renames CuBit.Display_Pool_Protocol;
      use type P.Completion;
      Grants : array (P.Buffer_Slot) of MG.Grant_Reference;
      Addresses : array (P.Buffer_Slot) of Integer_Address;
      Session : Unsigned_64 := 0;
      procedure Pool_Frame (Slot : P.Buffer_Slot; ID : Live_ID;
                            Outcome : Frame_Outcome; Disposition : Buffer_Disposition;
                            Area : DP.Rectangle := (0, 0, 32, 32)) is
         Msg : constant Message := From_Wire
           (P.Encode (P.Frame'(Slot, (Session, ID, Area))));
         C : CompletionEntry := NULL_COMPLETION;
         Deadline : constant Unsigned_64 := syscall (SYSCALL_GETTIME) + 5000;
         Activity : Activity_Result;
      begin
         Frame_Token := Frame_Token + 1;
         Check (capSubmit (CAP_SLOT_DISPLAY, Msg, Frame_Token), "pool submit transport");
         if not Passed then return; end if;
         if GPU_Test_Policy.Delay_First_Output_Ms /= 0 and then Outcome = Published then
            Wire := Send ((Label => Code (Get_Information), others => <>));
            Check (Wire.Length = 4, "pool pending query remains responsive");
            Expect (Encode_Lease_Request (Release_Display), DP.Bad_State, "pool pending cannot release");
            Expect (P.Encode (P.Attachment'(Slot, (Grants (Slot), (32, 32, 128)))),
              DP.Bad_State, "pool pending cannot replace slot");
            declare
               Other : constant P.Buffer_Slot := (if Slot = 3 then 1 else Slot + 1);
               Rejected_Reply : constant P.Completion_Decoding := P.Decode_Completion
                 (Send (P.Encode (P.Frame'(Other, (Session, ID + 100, (0, 0, 32, 32))))));
               Pixels : array (0 .. 1023) of Unsigned_32
                 with Import, Address => To_Address (Addresses (Other)), Volatile;
            begin
               Check (Rejected_Reply.Valid and then Rejected_Reply.Value =
                 (Other, (Session, ID + 100, Rejected, Not_Acquired)), "pool overload rejects without retargeting held slot");
               -- Independent root allocation remains writable during the hold.
               Pixels := [others => 16#FFAABBCC#];
            end;
         end if;
         loop
            Ignored := Poll_Completion (C'Address);
            exit when Ignored = 1;
            Activity := Wait_For_Activity_Until (Deadline);
            if Activity /= Work_Available or else syscall (SYSCALL_GETTIME) >= Deadline then
               Check (False, "pool completion deadline"); return;
            end if;
         end loop;
         declare R : constant P.Completion_Decoding := P.Decode_Completion (To_Wire (C.msg));
         begin
            Check (C.valid and then C.status = COMPLETION_OK and then C.token = Frame_Token and then
              R.Valid and then R.Value = (Slot, (Session, ID, Outcome, Disposition)),
              "pool completion slot/session/frame/disposition");
         end;
      end Pool_Frame;
   begin
      -- Rebinding deliberately invalidates the first session; covered separately.
      if Presentation_Test_Policy.Rebind_Enabled then return; end if;
      Expect (Encode_Lease_Request (Acquire_Display), DP.Success, "pool lease");
      for B in P.Buffer_Slot loop
         Raw := syscall (SYSCALL_SBRK, 8192);
         Check (Raw /= Unsigned_64'Last, "pool storage");
         if not Passed then return; end if;
         Addresses (B) := Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
         MG.Create_Via_Capability (CAP_SLOT_DISPLAY, To_Address (Addresses (B)), 1, False, Grants (B), Ok);
         Check (Ok, "pool root grant");
         if not Passed then return; end if;
         Expect (P.Encode (P.Attachment'(B, (Grants (B), (32, 32, 128)))),
                 DP.Success, "pool slot registration");
         Expect (P.Encode (P.Attachment'(B, (Grants (B), (32, 32, 128)))),
                 DP.Bad_State, "pool slot cannot be replaced");
         if B < 3 then
            Wire := Send (P.Encode_Open);
            Check (Wire.Length = 4 and then Wire.Words (0) = 3, "incomplete pool cannot open");
         end if;
      end loop;
      Expect (Encode_Attachment ((Grants (1), (32, 32, 128))), DP.Bad_State, "no legacy pool mixing");
      Wire := Send (P.Encode_Open);
      Check (Wire.Label = P.Open_Session and then Wire.Length = 4 and then Wire.Flags = 0 and then
        Wire.Reserved = 0 and then Wire.Words (0) = 0 and then Wire.Words (1) /= 0 and then
        Wire.Words (2) = 3 and then Wire.Words (3) = 1, "pool session contract");
      if not Passed then return; end if;
      Session := Wire.Words (1);
      Wire := Send (P.Encode_Open);
      Check (Wire.Words (0) = 3, "live pool cannot reopen");
      for ID in 1 .. 9 loop
         declare
            B : constant P.Buffer_Slot := 1 + (ID - 1) mod 3;
            Pixels : array (0 .. 1023) of Unsigned_32
              with Import, Address => To_Address (Addresses (B)), Volatile;
         begin
            Pixels := [others => 16#FF000000# + Unsigned_32 (ID) * 16#00010101#];
            Pool_Frame (B, Unsigned_64 (ID), Published, Released);
            if not Passed then return; end if;
         end;
      end loop;
      Pool_Frame (1, 9, Rejected, Not_Acquired); -- replay does not consume anything
      Pool_Frame (1, 10, Rejected, Not_Acquired, (0, 0, 33, 32));
      MG.Revoke (Grants (2), Ok);
      Check (Ok and then Generation (Grants (2)) = Grants (2).generation, "registration retains revoked pin");
      Pool_Frame (2, 10, Rejected, Not_Acquired); -- registration is not renewed authority
      Pool_Frame (3, 11, Published, Released);
      if not Passed then return; end if;
      Expect (Encode_Lease_Request (Release_Display), DP.Success, "pool release");
      for B in P.Buffer_Slot loop
         if B /= 2 then MG.Revoke (Grants (B), Ok); Check (Ok, "pool revoke"); end if;
         Check (Generation (Grants (B)) = 0, "all registration and frame pins returned");
      end loop;
      if Passed then debugPrint ("DISPLAY-POOL-CHECK: PASS three slots, exact replies, revocation and release" & ASCII.LF); end if;
   end Exercise_Pool;

   --  Display planes: two cursors (primary and agent pointer). On virtio-gpu
   --  the primary takes the head's single hardware cursor plane and the
   --  agent pointer is composited; a priority swap waits for a plan frame.
   --  Firmware framebuffers report no planes: both are composited.
   procedure Exercise_Planes is
      package DPL renames CuBit.Display_Planes;
      package PP renames CuBit.Display_Plane_Protocol;
      package P renames CuBit.Display_Pool_Protocol;
      use type PP.Request_Set, DPL.Plane_Count, DPL.Plan_Epoch, DPL.Request_Count,
        DPL.Surface_Extent, DP.Status_Code;
      Image_Extent : constant := 32;
      Image_Pages : constant := 1;
      Pause_Ms : constant := 3000;
      Hardware_Expected : Boolean;
      Images : array (DPL.Request_Id range 1 .. 2) of MG.Grant_Reference;
      Colors : constant array (DPL.Request_Id range 1 .. 2) of Unsigned_32 :=
        [16#FFFF_00FF#, 16#FF00_FF00#];
      Last : PP.Report;
      function Only (R : DPL.Request_Id) return PP.Request_Set is
        ([for X in DPL.Request_Id => X = R]);
      function Both return PP.Request_Set is
        ([for X in DPL.Request_Id => X in 1 .. 2]);
      procedure Plane (Request : Wire_Message; Kind : PP.Operation;
                       Status : DP.Status_Code; Name : String) is
         Decoded : constant PP.Report_Decoding :=
           PP.Decode_Report (Kind, Send (Request));
      begin
         Check (Decoded.Valid and then Decoded.Value.Status = Status, Name);
         if Decoded.Valid then Last := Decoded.Value; end if;
      end Plane;
      function Status_Word (Value : Wire_Message) return Unsigned_64 is
        (Value.Words (0));
      Status : Wire_Message;
      Start, Finish : Unsigned_64;
      Move_Count : constant := 64;
   begin
      Status := Send ((Label => Code (Get_Status), others => <>));
      Hardware_Expected := Status_Word (Status) = 3;
      Expect (Encode_Lease_Request (Acquire_Display), DP.Success, "planes lease");
      declare
         C : constant PP.Capability_Decoding :=
           PP.Decode_Capability (Send (PP.Encode_Empty (PP.Query_Output)));
      begin
         --  virtio-gpu's cursor is drawn by the host as its pointer image:
         --  a host-pointer plane, never offered to relative pointers.
         Check (C.Valid and then C.Value.Planes (DPL.Cursor) = 0 and then
                C.Value.Host_Pointer_Cursors = (if Hardware_Expected then 1 else 0) and then
                C.Value.Capacity = DPL.Request_Capacity, "plane capability");
         if C.Valid and then Hardware_Expected then
            Check (C.Value.Max_Width = 64 and then C.Value.Max_Height = 64,
                   "virtio cursor limit");
         end if;
      end;
      --  Both pointers are absolute (as a tablet or remote session would be).
      Plane (PP.Encode (PP.Create_Request, PP.Identity'(1, DPL.Cursor, DPL.Primary_Pointer, True)),
             PP.Create_Request, DP.Success, "create primary pointer");
      Plane (PP.Encode (PP.Create_Request, PP.Identity'(2, DPL.Cursor, DPL.Agent_Pointer, True)),
             PP.Create_Request, DP.Success, "create agent pointer");
      Plane (PP.Encode (PP.Create_Request, PP.Identity'(2, DPL.Cursor, DPL.Agent_Pointer, True)),
             PP.Create_Request, DP.Bad_State, "duplicate request rejected");
      Plane (PP.Encode (PP.Create_Request, PP.Identity'(3, DPL.Overlay, DPL.Agent_Pointer, False)),
             PP.Create_Request, DP.Unsupported, "overlay requests not built yet");
      for R in Images'Range loop
         Raw := syscall (SYSCALL_SBRK, 2 * 4096);
         Check (Raw /= Unsigned_64'Last, "cursor image storage");
         if not Passed then return; end if;
         declare
            Base : constant Integer_Address :=
              Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
            Pixels : array (0 .. Image_Extent * Image_Extent - 1) of Unsigned_32
              with Import, Address => To_Address (Base), Volatile;
         begin
            --  Solid square with a transparent 8-pixel hole at the hotspot.
            for Y in 0 .. Image_Extent - 1 loop
               for X in 0 .. Image_Extent - 1 loop
                  Pixels (Y * Image_Extent + X) :=
                    (if X < 8 and then Y < 8 then 0 else Colors (R));
               end loop;
            end loop;
            MG.Create_Via_Capability (CAP_SLOT_DISPLAY, To_Address (Base),
                                      Image_Pages, False, Images (R), Ok);
            Check (Ok, "cursor image grant");
         end;
         if not Passed then return; end if;
         Plane (PP.Encode (PP.Cursor_Image'(R, Images (R), Image_Extent, Image_Extent, 4, 4)),
                PP.Set_Cursor_Image, DP.Success, "set cursor image");
      end loop;
      Plane (PP.Encode (PP.Move'(1, 300, 200)), PP.Move_Request, DP.Success, "move primary");
      Plane (PP.Encode (PP.Move'(2, 600, 400)), PP.Move_Request, DP.Success, "move agent");
      Plane (PP.Encode (PP.Visibility'(1, True)), PP.Set_Visibility, DP.Success, "show primary");
      Plane (PP.Encode (PP.Visibility'(2, True)), PP.Set_Visibility, DP.Success, "show agent");
      if Hardware_Expected then
         Check (Last.Committed_Hardware = Only (1) and then
                Last.Committed_Composited = Only (2),
                "primary on the plane, agent composited");
      else
         Check (Last.Committed_Hardware = PP.No_Requests and then
                Last.Committed_Composited = Both, "no planes: all composited");
      end if;
      if not Passed then return; end if;
      --  A relative pointer (a mouse) never takes a host-pointer plane, even
      --  at the highest priority: the host would hide it under a grab.
      Plane (PP.Encode (PP.Create_Request, PP.Identity'(3, DPL.Cursor, DPL.Primary_Pointer, False)),
             PP.Create_Request, DP.Success, "create relative pointer");
      Plane (PP.Encode (PP.Cursor_Image'(3, Images (1), Image_Extent, Image_Extent, 4, 4)),
             PP.Set_Cursor_Image, DP.Success, "relative pointer image");
      Plane (PP.Encode (PP.Visibility'(3, True)), PP.Set_Visibility, DP.Success, "show relative pointer");
      Check (Last.Committed_Composited (3) and then not Last.Committed_Hardware (3) and then
             Last.Committed_Hardware = (if Hardware_Expected then Only (1) else PP.No_Requests),
             "relative pointer composited");
      Plane (PP.Encode_Destroy (3), PP.Destroy_Request, DP.Success, "destroy relative pointer");
      if not Passed then return; end if;
      debugPrint ("DISPLAY-PLANES: shown primary=300,200 agent=600,400" & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, Pause_Ms);
      Start := syscall (SYSCALL_GETTIME);
      for Step in 1 .. Move_Count loop
         Plane (PP.Encode (PP.Move'(1, DPL.Space_Coordinate (300 + Step * 400 / Move_Count),
                                       DPL.Space_Coordinate (200 + Step * 300 / Move_Count))),
                PP.Move_Request, DP.Success, "pointer motion");
      end loop;
      Finish := syscall (SYSCALL_GETTIME);
      debugPrint ("DISPLAY-PLANES: moved primary=700,500 moves=" & Move_Count'Image &
                  " ms=" & Unsigned_64'Image (Finish - Start) & ASCII.LF);
      Ignored := syscall (SYSCALL_SLEEP, Pause_Ms);
      --  Negative positions relative to the hotspot: partly off the top-left.
      Plane (PP.Encode (PP.Move'(1, 1, 1)), PP.Move_Request, DP.Success, "edge motion");
      Check (Last.Committed_Hardware = (if Hardware_Expected then Only (1) else PP.No_Requests),
             "partly visible cursor keeps its plane");
      --  Swap priorities: the swap waits for a frame rendered for the plan.
      Plane (PP.Encode (PP.Set_Priority, PP.Identity'(1, DPL.Cursor, DPL.Agent_Pointer, False)),
             PP.Set_Priority, DP.Success, "demote primary");
      Plane (PP.Encode (PP.Set_Priority, PP.Identity'(2, DPL.Cursor, DPL.Primary_Pointer, False)),
             PP.Set_Priority, DP.Success, "promote agent");
      if Hardware_Expected then
         Check (Last.Proposed_Hardware = Only (2) and then
                Last.Committed_Hardware = Only (1), "swap waits for a frame");
         declare
            Epoch : constant DPL.Plan_Epoch := Last.Epoch;
            Grant : MG.Grant_Reference;
            Session : Unsigned_64;
            C : CompletionEntry := NULL_COMPLETION;
            Deadline : Unsigned_64;
            Activity : Activity_Result;
         begin
            for B in P.Buffer_Slot loop
               Raw := syscall (SYSCALL_SBRK, 8192);
               Check (Raw /= Unsigned_64'Last, "plan frame storage");
               if not Passed then return; end if;
               MG.Create_Via_Capability
                 (CAP_SLOT_DISPLAY, To_Address (Integer_Address ((Raw + 4095) and not Unsigned_64'(4095))),
                  1, False, Grant, Ok);
               Expect (P.Encode (P.Attachment'(B, (Grant, (32, 32, 128)))),
                       DP.Success, "plan frame slot");
            end loop;
            Wire := Send (P.Encode_Open);
            Check (Wire.Words (0) = 0, "plan frame session");
            Session := Wire.Words (1);
            if not Passed then return; end if;
            Frame_Token := Frame_Token + 1;
            Check (capSubmit (CAP_SLOT_DISPLAY, From_Wire (PP.Encode (PP.Plan_Frame'
                     ((1, (Session, 1, (0, 0, 32, 32))), Epoch))), Frame_Token),
                   "plan frame submit");
            Deadline := syscall (SYSCALL_GETTIME) + 5000;
            loop
               Ignored := Poll_Completion (C'Address);
               exit when Ignored = 1;
               Activity := Wait_For_Activity_Until (Deadline);
               if Activity /= Work_Available or else syscall (SYSCALL_GETTIME) >= Deadline then
                  Check (False, "plan frame completion deadline"); return;
               end if;
            end loop;
            declare
               R : constant P.Completion_Decoding := P.Decode_Completion (To_Wire (C.msg));
            begin
               Check (R.Valid and then R.Value.Result.Outcome = Published,
                      "plan frame published");
            end;
            Plane (PP.Encode_Empty (PP.Get_Plan), PP.Get_Plan, DP.Success, "plan query");
            Check (Last.Committed_Hardware = Only (2) and then
                   Last.Committed_Composited = Only (1),
                   "frame committed the swap");
         end;
         if not Passed then return; end if;
         debugPrint ("DISPLAY-PLANES: swapped agent=600,400 on the plane" & ASCII.LF);
         Ignored := syscall (SYSCALL_SLEEP, Pause_Ms);
      end if;
      Plane (PP.Encode (PP.Visibility'(2, False)), PP.Set_Visibility, DP.Success, "hide agent");
      Plane (PP.Encode_Destroy (1), PP.Destroy_Request, DP.Success, "destroy primary");
      Plane (PP.Encode_Destroy (1), PP.Destroy_Request, DP.Bad_State, "destroy twice");
      Expect (Encode_Lease_Request (Release_Display), DP.Success, "planes release");
      for R in Images'Range loop
         MG.Revoke (Images (R), Ok);
         Check (Ok and then Generation (Images (R)) = 0, "image grants returned");
      end loop;
      if Passed then
         debugPrint ("DISPLAY-PLANES-CHECK: PASS " &
                     (if Hardware_Expected then "hardware" else "composited") & ASCII.LF);
      end if;
   end Exercise_Planes;

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
      if Passed then Exercise_Pool; end if;
      if Passed then Exercise_Planes; end if;
   end if;
   if Passed then
      debugPrint ("DISPLAY-GRANTS-CHECK: PASS" & ASCII.LF);
   end if;
   Ignored := syscall (SYSCALL_EXIT, (if Passed then 0 else 1));
end Main;
