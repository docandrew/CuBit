pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Display_Protocol; use CuBit.Display_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;

--  Dedicated fixture: never starts alongside a desktop session. Only authority
--  is an endpoint to display.svc (including grants addressed to that service).
procedure Main is
   package MG renames CuBit.Memory_Grants;
   Passed : Boolean := True;
   First, Second, Reused : MG.Grant_Reference;
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
   function Generation (Ref : MG.Grant_Reference) return Unsigned_64 is
     (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Ref.slot));
   procedure Frame (Session, ID : Live_ID; Outcome : Frame_Outcome;
                    Disposition : Buffer_Disposition;
                    Area : DP.Rectangle := (0, 0, 4, 2)) is
      Msg : constant Message := From_Wire (Encode_Frame ((Session, ID, Area)));
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
               when 3 => Wire.Reserved := 1;
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
         Check (Generation (First) = Unsigned_64'Last, "replacement returns old pin");
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
               when 3 => Wire.Reserved := 1;
               when 4 => Wire.Words (0) := 1;
            end case;
            Expect (Wire, DP.Bad_Object, "malformed release rejected");
            Check (Generation (Second) = Second.generation, "malformed release retains pin");
         end loop;
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "release lease");
         Check (Generation (Second) = Unsigned_64'Last, "release returns final pin");
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "idempotent release");
         Expect (Encode_Lease_Request (Acquire_Display), DP.Success, "lease reusable");
         Expect ((Code (Present_Rectangle), 4, 0, 0, [0, 0, 4, 2]), DP.Bad_State,
                 "released source pointer cleared");
         Expect (Encode_Lease_Request (Release_Display), DP.Success, "final release");
         if Passed then debugPrint ("DISPLAY-ASYNC-CHECK: PASS" & ASCII.LF); end if;
      end;
   end Exercise;
begin
   Exercise;
   if Passed then
      debugPrint ("DISPLAY-GRANTS-CHECK: PASS" & ASCII.LF);
   end if;
   Ignored := syscall (SYSCALL_EXIT, (if Passed then 0 else 1));
end Main;
