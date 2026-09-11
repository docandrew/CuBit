with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages; use CuBit.Desktop_Messages;
procedure Main is
   Passed : Boolean := True;
   Wire, Response : Wire_Message;
   Created : Creation_Result;
   Names : array (Positive range 1 .. 128) of Surface_Name := [others => 0];
   Count : Natural := 0;
   Exhausted : Boolean := False;
   package MG renames CuBit.Memory_Grants;
   Held : MG.Grant_Reference;
   Has_Held : Boolean := False;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Passed := False;
         debugPrint ("TEST: FAIL desktop protocol " & Name & ASCII.LF);
      end if;
   end Check;
   function Send (Wire : Wire_Message) return Wire_Message is
      Msg : Message := From_Wire (Wire);
   begin
      Msg.tag := capCall (CAP_SLOT_DESKTOP, Msg);
      return To_Wire (Msg);
   end Send;
   procedure Check_Control_Boundaries (Owned : Live_Surface_Name) is
      Fixture : Creation_Result;
      Request, Result : Wire_Message;
      Controls : constant array (Positive range 1 .. 2) of Operation :=
        [Set_Pointer_Cursor, Destroy_Surface];
   begin
      for Op of Controls loop
         Request := (Code (Op), 4, 0, 0, [1, 0, 0, 0]);
         Result := Send (Request);
         Check (Decode_Status (Result, Op) = Denied,
                "foreign control denied");
         Request.Words (0) := Unsigned_64'Last;
         Result := Send (Request);
         Check (Decode_Status (Result, Op) = Bad_Object,
                "missing control target");
      end loop;
      for Style in Cursor_Style loop
         Result := Send (Encode_Cursor ((Owned, Style)));
         Check (Decode_Status (Result, Set_Pointer_Cursor) = Success,
                "owned cursor style accepted");
      end loop;
      for Malformation in 1 .. 6 loop
         Request := (Code (Set_Pointer_Cursor), 4, 0, 0,
                     [Unsigned_64 (Owned), 1, 0, 0]);
         case Malformation is
            when 1 => Request.Length := 1;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (2) := 1;
            when 5 => Request.Words (3) := 1;
            when 6 => Request.Words (1) := Unsigned_64'Last;
         end case;
         Result := Send (Request);
         Check (Result = Encode_Status (Set_Pointer_Cursor, Invalid_Request),
                "malformed cursor rejected");

         Fixture := Decode_Creation_Result
           (Send (Encode_Create ((320, 200, Plain_Surface))));
         Check (Fixture.Status = Success, "destroy boundary fixture");
         if Fixture.Status = Success then
            Request.Label := Code (Destroy_Surface);
            Request.Words (0) := Unsigned_64 (Fixture.Surface);
            Request.Words (1) := (if Malformation = 6 then 1 else 0);
            Result := Send (Request);
            Check (Result = Encode_Status (Destroy_Surface, Invalid_Request),
                   "malformed destroy rejected");
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Result.Words (0) = 0, "malformed destroy preserves surface");
            Result := Send ((Code (Destroy_Surface), 4, 0, 0,
                             [Unsigned_64 (Fixture.Surface), 0, 0, 0]));
            Check (Decode_Status (Result, Destroy_Surface) = Success,
                   "valid destroy after rejection");
            Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            Check (Decode_Status (Result, Destroy_Surface) = Bad_Object,
                   "double destroy rejected");
         end if;
      end loop;
      Result := Send (Encode_Cursor ((Owned, Default_Cursor)));
      Check (Decode_Status (Result, Set_Pointer_Cursor) = Success,
             "cursor restored after malformed requests");
   end Check_Control_Boundaries;
   procedure Check_Title_Boundaries (Owned : Live_Surface_Name) is
      Request, Result : Wire_Message;
   begin
      for Malformation in 1 .. 6 loop
         -- One byte, 'A', with otherwise zero padding.
         Request := (Code (Set_Window_Title), 4, 0, 0,
                     [Unsigned_64 (Owned), 65, 0, 2 ** 56]);
         case Malformation is
            when 1 => Request.Length := 3;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (3) := 24 * 2 ** 56;
            when 5 => Request.Words (1) := 65 + 66 * 256;
            when 6 => Request.Words (3) := 1 + 2 ** 56;
         end case;
         Result := Send (Request);
         Check (Result = Encode_Status (Set_Window_Title, Invalid_Request),
                "malformed title rejected");
      end loop;
      Request := (Code (Set_Window_Title), 4, 0, 0, [1, 0, 0, 0]);
      Check (Decode_Status (Send (Request), Set_Window_Title) = Denied,
             "foreign title denied");
      Request.Words (0) := Unsigned_64'Last;
      Check (Decode_Status (Send (Request), Set_Window_Title) = Bad_Object,
             "missing title target");
      for Size in Title_Length loop
         Result := Send (Encode_Title ((Owned, (Size, [others => 'A']))));
         Check (Result = Encode_Status (Set_Window_Title, Success),
                "valid title length accepted after malformed traffic");
      end loop;
      Result := Send (Encode_Title ((Owned, Make_Title (""))));
      Check (Result = Encode_Status (Set_Window_Title, Success),
             "empty title clears custom caption");
   end Check_Title_Boundaries;
   procedure Check_Session_Boundaries is
      Request, Result : Wire_Message;
      Fixture : Creation_Result;
   begin
      for Fault in 1 .. 4 loop
         Request := (Code (Hello), 4, 0, 0, [2 ** 32, 0, 0, 0]);
         case Fault is
            when 1 => Request.Length := 1;
            when 2 => Request.Flags := 1;
            when 3 => Request.Reserved := 1;
            when 4 => Request.Words (1) := 1;
         end case;
         Result := Send (Request);
         Check (Result = (Code (Hello), 2, 0, 0, [0, 4, 0, 0]),
                "malformed hello rejected");
         Request := (Code (Get_Information), Request.Length,
                     Request.Flags, Request.Reserved, [0, 0, 0, 0]);
         if Fault = 4 then Request.Words (0) := 1; end if;
         Result := Send (Request);
         Check (Result = (Code (Get_Information), 2, 0, 0, [0, 4, 0, 0]),
                "malformed information query rejected");

         Fixture := Decode_Creation_Result
           (Send (Encode_Create ((320, 200, Plain_Surface))));
         Check (Fixture.Status = Success, "goodbye boundary fixture");
         if Fixture.Status = Success then
            Request.Label := Code (Goodbye);
            Result := Send (Request);
            Check (Result = Encode_Status (Goodbye, Invalid_Request),
                   "malformed goodbye rejected");
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Result.Words (0) = 0, "malformed goodbye preserves surface");
            Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
         end if;
      end loop;
      Result := Send ((Code (Hello), 4, 0, 0, [Unsigned_64'Last, 0, 0, 0]));
      Check (Result = (Code (Hello), 2, 0, 0, [0, 5, 0, 0]),
             "unsupported desktop version rejected");
      Check (Decode_Hello_Result (Send (Encode_Hello (Current_Revision))).Status = Success,
             "supported handshake after malformed traffic");
      Check (Decode_Information_Result (Send (Encode_Empty_Request (Get_Information))).Status = Success,
             "checked display information");
      Fixture := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
      Check (Fixture.Status = Success, "session cleanup fixture");
      if Fixture.Status = Success then
         declare
            Other : constant Creation_Result :=
              Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
            Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
            Grant : MG.Grant_Reference;
            Acquired : Boolean := False;
            Ok : Boolean;
         begin
            Check (Other.Status = Success, "second session surface");
            if Raw /= Unsigned_64'Last then
               declare
                  Address : constant Integer_Address :=
                    Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
                  Pixels : array (Natural range 0 .. 1023) of Unsigned_32
                    with Address => To_Address (Address), Volatile;
               begin
                  Pixels := [others => 16#FF80_8080#];
                  MG.Create_Via_Capability (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Grant, Ok);
                  Check (Ok, "session cleanup grant");
                  if Ok then
                     Result := Send (Encode_Attachment ((Fixture.Surface, Grant, (4, 2, 16))));
                     Acquired := Decode_Status (Result, Attach_Buffer) = Success;
                     Check (Acquired, "session cleanup acquisition");
                     MG.Revoke (Grant, Ok);
                     Check (Ok, "session cleanup pending revocation");
                     Request := Encode_Empty_Request (Goodbye); Request.Reserved := 1;
                     Check (Send (Request) = Encode_Status (Goodbye, Invalid_Request),
                            "invalid goodbye during pending revocation rejected");
                     Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Grant.slot) = Grant.generation,
                            "invalid goodbye retains acquisition");
                  end if;
               end;
            else
               Check (False, "session cleanup allocation");
            end if;
            Check (Send (Encode_Empty_Request (Goodbye)) = Encode_Status (Goodbye, Success),
                   "valid goodbye acknowledged");
            if Acquired then
               Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION, Grant.slot) = Unsigned_64'Last,
                      "goodbye releases pending acquisition");
            end if;
            Result := Send (Encode_Present ((Fixture.Surface, (0, 0, 0, 0))));
            Check (Decode_Status (Result, Present_Surface) = Bad_Object, "goodbye removes first owned surface");
            if Other.Status = Success then
               Result := Send (Encode_Present ((Other.Surface, (0, 0, 0, 0))));
               Check (Decode_Status (Result, Present_Surface) = Bad_Object, "goodbye removes second owned surface");
            end if;
            Result := Send (Encode_Present ((1, (0, 0, 0, 0))));
            Check (Decode_Status (Result, Present_Surface) = Denied, "goodbye preserves foreign surface");
            Check (Send (Encode_Empty_Request (Goodbye)) = Encode_Status (Goodbye, Success),
                   "repeated goodbye is harmless");
         end;
      end if;
   end Check_Session_Boundaries;
   procedure Check_Input_Boundaries is
      Operations : constant array (Positive range 1 .. 2) of Operation := [Poll_Input, Wait_Input];
      Fixture : Creation_Result;
      Request, Result : Wire_Message;
   begin
      for Op of Operations loop
         for Fault in 1 .. 4 loop
            Fixture := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
            Check (Fixture.Status = Success, "input validation fixture");
            if Fixture.Status = Success then
               Request := (Code (Op), 4, 0, 0,
                           [Unsigned_64 (Fixture.Surface), 0, (if Op = Wait_Input then 1 else 0), 0]);
               case Fault is
                  when 1 => Request.Length := 3;
                  when 2 => Request.Flags := 1;
                  when 3 => Request.Reserved := 1;
                  when 4 => Request.Words (3) := 1;
               end case;
               Check (Send (Request) = Encode_Status (Op, Invalid_Request), "malformed input rejected");
               Result := Send ((Code (Poll_Input), 4, 0, 0, [Unsigned_64 (Fixture.Surface), 0, 0, 0]));
               Check (Result.Length = 4 and then Result.Words (0) = 8,
                      "malformed input preserves queued configure");
               Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            end if;
         end loop;
         Request := (Code (Op), 4, 0, 0, [1, 0, (if Op = Wait_Input then 1 else 0), 0]);
         Check (Send (Request) = Encode_Status (Op, Denied), "foreign input denied");
         Request.Words (0) := Unsigned_64'Last;
         Check (Send (Request) = Encode_Status (Op, Bad_Object), "missing input target");
      end loop;
      Fixture := Decode_Creation_Result
        (Send (Encode_Create ((320, 200, Plain_Surface))));
      Check (Fixture.Status = Success, "input waiter fixture");
      if Fixture.Status = Success then
         declare
            Event : Input_Result;
            Serial, Started, Deadline, Finished : Unsigned_64;
         begin
            Event := Decode_Input_Result
              (Send (Encode_Input_Request ((Poll_Input, Fixture.Surface, 0))), Poll_Input);
            Check (Event.Status = Success and then Event.Value.Kind = Surface_Configured,
                   "initial configure before deferred wait");
            if Event.Status = Success then
               Serial := Event.Value.Serial;
               --  Reuse the same channel: each timeout must consume its saved
               --  reply and leave the channel able to install another waiter.
               for Attempt in 1 .. 4 loop
                  Started := syscall (SYSCALL_GETTIME);
                  if Started > Unsigned_64'Last - 20 then
                     Check (False, "waiter monotonic clock available");
                     exit;
                  end if;
                  Deadline := Started + 20;
                  Event := Decode_Input_Result
                    (Send (Encode_Input_Request
                       ((Wait_Input, Fixture.Surface, Serial, Deadline))), Wait_Input);
                  Finished := syscall (SYSCALL_GETTIME);
                  Check (Event.Status = Success and then
                         Event.Value.Kind = No_Input and then
                         Event.Value.Serial = Serial,
                         "deferred wait expires without fabricated input");
                  Check (Finished >= Deadline and Finished /= Unsigned_64'Last,
                         "deferred wait does not return before deadline");
               end loop;
               Check (Decode_Resize_Result
                        (Send (Encode_Resize ((Fixture.Surface, 321, 201)))).Status = Success,
                      "configure after repeated timeout");
               Event := Decode_Input_Result
                 (Send (Encode_Input_Request
                    ((Wait_Input, Fixture.Surface, Serial, 0))), Wait_Input);
               Check (Event.Status = Success and then
                      Event.Value.Kind = Surface_Configured and then
                      Event.Value.Serial > Serial,
                      "queued input delivered after repeated waiter reuse");
            end if;
            Request := Encode_Input_Request ((Poll_Input, Fixture.Surface, 0));
            Request.Words (2) := 1;
            Check (Send (Request) = Encode_Status (Poll_Input, Invalid_Request),
                   "poll deadline rejected");
            Request.Words := [others => 0];
            Check (Send (Request) = Encode_Status (Poll_Input, Invalid_Request),
                   "zero input surface rejected");
            if Event.Status = Success then
               declare
                  Wait_Token : constant Unsigned_64 := 16#D350_0001#;
                  Bad_Token : constant Unsigned_64 := 16#D350_0002#;
                  Destroy_Token : constant Unsigned_64 := 16#D350_0003#;
                  Entries : CompletionRing := [others => NULL_COMPLETION];
                  Extra : CompletionEntry := NULL_COMPLETION;
                  Received : Unsigned_64;
                  Submitted : Boolean;
               begin
                  -- One async lane preserves submission order without a
                  -- sleep-based assumption that the waiter is installed.
                  Request := Encode_Input_Request
                    ((Wait_Input, Fixture.Surface, Event.Value.Serial, 0));
                  Submitted := capSubmit (CAP_SLOT_DESKTOP, From_Wire (Request), Wait_Token);
                  Request := Encode_Input_Request ((Poll_Input, Fixture.Surface, 0));
                  Request.Length := 3;
                  Submitted := Submitted and then
                    capSubmit (CAP_SLOT_DESKTOP, From_Wire (Request), Bad_Token);
                  Submitted := Submitted and then capSubmit
                    (CAP_SLOT_DESKTOP,
                     From_Wire (Encode_Destroy ((Surface => Fixture.Surface))), Destroy_Token);
                  Check (Submitted, "deferred destruction submitted");
                  if Submitted then
                     -- The headless runner supplies the outer watchdog.
                     Received := waitCompletion (Entries'Address, 3, 3);
                     Check (Received = 3, "all deferred destruction completions");
                     if Received = 3 then
                        for Item of Entries (0 .. 2) loop
                           Check (Item.status = COMPLETION_OK and Item.requestId /= 0,
                                  "deferred completion transport and identity");
                        end loop;
                        Check (Entries (0).token = Bad_Token and then
                               Decode_Status (To_Wire (Entries (0).msg), Poll_Input) = Invalid_Request,
                               "malformed concurrent request rejected independently");
                        Event := Decode_Input_Result (To_Wire (Entries (1).msg), Wait_Input);
                        Check (Entries (1).token = Wait_Token and then
                               Event.Status = Success and then Event.Value.Kind = Input_Resynchronized,
                               "destroy resolves original outstanding waiter");
                        Check (Entries (2).token = Destroy_Token and then
                               Decode_Status (To_Wire (Entries (2).msg), Destroy_Surface) = Success,
                               "destroy acknowledgement follows waiter resolution");
                        Check (Entries (0).requestId /= Entries (1).requestId and
                               Entries (1).requestId /= Entries (2).requestId and
                               Entries (0).requestId /= Entries (2).requestId,
                               "simultaneous requests have distinct identities");
                        Check (Poll_Completion (Extra'Address) = 0,
                               "no duplicate deferred completion");
                     end if;
                  else
                     Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
                  end if;

                  Fixture := Decode_Creation_Result
                    (Send (Encode_Create ((320, 200, Plain_Surface))));
                  Check (Fixture.Status = Success, "channel reuse after waiter destruction");
                  if Fixture.Status = Success then
                     Event := Decode_Input_Result
                       (Send (Encode_Input_Request ((Poll_Input, Fixture.Surface, 0))), Poll_Input);
                     if Event.Status = Success then
                        Started := syscall (SYSCALL_GETTIME);
                        Deadline := Started + 20;
                        Event := Decode_Input_Result
                          (Send (Encode_Input_Request
                             ((Wait_Input, Fixture.Surface, Event.Value.Serial, Deadline))), Wait_Input);
                        Check (Event.Status = Success and then Event.Value.Kind = No_Input and then
                               syscall (SYSCALL_GETTIME) >= Deadline,
                               "destroyed waiter slot accepts another deferred wait");
                     else
                        Check (False, "reused channel initial input");
                     end if;
                     Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
                     Check (Decode_Status (Result, Destroy_Surface) = Success,
                            "reused channel fixture released");
                  end if;
               end;
            else
               Result := Send (Encode_Destroy ((Surface => Fixture.Surface)));
            end if;
         end;
      end if;
   end Check_Input_Boundaries;
begin
   Wire := Encode_Create ((320, 200, Plain_Surface));
   Wire.Words (0) := Unsigned_64'Last;
   Response := Send (Wire);
   Check (Response.Words (0) = 0 and Decode_Creation_Result (Response).Status = Invalid_Request, "oversized width");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Length := 3;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "short header");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Reserved := 1;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "reserved header");
   Wire := Encode_Create ((320, 200, Plain_Surface)); Wire.Words (2) := Unsigned_64'Last;
   Check (Decode_Creation_Result (Send (Wire)).Status = Invalid_Request, "unknown kind");
   Created := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
   Check (Created.Status = Success, "create after malformed traffic");
   if Created.Status /= Success then return; end if;
   Count := 1; Names (1) := Created.Surface;
   Check_Control_Boundaries (Created.Surface);
   Check_Title_Boundaries (Created.Surface);
   Response := Send (Encode_Present ((Created.Surface, (0, 0, 10, 10))));
   Check (Response.Words (0) = 0, "own present");
   Wire := Encode_Present ((Created.Surface, (0, 0, 10, 10)));
   Wire.Words (1) := Unsigned_64'Last;
   Response := Send (Wire);
   Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "oversized damage");
   Response := Send (Encode_Present ((Created.Surface, (65_535, 65_535, 65_535, 65_535))));
   Check (Response.Words (0) = 0, "out-of-surface damage safely clipped");
   -- This test profile boots the internal desktop shell as surface 1 before
   -- this app. Its name is deliberately known; it confers no authority.
   Check (Created.Surface /= 1, "foreign surface fixture");
   Response := Send (Encode_Present ((1, (0, 0, 0, 0))));
   Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "foreign present denied");
   Response := Send (Encode_Present ((Live_Surface_Name'Last, (0, 0, 0, 0))));
   Check (Response.Words (0) = Status_Code'Enum_Rep (Bad_Object), "missing surface");
   declare
      Limits : Limits_Request :=
        (Created.Surface, (120, 80, 400, 300), [others => False]);
      Resized : Resize_Result;
      Applied : Limits_Result;
   begin
      Check (Decode_Resize_Result (Send (Encode_Resize ((1, 320, 200)))).Status = Denied,
             "foreign resize denied");
      Check (Decode_Resize_Result (Send (Encode_Resize ((Live_Surface_Name'Last, 320, 200)))).Status = Bad_Object,
             "missing resize surface");
      Wire := Encode_Limits (Limits); Wire.Words (0) := 1;
      Check (Decode_Limits_Result (Send (Wire)).Status = Denied, "foreign limits denied");
      Wire.Words (0) := Unsigned_64'Last;
      Check (Decode_Limits_Result (Send (Wire)).Status = Bad_Object, "missing limits surface");
      Applied := Decode_Limits_Result (Send (Encode_Limits (Limits)));
      Check (Applied.Status = Success and then Applied.Bounds = Limits.Bounds,
             "typed limits applied");
      for Field in 1 .. 2 loop
         Wire := Encode_Resize ((Created.Surface, 320, 200));
         Wire.Words (Field) := Unsigned_64'Last;
         Check (Decode_Resize_Result (Send (Wire)).Status = Invalid_Request,
                "oversized resize rejected");
         Wire := Encode_Limits (Limits); Wire.Words (Field) := Unsigned_64'Last;
         Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request,
                "oversized limits rejected");
      end loop;
      Wire := Encode_Resize ((Created.Surface, 320, 200)); Wire.Length := 3;
      Check (Decode_Resize_Result (Send (Wire)).Status = Invalid_Request, "short resize rejected");
      Wire := Encode_Limits (Limits); Wire.Reserved := 1;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "reserved limits rejected");
      Wire := Encode_Limits (Limits); Wire.Words (3) := 256;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "unknown features rejected");
      Wire := Encode_Limits (Limits); Wire.Words (2) := 119;
      Check (Decode_Limits_Result (Send (Wire)).Status = Invalid_Request, "contradictory limits rejected");
      Resized := Decode_Resize_Result (Send (Encode_Resize ((Created.Surface, 999, 999))));
      Check (Resized.Status = Success and then Resized.Width = 400 and then Resized.Height = 300,
             "rejected limits preserve prior bounds");
      Limits.Bounds := (120, 80, 0, 0);
      Check (Decode_Limits_Result (Send (Encode_Limits (Limits))).Status = Success,
             "unbounded maxima restored");
      Resized := Decode_Resize_Result (Send (Encode_Resize ((Created.Surface, 320, 200))));
      Check (Resized.Status = Success and then Resized.Width = 320 and then Resized.Height = 200,
             "resize after malformed traffic");
   end;
   declare
      Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
      First, Second, Reused : MG.Grant_Reference;
      Ok : Boolean;
      Surface : constant Live_Surface_Name := Created.Surface;
   begin
      Check (Raw /= Unsigned_64'Last, "buffer allocation");
      if Raw /= Unsigned_64'Last then
         declare
            Address : constant Integer_Address :=
              Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
            Pixels : array (Natural range 0 .. 1023) of Unsigned_32
              with Address => To_Address (Address), Volatile;
         begin
            Pixels := [others => 16#FF80_4020#];
            MG.Create_Via_Capability
              (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, First, Ok);
            Check (Ok, "create desktop grant");
            if Ok then
               -- More than the kernel's acquisition limit: each replacement
               -- must return exactly one old acquisition, even for one grant.
               for Attempt in 1 .. 140 loop
                  Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "balanced repeated attachment");
               end loop;
               Response := Send (Encode_Attachment ((Surface, First, (1024, 2, 4096))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "grant too short");
               Response := Send (Encode_Attachment ((1, First, (4, 2, 16))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "foreign attachment");
               Wire := Encode_Attachment ((Surface, First, (4, 2, 16)));
               Wire.Words (2) := 0;
               Response := Send (Wire);
               Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "zero generation");
               Wire := Encode_Attachment ((Surface, First, (4, 2, 16)));
               Wire.Words (3) := Unsigned_64'Last;
               Response := Send (Wire);
               Check (Response.Words (0) = Status_Code'Enum_Rep (Invalid_Request), "hostile buffer layout");
               MG.Revoke (First, Ok);
               Check (Ok, "revoke while attached");
               Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
               Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "pending revoke blocks new attachment");
               Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                               First.slot) = First.generation, "old attachment retained on rejection");
               Response := Send (Encode_Present ((Surface, (0, 0, 0, 0))));
               Check (Response.Words (0) = 0, "present during pending revoke");
               MG.Create_Via_Capability
                 (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Second, Ok);
               Check (Ok, "replacement grant creation");
               if Ok then
                  Response := Send (Encode_Attachment ((Surface, Second, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "replacement acquisition");
                  Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                                  First.slot) = Unsigned_64'Last, "replacement completes old revoke");
                  MG.Create_Via_Capability
                    (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Reused, Ok);
                  Check (Ok and then Reused.slot = First.slot and then
                         Reused.generation /= First.generation, "generation advances on reuse");
                  Response := Send (Encode_Attachment ((Surface, First, (4, 2, 16))));
                  Check (Response.Words (0) = Status_Code'Enum_Rep (Denied), "stale attachment denied");
                  if Ok then MG.Revoke (Reused, Ok); end if;
                  MG.Revoke (Second, Ok);
                  Check (Ok, "second pending revoke");
                  Wire := Encode_Destroy ((Surface => Surface));
                  Wire.Reserved := 1;
                  Response := Send (Wire);
                  Check (Response = Encode_Status (Destroy_Surface, Invalid_Request),
                         "malformed destroy during pending revoke rejected");
                  Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                                  Second.slot) = Second.generation,
                         "malformed destroy retains buffer acquisition");
                  Held := Second;
                  Has_Held := True;
               end if;
            end if;
         end;
      end if;
   end;
   for Attempt in 2 .. Names'Last loop
      Response := Send (Encode_Create ((320, 200, Plain_Surface)));
      Created := Decode_Creation_Result (Response);
      if Created.Status /= Success then
         Check (Created.Status = Resources_Exhausted and Response.Words (0) = 0, "unambiguous exhaustion");
         Exhausted := True;
         exit;
      end if;
      Count := Count + 1;
      Names (Count) := Created.Surface;
   end loop;
   Check (Exhausted, "bounded surface table");
   for Index in 1 .. Count loop
      Response := Send (Encode_Destroy ((Surface => Live_Surface_Name (Names (Index)))));
      Check (Decode_Status (Response, Destroy_Surface) = Success, "release owned surface");
   end loop;
   if Has_Held then
      Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                      Held.slot) = Unsigned_64'Last, "destroy returns held acquisition");
   end if;
   Check_Input_Boundaries;
   Check_Session_Boundaries;
   Created := Decode_Creation_Result (Send (Encode_Create ((320, 200, Plain_Surface))));
   Check (Created.Status = Success, "service usable after exhaustion");
   if Created.Status = Success then
      declare
         Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, 8192);
         Grant : MG.Grant_Reference;
         Ok : Boolean;
      begin
         Check (Raw /= Unsigned_64'Last, "exit fixture allocation");
         if Raw /= Unsigned_64'Last then
            declare
               Address : constant Integer_Address :=
                 Integer_Address ((Raw + 4095) and not Unsigned_64'(4095));
               Pixels : array (Natural range 0 .. 1023) of Unsigned_32
                 with Address => To_Address (Address), Volatile;
            begin
               Pixels := [others => 16#FF40_8040#];
               MG.Create_Via_Capability
                 (CAP_SLOT_DESKTOP, To_Address (Address), 1, False, Grant, Ok);
               Check (Ok, "exit fixture grant");
               if Ok then
                  Response := Send (Encode_Attachment
                    ((Created.Surface, Grant, (4, 2, 16))));
                  Check (Response.Words (0) = 0, "exit fixture attachment");
               end if;
            end;
         end if;
      end;
   end if;
   -- Deliberately exit with the final buffer acquired. The headless driver
   -- requests a repaint after exit and requires the desktop's reap marker.
   if Passed then debugPrint ("DESKTOP-PROTOCOL-CHECK: PASS" & ASCII.LF); end if;
end Main;
