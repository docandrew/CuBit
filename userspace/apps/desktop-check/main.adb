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
      Response := Send ((Code (Destroy_Surface), 4, 0, 0, [Unsigned_64 (Names (Index)), 0, 0, 0]));
      Check (Response.Words (0) = 0, "release owned surface");
   end loop;
   if Has_Held then
      Check (syscall (SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION,
                      Held.slot) = Unsigned_64'Last, "destroy returns held acquisition");
   end if;
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
