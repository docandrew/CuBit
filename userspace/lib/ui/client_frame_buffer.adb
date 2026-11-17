with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Messages;
with UI_Desktop_Service;
package body Client_Frame_Buffer is
   package P renames Client_Frame_State;
   package MG renames CuBit.Memory_Grants;
   use type P.Phase;
   use type DP.Status_Code;
   use type DP.Wire_Message;
   use type Pub.Receipt;
   function Call (Request : DP.Wire_Message) return DP.Wire_Message is
      Message_Value : Message := CuBit.Desktop_Messages.From_Wire (Request);
   begin
      -- capCall authenticates the reply through the endpoint invocation.
      Message_Value.tag := capCall (UI_Desktop_Service.Endpoint, Message_Value, CuBit.Messages.Wait_Forever);
      return CuBit.Desktop_Messages.To_Wire (Message_Value);
   end Call;
   procedure Receipt
     (Request : DP.Wire_Message; Label : Pub.Receipt_Label;
      Result : out Pub.Receipt; Canonical : out Boolean)
   is
      Wire : constant DP.Wire_Message := Call (Request);
   begin
      Result := Pub.Decode_Receipt (Wire, Label);
      Canonical := Wire = Pub.Encode_Receipt (Result, Label);
   end Receipt;
   function Writable_Address (B : Buffer) return System.Address is
     (if B.Policy.Mode = P.Writable then To_Address (Integer_Address (B.Base))
      else System.Null_Address);
   function Capacity (B : Buffer) return Natural is (B.Bytes);
   procedure Allocate (B : in out Buffer; Bytes : Natural; OK : out Boolean) is
      Cleaned : Boolean;
   begin
      OK := False;
      --  The kernel refuses an allocation beyond this process's memory quota.
      if B.Policy.Mode /= P.Empty or else Bytes = 0 then
         return;
      end if;
      B.Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Unsigned_64 (Bytes));
      if B.Base = 0 then return; end if;
      B.Bytes := ((Bytes - 1) / 4096 + 1) * 4096;
      P.Allocate (B.Policy);
      MG.Create_Via_Capability
        (UI_Desktop_Service.Endpoint, To_Address (Integer_Address (B.Base)), B.Bytes / 4096,
         False, B.Grant, B.Has_Grant);
      if not B.Has_Grant then
         Release (B, Cleaned);
         return;
      end if;
      OK := True;
   end Allocate;
   procedure Prepare_Write (B : in out Buffer; Ready : out Boolean) is
      Result : Pub.Receipt;
      Canonical, Protection_OK : Boolean;
   begin
      Ready := False;
      if B.Policy.Mode = P.Held then
         Receipt (Pub.Encode_Query ((B.Surface, Unsigned_64 (B.Policy.Ticket)), True),
                  Pub.Retirement_Label, Result, Canonical);
         if not Canonical then P.Quarantine (B.Policy); return; end if;
         if Result.Status = DP.Bad_State then return; end if;
         if Result.Status /= DP.Success or else Result.Epoch /= Unsigned_64 (B.Policy.Epoch) or else
           Result.Ticket /= Unsigned_64 (B.Policy.Ticket)
         then P.Quarantine (B.Policy); return; end if;
         P.Retire (B.Policy, P.Identity (Result.Epoch), P.Identity (Result.Ticket), True);
      end if;
      if B.Policy.Mode = P.Sealed then
         Protection_OK := syscall (SYSCALL_PROTECT_OWNED_MEMORY, B.Base, Unsigned_64 (B.Bytes), 3) = 0;
         P.Reopen (B.Policy, Protection_OK);
      end if;
      Ready := B.Policy.Mode = P.Writable;
   end Prepare_Write;
   procedure Present
     (B : in out Buffer; Surface : DP.Live_Surface_Name; Epoch : Pub.Identity;
      Area : DP.Rectangle; Accepted : out Boolean;
      Input_After : Interfaces.Unsigned_64 := 0)
   is
      Protection_OK, Canonical : Boolean;
      Staged, Published : Pub.Receipt;
   begin
      Accepted := False;
      if B.Policy.Mode /= P.Writable or else not DP.Valid_Damage (Area) then return; end if;
      Protection_OK := syscall (SYSCALL_PROTECT_OWNED_MEMORY, B.Base, Unsigned_64 (B.Bytes), 1) = 0;
      P.Seal (B.Policy, Protection_OK);
      if not Protection_OK then return; end if;
      Receipt (Pub.Encode_Stage ((Surface, Epoch, B.Grant)), Pub.Stage_Label, Staged, Canonical);
      if not Canonical then P.Quarantine (B.Policy); return; end if;
      -- A canonical stage refusal admitted no loan. Keep the allocation
      -- sealed; a later Prepare_Write may restore access for a fresh frame.
      if Staged.Status /= DP.Success then return; end if;
      if Staged.Epoch /= Epoch then P.Quarantine (B.Policy); return; end if;
      B.Surface := Surface;
      P.Borrow (B.Policy, P.Identity (Staged.Epoch), P.Identity (Staged.Ticket));
      Receipt (Pub.Encode_Publish ((Surface, Epoch, Staged.Ticket, Area, Input_After)),
               Pub.Publish_Label, Published, Canonical);
      if not Canonical then P.Quarantine (B.Policy); return; end if;
      if Published.Status = DP.Success and then Published /= Staged then
         P.Quarantine (B.Policy); return;
      end if;
      Accepted := Published = Staged;
   end Present;
   ---------------------------------------------------------------------------
   --  The surface ring path.
   ---------------------------------------------------------------------------
   package Wire renames Compositor_Surface_Wire;
   use type Wire.Request_Kind;
   use type Client_Surface_Ring.Submit_Result;
   use type Client_Surface_Ring.Token;

   function Awaiting (B : Buffer) return Boolean is (B.Publish_Tag /= 0 or else B.Await_Tag /= 0);

   --  Ask once for B's retirement (answered when Desktop no longer reads it).
   procedure Request_Retirement (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection) is
      Result : Client_Surface_Ring.Submit_Result;
   begin
      if B.Policy.Mode /= P.Held or else B.Await_Tag /= 0 then
         return;
      end if;
      Client_Surface_Ring.Submit
        (Ring, (Kind => Wire.Await_Retirement, Surface => B.Surface,
                Retiring_Epoch => Pub.Identity (B.Policy.Epoch), Ticket => Pub.Identity (B.Policy.Ticket)),
         B.Await_Tag, Result);
      if Result /= Client_Surface_Ring.Queued then
         B.Await_Tag := 0;
      end if;
   end Request_Retirement;

   procedure Prepare_Write
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection; Ready : out Boolean) is
   begin
      Ready := False;
      if B.Policy.Mode = P.Held then
         Request_Retirement (B, Ring);
         return;
      end if;
      if B.Policy.Mode = P.Sealed and then B.Publish_Tag = 0 then
         --  Unprotected on this path: reopening needs no call.
         P.Reopen (B.Policy, True);
      end if;
      Ready := B.Policy.Mode = P.Writable;
   end Prepare_Write;

   procedure Present
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection;
      Surface : DP.Live_Surface_Name; Epoch : Pub.Identity;
      Area : DP.Rectangle; Accepted : out Boolean;
      Input_After : Interfaces.Unsigned_64 := 0)
   is
      Result : Client_Surface_Ring.Submit_Result;
   begin
      Accepted := False;
      if B.Policy.Mode /= P.Writable or else not DP.Valid_Damage (Area) or else Awaiting (B)
        or else not Client_Surface_Ring.Can_Submit (Ring, Wire.Publish)
      then
         return;
      end if;
      P.Seal (B.Policy, True);
      Client_Surface_Ring.Submit
        (Ring, (Kind => Wire.Publish, Surface => Surface, Area => Area, Input_After => Input_After,
                Epoch => Epoch, Grant => B.Grant),
         B.Publish_Tag, Result);
      if Result /= Client_Surface_Ring.Queued then
         B.Publish_Tag := 0;
         P.Reopen (B.Policy, True);
         return;
      end if;
      B.Surface := Surface;
      B.Publish_Epoch := Epoch;
      Accepted := True;
   end Present;

   procedure Note_Answer
     (B : in out Buffer; Ring : in out Client_Surface_Ring.Connection;
      Tag : Client_Surface_Ring.Token; Answer : Compositor_Surface_Wire.Result;
      Matched, Refused : out Boolean) is
   begin
      Matched := False;
      Refused := False;
      if Tag = 0 then
         return;
      end if;
      if Tag = B.Publish_Tag and then Answer.Kind = Wire.Publish then
         Matched := True;
         B.Publish_Tag := 0;
         if Answer.Status = DP.Success and then Answer.Epoch = B.Publish_Epoch
           and then B.Policy.Mode = P.Sealed
         then
            P.Borrow (B.Policy, P.Identity (Answer.Epoch), P.Identity (Answer.Ticket));
            --  Ask now: the answer comes as soon as the next frame replaces
            --  this one and Desktop lets go of it.
            Request_Retirement (B, Ring);
         elsif Answer.Status = DP.Success then
            --  Accepted with an epoch it was not asked for.
            P.Quarantine (B.Policy);
         else
            --  Refused: no loan; the frame was not shown.
            Refused := True;
         end if;
      elsif Tag = B.Await_Tag and then Answer.Kind = Wire.Await_Retirement then
         Matched := True;
         B.Await_Tag := 0;
         if Answer.Status = DP.Success then
            if B.Policy.Mode = P.Held and then Natural (Answer.Epoch) = B.Policy.Epoch
              and then Natural (Answer.Ticket) = B.Policy.Ticket
            then
               P.Retire (B.Policy, P.Identity (Answer.Epoch), P.Identity (Answer.Ticket), True);
            else
               P.Quarantine (B.Policy);
            end if;
         elsif Answer.Status in DP.Bad_Object | DP.Denied | DP.Invalid_Request then
            --  The surface is gone: the loan ends with it (Release).
            P.Quarantine (B.Policy);
         end if;
         --  Otherwise (no room): asked again at the next Prepare_Write.
      end if;
   end Note_Answer;

   procedure Release (B : in out Buffer; Released : out Boolean) is
      Revoked, Retired : Boolean := True;
      pragma Unreferenced (Revoked);
   begin
      Released := B.Policy.Mode = P.Empty;
      if Released then return; end if;
      if B.Has_Grant then
         MG.Revoke (B.Grant, Revoked);
         -- Revoke may already have been accepted on an earlier call. Only
         -- exact kernel retirement confirmation permits backing reclamation.
         Retired := MG.Retirement_Confirmed (B.Grant);
         if Retired then B.Has_Grant := False; end if;
      end if;
      if Retired then
         Released := syscall (SYSCALL_RELEASE_OWNED_MEMORY, B.Base, Unsigned_64 (B.Bytes)) = 0;
      end if;
      P.Release (B.Policy, Retired, Released);
      if Released then
         B.Base := 0; B.Bytes := 0;
         B.Publish_Tag := 0; B.Await_Tag := 0;
      end if;
   end Release;
end Client_Frame_Buffer;
