with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Messages;
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
      Message_Value.tag := capCall (CAP_SLOT_DESKTOP, Message_Value, CuBit.Messages.Wait_Forever);
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
      if B.Policy.Mode /= P.Empty or else Bytes = 0 or else Bytes > DP.Maximum_Buffer_Bytes then
         return;
      end if;
      B.Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Unsigned_64 (Bytes));
      if B.Base = 0 then return; end if;
      B.Bytes := ((Bytes - 1) / 4096 + 1) * 4096;
      P.Allocate (B.Policy);
      MG.Create_Via_Capability
        (CAP_SLOT_DESKTOP, To_Address (Integer_Address (B.Base)), B.Bytes / 4096,
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
      if Released then B.Base := 0; B.Bytes := 0; end if;
   end Release;
end Client_Frame_Buffer;
