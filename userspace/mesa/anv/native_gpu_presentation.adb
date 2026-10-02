with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
package body Native_GPU_Presentation is
   package M renames CuBit.Messages;
   package G renames CuBit.Memory_Grants;
   package R renames CuBit.Grant_References;
   package D renames CuBit.Desktop_Protocol;
   function Forward
     (Desktop_Slot, Parent, Offset, Bytes : Unsigned_64;
      Output : access Unsigned_64) return Unsigned_32 is
      Child : G.Grant_Reference;
      OK : Boolean;
   begin
      if Output = null then return 1; end if;
      Output.all := 0;
      if Desktop_Slot > Unsigned_64 (M.CapabilitySlot'Last) or else
        not R.Valid_Wire (Parent) or else Bytes = 0 or else
        Bytes > D.Maximum_Buffer_Bytes or else Offset mod 4096 /= 0 or else
        Bytes mod 4096 /= 0 or else Offset > D.Maximum_Buffer_Bytes - Bytes
      then return 1; end if;
      G.Derive_Via_Capability (M.CapabilitySlot (Desktop_Slot), R.Decode (Parent),
        Natural (Offset / 4096), Natural (Bytes / 4096), False, Child, OK);
      if not OK then return 1; end if;
      Output.all := R.Encode (Child);
      return 0;
   end Forward;
   function Attach_Linear
     (Desktop_Slot, Surface, Child, Width, Height, Pitch : Unsigned_64)
      return Unsigned_32 is
      Layout : D.Buffer_Layout;
      Msg : M.Message;
      Expected : M.MessageTag;
      Wire : D.Wire_Message;
      use type M.MessageTag;
      use type D.Wire_Message;
   begin
      if Desktop_Slot > Unsigned_64 (M.CapabilitySlot'Last) or else Surface = 0
        or else not R.Valid_Wire (Child) or else Width not in 1 .. 65535
        or else Height not in 1 .. 65535 or else Pitch > D.Maximum_Buffer_Bytes
      then return 7; end if;
      Layout := (D.Positive_Extent (Width), D.Positive_Extent (Height), Natural (Pitch));
      if not D.Valid_Layout (Layout) then return 7; end if;
      Msg := CuBit.Desktop_Messages.From_Wire
        (D.Encode_Attachment ((D.Live_Surface_Name (Surface), R.Decode (Child), Layout)));
      Expected := M.capCall (M.CapabilitySlot (Desktop_Slot), Msg);
      if Expected /= Msg.tag then return 7; end if;
      Wire := CuBit.Desktop_Messages.To_Wire (Msg);
      -- Decode_Status's Invalid_Request fallback cannot distinguish malformed
      -- transport from a real rejection. Require the exact status envelope.
      for Status in D.Status_Code loop
         if Wire = D.Encode_Status (D.Attach_Buffer, Status) then
            return Unsigned_32 (Status'Enum_Rep);
         end if;
      end loop;
      return 7;
   end Attach_Linear;
   function Retire (Child : Unsigned_64) return Unsigned_32 is
      OK : Boolean;
   begin
      if not R.Valid_Wire (Child) then return 2; end if;
      if G.Retirement_Confirmed (R.Decode (Child)) then return 0; end if;
      G.Revoke (R.Decode (Child), OK);
      if not OK then return 2; end if;
      return (if G.Retirement_Confirmed (R.Decode (Child)) then 0 else 1);
   end Retire;
end Native_GPU_Presentation;
