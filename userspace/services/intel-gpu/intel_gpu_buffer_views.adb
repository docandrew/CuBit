with System.Storage_Elements;
with CuBit.Memory_Grants;
with CuBit.Capability_Grants;
with Intel_GPU_Buffer_Reply;
package body Intel_GPU_Buffer_Views is
   package Grants renames CuBit.Memory_Grants;
   function State (Object : View) return View_State is (Object.Current);
   function Wire_Reference (Object : View) return Unsigned_64 is
     (if Object.Current = Shared then
         CuBit.Grant_References.Encode (Object.Reference) else 0);

   procedure Share_Backing
     (Object : in out View; Backing : Intel_GPU_Buffer_Reply.Backing;
      Recipient : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64;
      Offset, Bytes : Unsigned_64; Writable : Boolean;
      Attempted : out Boolean;
      Presentation : Boolean := False)
   is
      Success : Boolean;
   begin
      Attempted := False;
      if Object.Current /= Empty then return; end if;
      Object.Current := Failed;
      if (Presentation and Writable) or else not Intel_GPU_Buffer_Reply.Valid (Backing) or else Bytes = 0 or else
         (Offset mod 4096) /= 0 or else (Bytes mod 4096) /= 0 or else
         Offset > Backing.Bytes or else Bytes > Backing.Bytes - Offset
      then
         return;
      end if;
      if not CuBit.Capability_Grants.Endpoint_Matches (Recipient, Identity) then
         return;
      end if;
      Attempted := True;
      if Presentation then
         Grants.Create_Forwardable_Via_Capability
           (Recipient, System.Storage_Elements.To_Address
              (System.Storage_Elements.Integer_Address (Backing.CPU_Address + Offset)),
            Natural (Bytes / 4096), False, Object.Reference, Success);
      else
         Grants.Create_Via_Capability
        (Recipient, System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Backing.CPU_Address + Offset)),
         Natural (Bytes / 4096), Writable, Object.Reference, Success);
      end if;
      if Success then Object.Current := Shared; end if;
   end Share_Backing;

   procedure Share_Pinned
     (Object : in out View; Buffers : in out Intel_GPU_Buffer_Handles.Registry;
      Recipient : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64;
      Offset, Bytes : Unsigned_64; Writable, Presentation : Boolean) is
      Attempted, Returned : Boolean;
   begin
      Share_Backing (Object, Intel_GPU_Buffer_Handles.Referenced_Backing
        (Buffers, Object.Backing_Reference), Recipient, Identity, Offset, Bytes,
        Writable, Attempted, Presentation);
      if not Attempted then
         -- No kernel creation was attempted: this new pin has no readers.
         Intel_GPU_Buffer_Handles.Return_Reference
           (Buffers, Object.Backing_Reference, True, Returned);
         if Returned then
            Object.Pinned := False;
            Object.Current := Retired;
         end if;
      end if;
   end Share_Pinned;

   procedure Share
     (Object : in out View; Buffers : in out Intel_GPU_Buffer_Handles.Registry;
      Session : Intel_GPU_Buffer_Handles.Session_ID;
      ID : Intel_GPU_Buffer_Handles.Handle;
      Recipient : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64;
      Offset, Bytes : Unsigned_64; Writable : Boolean;
      Presentation : Boolean := False) is
   begin
      if Object.Current /= Empty then return; end if;
      -- Enforce the allocation hold here too: trusted callers may bypass the
      -- request-level map table. No pin or kernel grant exists on rejection.
      if Writable and then Intel_GPU_Buffer_Handles.Writes_Excluded
        (Buffers, Session, ID)
      then Object.Current := Retired; return; end if;
      Intel_GPU_Buffer_Handles.Retain_Backing
        (Buffers, Session, ID, Object.Backing_Reference, Object.Pinned);
      if not Object.Pinned then Object.Current := Failed; return; end if;
      -- Acquire the lifetime reference BEFORE exposing any grant. Creation
      -- failure may be ambiguous: retain rather than guess that no alias lives.
      Share_Pinned (Object, Buffers, Recipient, Identity, Offset, Bytes,
                    Writable, Presentation);
   end Share;

   procedure Share_Retained
     (Object : in out View; Buffers : in out Intel_GPU_Buffer_Handles.Registry;
      Source : Intel_GPU_Buffer_Handles.Retained_Reference;
      Recipient : CuBit.Messages.CapabilitySlot; Identity : Unsigned_64;
      Offset, Bytes : Unsigned_64) is
   begin
      if Object.Current /= Empty then return; end if;
      Intel_GPU_Buffer_Handles.Retain_Referenced_Backing
        (Buffers, Source, Object.Backing_Reference, Object.Pinned);
      if not Object.Pinned then Object.Current := Failed; return; end if;
      Share_Pinned (Object, Buffers, Recipient, Identity, Offset, Bytes,
                    Writable => False, Presentation => False);
   end Share_Retained;

   procedure Share_Completed
     (Object : in out View; Recipient : CuBit.Messages.CapabilitySlot;
      Identity : Unsigned_64) is
      Before, After : Intel_GPU_Buffer_Reply.Backing;
      Attempted : Boolean;
   begin
      if Object.Current /= Empty then return; end if;
      Before := Completed_Backing;
      if not Intel_GPU_Buffer_Reply.Valid (Before) then
         Object.Current := Failed; return;
      end if;
      Share_Backing (Object, Before, Recipient, Identity, 0, Before.Bytes,
        Writable => False, Attempted => Attempted, Presentation => True);
      if Object.Current /= Shared then return; end if;
      After := Completed_Backing;
      if not Intel_GPU_Buffer_Reply.Valid (After) or else
        not Intel_GPU_Buffer_Reply.Same_Arena (Before, After) or else
        Before.CPU_Address /= After.CPU_Address or else Before.Bytes /= After.Bytes
      then Retire (Object); end if;
   end Share_Completed;

   procedure Poll_Retirement (Object : in out View) is
   begin
      if Object.Current = Retiring and then not Object.Pinned and then
         Grants.Retirement_Confirmed (Object.Reference)
      then
         Object.Current := Retired;
      end if;
   end Poll_Retirement;

   procedure Poll_Retirement
     (Object : in out View; Buffers : in out Intel_GPU_Buffer_Handles.Registry)
   is
      Accepted : Boolean;
   begin
      if Object.Current = Retiring and then Object.Pinned and then
        Grants.Retirement_Confirmed (Object.Reference)
      then
         Intel_GPU_Buffer_Handles.Return_Reference
           (Buffers, Object.Backing_Reference, References_Retired => True,
            Accepted => Accepted);
         if not Accepted then
            Object.Current := Failed;
            return;
         end if;
         Object.Pinned := False;
         Object.Current := Retired;
      else
         Poll_Retirement (Object);
      end if;
   end Poll_Retirement;

   procedure Recycle (Object : in out View; Accepted : out Boolean) is
   begin
      Accepted := Object.Current = Retired and then not Object.Pinned;
      if Accepted then
         Object.Current := Empty;
         -- The previous reference is no longer published and Share replaces it.
      end if;
   end Recycle;

   procedure Retire (Object : in out View) is
      Accepted : Boolean;
   begin
      if Object.Current = Shared then
         -- Stop publishing the reference even if revocation is uncertain.
         Object.Current := Retiring;
         Grants.Revoke (Object.Reference, Accepted);
         if not Accepted then
            Object.Current := Failed;
            return;
         end if;
      end if;
      Poll_Retirement (Object);
   end Retire;

   procedure Retire
     (Object : in out View; Buffers : in out Intel_GPU_Buffer_Handles.Registry) is
   begin
      Retire (Object);
      Poll_Retirement (Object, Buffers);
   end Retire;
end Intel_GPU_Buffer_Views;
