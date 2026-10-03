with System.Storage_Elements;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Grant_References;
with CuBit.Process_Observer;

--  proc.list on CuBit: procmgr's process-observer role. The console lends
--  procmgr one page; procmgr fills it with the kernel's process table joined
--  with each process's manifest identity, launcher and start time
--  (CuBit.Process_Observer). Without the role nothing is listed: no
--  fallback to an ambient query.
package body CCL_Processes is
   use Interfaces;
   use type CuBit.Messages.MessageTag;
   package PO renames CuBit.Process_Observer;
   FRAME_BYTES : constant := 4_096;
   --  The kernel's INVALID state, never listed.
   STATE_OFFSET : constant := 1;
   Page : array (0 .. PO.Page_Bytes - 1) of Unsigned_8 := [others => 0]
     with Alignment => PO.Page_Bytes;

   function U16 (At_Byte : Natural) return Unsigned_16 is
     (Unsigned_16 (Page (At_Byte)) or Shift_Left (Unsigned_16 (Page (At_Byte + 1)), 8));
   function U32 (At_Byte : Natural) return Unsigned_32 is
     (Unsigned_32 (U16 (At_Byte)) or Shift_Left (Unsigned_32 (U16 (At_Byte + 2)), 16));
   function U64 (At_Byte : Natural) return Unsigned_64 is
     (Unsigned_64 (U32 (At_Byte)) or Shift_Left (Unsigned_64 (U32 (At_Byte + 4)), 32));

   procedure List
     (Entries : out Listing; Count : out Listed_Count; Total : out Natural; Result : out Result_Kind)
   is
      Loan : CuBit.Memory_Grants.Grant_Reference;
      Lent, Revoked : Boolean;
      Request : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Reply_Tag : CuBit.Messages.MessageTag;
      Now : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
   begin
      Entries := [others => (others => <>)];
      Count := 0;
      Total := 0;
      Result := Unavailable;
      CuBit.Memory_Grants.Create_Via_Capability
        (CuBit.Messages.CapabilitySlot (PO.Observer_Slot), Page'Address, 1, True, Loan, Lent);
      if not Lent then
         --  No endpoint in the observer slot: the role was not granted.
         Result := Not_Granted;
         return;
      end if;
      Request.tag := (label => PO.List_Label, length => 1, flags => 0, reserved => 0);
      Request.words (0) := CuBit.Grant_References.Encode (Loan);
      Reply_Tag := CuBit.Messages.capCall (CuBit.Messages.CapabilitySlot (PO.Observer_Slot), Request);
      CuBit.Memory_Grants.Revoke (Loan, Revoked);
      if Reply_Tag.label = Unsigned_32 (PO.Status'Enum_Rep (PO.Denied)) then
         Result := Not_Granted;
         return;
      elsif Reply_Tag.label /= Unsigned_32 (PO.Status'Enum_Rep (PO.OK)) then
         return;
      end if;
      Result := Listed_All;
      Total := Natural (Unsigned_64'Min (Request.words (2), Unsigned_64 (Natural'Last)));
      for Index in 0 .. Natural (Unsigned_64'Min (Request.words (1), PO.Page_Records)) - 1 loop
         exit when Count = Processes.MAXIMUM_LISTED;
         declare
            Base : constant Natural := Index * PO.Record_Bytes;
            State : constant Natural := Natural (Page (Base + PO.State_Offset));
            Name_Length : constant Natural :=
              Natural'Min (Natural (Page (Base + PO.Name_Length_Offset)), PO.Name_Bytes);
            Identity_Length : constant Natural :=
              Natural'Min (Natural (Page (Base + PO.Identity_Length_Offset)), MAXIMUM_IDENTITY);
            Started : constant Unsigned_64 := U64 (Base + PO.Started_Offset);
            Item : Listed;
         begin
            if State in STATE_OFFSET .. STATE_OFFSET + Processes.Run_State'Pos (Processes.Run_State'Last) then
               Item.Pid := Natural (U32 (Base + PO.Pid_Offset));
               Item.Name_Length := Name_Length;
               for I in 1 .. Name_Length loop
                  Item.Name (I) := Character'Val (Page (Base + PO.Name_Offset + I - 1));
               end loop;
               Item.Identity_Length := Identity_Length;
               for I in 1 .. Identity_Length loop
                  Item.Identity (I) := Character'Val (Page (Base + PO.Identity_Offset + I - 1));
               end loop;
               Item.State := Processes.Run_State'Val (State - STATE_OFFSET);
               Item.Memory := Unsigned_64 (U32 (Base + PO.Frames_Offset)) * FRAME_BYTES;
               Item.Launcher := Natural (U32 (Base + PO.Launcher_Offset));
               Item.Age_Ms := (if Started in 1 .. Now then Now - Started else 0);
               Count := Count + 1;
               Entries (Count) := Item;
            end if;
         end;
      end loop;
   end List;
end CCL_Processes;
