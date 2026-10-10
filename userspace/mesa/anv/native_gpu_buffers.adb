with CuBit.Messages; use CuBit.Messages;
with Native_GPU_Calls;
with CuBit.Grant_References;
package body Native_GPU_Buffers is
   function Query_Accounting
     (Slot : Unsigned_64; Limit, Charged : access Unsigned_64)
      return Unsigned_32 is
      Expected : constant MessageTag := (16#0A30#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Limit /= null then Limit.all := 0; end if;
      if Charged /= null then Charged.all := 0; end if;
      if Limit = null or else Charged = null or else Limit = Charged or else
        Slot > Unsigned_64 (CapabilitySlot'Last)
      then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) > 3 or else Msg.words (1) /= 1
      then return 4; end if;
      if Msg.words (0) /= 0 then
         if Msg.words (2) /= 0 or else Msg.words (3) /= 0 then return 4; end if;
         return Unsigned_32 (Msg.words (0));
      end if;
      if Msg.words (2) = 0 or else Msg.words (2) mod 4096 /= 0 or else
        Msg.words (3) mod 4096 /= 0 or else Msg.words (3) > Msg.words (2)
      then return 4; end if;
      Limit.all := Msg.words (2);
      Charged.all := Msg.words (3);
      return 0;
   end Query_Accounting;
   function Memory_Contract (Slot : Unsigned_64) return Unsigned_32 is
      Expected : constant MessageTag := (16#0A20#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 0; end if;
      Msg.tag := Expected;
      Msg.words := [1, 3, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) /= 0 or else Msg.words (1) /= 1 or else
        Msg.words (2) not in 1 .. 2 or else Msg.words (3) /= 0
      then return 0; end if;
      return Unsigned_32 (Msg.words (2));
   end Memory_Contract;
   function Session_Status (Slot : Unsigned_64) return Unsigned_32 is
      Expected : constant MessageTag := (16#0A2F#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) > 3 or else Msg.words (1) /= 1 or else
        Msg.words (2) /= 0 or else Msg.words (3) /= 0
      then return 4; end if;
      return Unsigned_32 (Msg.words (0));
   end Session_Status;
   function Poll_Session_Retirement (Slot : Unsigned_64) return Unsigned_32 is
      Expected : constant MessageTag := (16#0A2D#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 5; end if;
      Msg.tag := Expected;
      Msg.words := [1, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) > 4 or else Msg.words (1) /= 1 or else
        Msg.words (2) /= 0 or else Msg.words (3) /= 0
      then return 5; end if;
      return Unsigned_32 (Msg.words (0));
   end Poll_Session_Retirement;
   function Close_Session (Slot : Unsigned_64; Retired_Tag : access Unsigned_64)
      return Unsigned_32 is
      Expected : constant MessageTag := (16#0A2C#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Retired_Tag /= null then Retired_Tag.all := 0; end if;
      if Retired_Tag = null or else Slot > Unsigned_64 (CapabilitySlot'Last)
      then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) > 3 or else Msg.words (1) /= 1 or else Msg.words (3) /= 0
      then return 4; end if;
      if Msg.words (0) /= 0 then
         if Msg.words (2) /= 0 then return 4; end if;
         return Unsigned_32 (Msg.words (0));
      end if;
      if Msg.words (2) = 0 then return 4; end if;
      Retired_Tag.all := Msg.words (2);
      return 0;
   end Close_Session;
   function Update_Binding
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64;
      Remove, Previous : Unsigned_32; Generation : access Unsigned_32)
      return Unsigned_32 is
      Expected : constant MessageTag := (16#0A28#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
      Limit : constant Unsigned_64 := 16 * 1024 * 1024;
   begin
      if Generation /= null then Generation.all := 0; end if;
      if Generation = null or else Slot > Unsigned_64 (CapabilitySlot'Last) or else
        Handle = 0 or else Remove > 1 or else Previous = Unsigned_32'Last or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 4096 /= 0 or else
        Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > Limit or else
        Offset mod 4096 /= 0 or else Offset > Limit - Bytes or else
        Bytes > 2 ** 48 - GPU then return 4; end if;
      Msg.tag := Expected;
      Msg.words :=
        [1 + Shift_Left (Unsigned_64 (Remove), 16) + Shift_Left (Unsigned_64 (Previous), 32),
         Unsigned_64 (Handle) + Shift_Left (Offset / 4096, 32), GPU, Bytes];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or else Msg.tag /= Expected or else
        Msg.words (0) > 3 or else Msg.words (1) /= 1 or else Msg.words (3) /= 0
      then return 4; end if;
      if Msg.words (0) /= 0 then
         if Msg.words (2) /= 0 then return 4; end if;
         return Unsigned_32 (Msg.words (0));
      end if;
      if Msg.words (2) /= Unsigned_64 (Previous) + 1 then return 4; end if;
      Generation.all := Unsigned_32 (Msg.words (2));
      return 0;
   end Update_Binding;
   function Context_Transition
     (Slot : Unsigned_64; Label : Unsigned_32) return Unsigned_32 is
      Expected : constant MessageTag := (Label, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or Msg.tag /= Expected or
        Msg.words (0) > 3 or Msg.words (1) /= 1 or
        Msg.words (2) /= 0 or Msg.words (3) /= 0
      then return 4; end if;
      return Unsigned_32 (Msg.words (0));
   end Context_Transition;
   function Prepare_Context (Slot : Unsigned_64) return Unsigned_32 is
     (Context_Transition (Slot, 16#0A25#));
   function Register_Context (Slot : Unsigned_64) return Unsigned_32 is
     (Context_Transition (Slot, 16#0A26#));
   function Change_Offline_GPU
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64;
      Remove : Boolean)
      return Unsigned_32 is
      Expected : constant MessageTag := (16#0A24#, 4, 0, 0);
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) or Handle = 0 or GPU = 0 or
        GPU >= 2 ** 48 or GPU mod 4096 /= 0 or Bytes = 0 or
        Bytes mod 4096 /= 0 or Bytes > 16 * 1024 * 1024 or
        Offset mod 4096 /= 0 or Offset > 16 * 1024 * 1024 - Bytes
      then return 4; end if;
      if Bytes > 2 ** 48 - GPU then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1 + (if Remove then 16#10000# else 0) +
        Shift_Left (Offset / 4096, 32), Unsigned_64 (Handle), GPU, Bytes];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or Msg.tag /= Expected or
        Msg.words (0) > 3 or Msg.words (1) /= 1 then return 4; end if;
      if Msg.words (0) = 0 then
         if Msg.words (2) /= GPU or Msg.words (3) /= Bytes then return 4; end if;
      elsif Msg.words (2) /= 0 or Msg.words (3) /= 0 then return 4;
      end if;
      return Unsigned_32 (Msg.words (0));
   end Change_Offline_GPU;
   function Bind_GPU
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64)
      return Unsigned_32 is
     (Change_Offline_GPU (Slot, Handle, GPU, Offset, Bytes, False));
   function Unbind_GPU
     (Slot : Unsigned_64; Handle : Unsigned_32; GPU, Offset, Bytes : Unsigned_64)
      return Unsigned_32 is
     (Change_Offline_GPU (Slot, Handle, GPU, Offset, Bytes, True));
   Map_Tag : constant MessageTag := (16#0A23#, 4, 0, 0);
   function Map_Operation
     (Slot : Unsigned_64; Handle : Unsigned_32; Offset, Bytes : Unsigned_64;
      Operation : Unsigned_32; Mapping : access Unsigned_32;
      Reference : access Unsigned_64) return Unsigned_32 is
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Mapping /= null then Mapping.all := 0; end if;
      if Reference /= null then Reference.all := 0; end if;
      if Mapping = null or Reference = null or Slot > Unsigned_64 (CapabilitySlot'Last)
        or Handle = 0 or (Operation /= 0 and Operation /= 1 and Operation /= 3)
        or Offset mod 4096 /= 0 or Bytes = 0
        or Bytes mod 4096 /= 0 or Bytes > 16 * 1024 * 1024
        or Offset > Unsigned_64'Last - Bytes then return 5; end if;
      Msg.tag := Map_Tag;
      Msg.words := [1 + Shift_Left (Unsigned_64 (Operation), 32),
                    Unsigned_64 (Handle), Offset, Bytes];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Map_Tag or Msg.tag /= Map_Tag or Msg.words (0) > 3 or
        Msg.words (1) /= 1 then return 5; end if;
      if Msg.words (0) /= 0 then
         if Msg.words (2) /= 0 or Msg.words (3) /= 0 then return 5; end if;
         return Unsigned_32 (Msg.words (0));
      end if;
      -- Use the shared codec, including its admitted namespace bound.
      -- Acquisition independently authenticates this reference in the kernel.
      if Msg.words (2) = 0 or Msg.words (2) > Unsigned_64 (Unsigned_32'Last) or
        not CuBit.Grant_References.Valid_Wire (Msg.words (3)) then return 5; end if;
      Mapping.all := Unsigned_32 (Msg.words (2));
      Reference.all := Msg.words (3);
      return 0;
   end Map_Operation;
   function Map
     (Slot : Unsigned_64; Handle : Unsigned_32; Offset, Bytes : Unsigned_64;
      Writable : Unsigned_32; Mapping : access Unsigned_32;
      Reference : access Unsigned_64) return Unsigned_32 is
   begin
      -- Do not let the ordinary writable flag accidentally opt into forwarding.
      if Writable > 1 then
         if Mapping /= null then Mapping.all := 0; end if;
         if Reference /= null then Reference.all := 0; end if;
         return 5;
      end if;
      return Map_Operation (Slot, Handle, Offset, Bytes, Writable, Mapping, Reference);
   end Map;
   function Map_Presentation
     (Slot : Unsigned_64; Handle : Unsigned_32; Offset, Bytes : Unsigned_64;
      Mapping : access Unsigned_32; Reference : access Unsigned_64)
      return Unsigned_32 is
     (Map_Operation (Slot, Handle, Offset, Bytes, 3, Mapping, Reference));
   function Retire_Map
     (Slot : Unsigned_64; Mapping : Unsigned_32) return Unsigned_32 is
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Slot > Unsigned_64 (CapabilitySlot'Last) or Mapping = 0 then return 5; end if;
      Msg.tag := Map_Tag;
      Msg.words := [1 + 2 * 2 ** 32, Unsigned_64 (Mapping), 0, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Map_Tag or Msg.tag /= Map_Tag or Msg.words (0) > 4 or
        Msg.words (1) /= 1 or Msg.words (2) /= 0 or Msg.words (3) /= 0
      then return 5; end if;
      return Unsigned_32 (Msg.words (0));
   end Retire_Map;
   Expected : constant MessageTag := (16#0A22#, 4, 0, 0);
   function Execute (Slot, Operation, Value : Unsigned_64;
                     Result : out Unsigned_32) return Unsigned_32 is
      Msg : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      Result := 0;
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 4; end if;
      Msg.tag := Expected;
      Msg.words := [1, Operation, Value, 0];
      Returned := Native_GPU_Calls.Call (CapabilitySlot (Slot), Msg);
      if Returned /= Expected or Msg.tag /= Expected or
        Msg.words (0) > 3 or Msg.words (1) /= 1 then return 4; end if;
      if Msg.words (0) /= 0 then
         if Msg.words (2) /= 0 or Msg.words (3) /= 0 then return 4; end if;
         return Unsigned_32 (Msg.words (0));
      end if;
      if Operation = 0 then
         if Msg.words (2) = 0 or Msg.words (2) > Unsigned_64 (Unsigned_32'Last) or
           Msg.words (3) /= Value then return 4; end if;
         Result := Unsigned_32 (Msg.words (2));
      elsif Msg.words (2) /= 0 or Msg.words (3) /= 0 then
         return 4;
      end if;
      return 0;
   end Execute;
   function Create
     (Slot, Bytes : Unsigned_64; Handle : access Unsigned_32) return Unsigned_32 is
      ID, Status : Unsigned_32;
   begin
      if Handle = null then return 4; end if;
      Handle.all := 0;
      if Bytes = 0 or Bytes mod 4096 /= 0 or Bytes > 16 * 1024 * 1024
      then return 4; end if;
      Status := Execute (Slot, 0, Bytes, ID);
      if Status = 0 then Handle.all := ID; end if;
      return Status;
   end Create;
   function Close (Slot : Unsigned_64; Handle : Unsigned_32) return Unsigned_32 is
      Unused : Unsigned_32;
   begin
      if Handle = 0 then return 4; end if;
      return Execute (Slot, 1, Unsigned_64 (Handle), Unused);
   end Close;
end Native_GPU_Buffers;
