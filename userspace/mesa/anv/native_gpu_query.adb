with CuBit.Messages; use CuBit.Messages;
package body Native_GPU_Query is
   function Budget (Slot : Unsigned_64; Output : access Reply_Words)
      return Unsigned_32 is
      Expected : constant MessageTag := (16#0A2E#, 4, 0, 0);
      Request : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Output = null then return 1; end if;
      Output.all := [others => 0];
      if Slot > Unsigned_64 (CapabilitySlot'Last) then return 1; end if;
      Request.tag := Expected;
      Request.words := [2, 0, 0, 0];
      Returned := capCall (CapabilitySlot (Slot), Request);
      if Returned /= Expected or Request.tag /= Expected then return 1; end if;
      Output.all := [Request.words (0), Request.words (1),
                     Request.words (2), Request.words (3)];
      return 0;
   end Budget;
   function Execute
     (Slot, Selector : Unsigned_64; Output : access Reply_Words)
      return Unsigned_32
   is
      Expected : constant MessageTag := (16#0A20#, 4, 0, 0);
      Request : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Output = null then return 1; end if;
      Output.all := [others => 0];
      if Slot > Unsigned_64 (CapabilitySlot'Last) or Selector > 4 then
         return 1;
      end if;
      Request.tag := Expected;
      Request.words := [1, Selector, 0, 0];
      -- Authority comes from the kernel-resolved endpoint capability, never
      -- from a caller-provided tag or device identifier.
      Returned := capCall (CapabilitySlot (Slot), Request);
      if Returned /= Expected or Request.tag /= Expected then return 1; end if;
      Output.all := [Request.words (0), Request.words (1),
                     Request.words (2), Request.words (3)];
      return 0;
   end Execute;
end Native_GPU_Query;
