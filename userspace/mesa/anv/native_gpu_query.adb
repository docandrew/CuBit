with CuBit.Messages; use CuBit.Messages;
with Native_GPU_Calls;
package body Native_GPU_Query is
   function Budget (Endpoint : Native_GPU_Calls.C_Endpoint; Output : access Reply_Words)
      return Unsigned_32 is
      Expected : constant MessageTag := (label => 16#0A2E#, length => 4,
                                         protocol => 0, ipc => <>, reserved => 0);
      Request : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Output = null then return 1; end if;
      Output.all := [others => 0];
      if not Native_GPU_Calls.Present (Endpoint) then return 1; end if;
      Request.tag := Expected;
      Request.words := [2, 0, 0, 0];
      Returned := Native_GPU_Calls.Call (Native_GPU_Calls.Target_Of (Endpoint), Request);
      if Returned /= Expected or Request.tag /= Expected then return 1; end if;
      Output.all := [Request.words (0), Request.words (1),
                     Request.words (2), Request.words (3)];
      return 0;
   end Budget;
   function Execute
     (Endpoint : Native_GPU_Calls.C_Endpoint; Selector : Unsigned_64; Output : access Reply_Words)
      return Unsigned_32
   is
      Expected : constant MessageTag := (label => 16#0A20#, length => 4,
                                         protocol => 0, ipc => <>, reserved => 0);
      Request : Message := NULL_MESSAGE;
      Returned : MessageTag;
   begin
      if Output = null then return 1; end if;
      Output.all := [others => 0];
      if not Native_GPU_Calls.Present (Endpoint) or Selector > 4 then
         return 1;
      end if;
      Request.tag := Expected;
      Request.words := [1, Selector, 0, 0];
      -- Authority comes from the kernel-resolved endpoint capability, never
      -- from a caller-provided tag or device identifier.
      Returned := Native_GPU_Calls.Call (Native_GPU_Calls.Target_Of (Endpoint), Request);
      if Returned /= Expected or Request.tag /= Expected then return 1; end if;
      Output.all := [Request.words (0), Request.words (1),
                     Request.words (2), Request.words (3)];
      return 0;
   end Execute;
end Native_GPU_Query;
