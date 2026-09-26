package body CuBit.Messages is
   function waitCompletion (entries : System.Address; max, min : Unsigned_64) return Unsigned_64 is
      Output : CompletionEntry with Import, Address => entries;
   begin
      pragma Assert (max = 1 and min = 1);
      Waits := Waits + 1;
      Output := (1, Last_Token, Wait_Reply, 42, COMPLETION_OK, True);
      return 1;
   end waitCompletion;
   function capSubmit (slot : CapabilitySlot; msg : Message; token : Unsigned_64) return Boolean is
   begin
      Last_Request := msg;
      Last_Token := token;
      Last_Endpoint := slot;
      Submissions := Submissions + 1;
      return Accept_Submission;
   end capSubmit;
end CuBit.Messages;
