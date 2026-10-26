package body Intel_GPU_Supervisor is
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean is
      pragma Unreferenced (Token);
   begin
      Submissions := Submissions + 1;
      Last_Label := Msg.tag.label;
      return Accept_Submissions;
   end Submit_Accepting;
end Intel_GPU_Supervisor;
