package body Intel_GPU_Image_Consumers is
   use type Intel_GPU_Image_Lease.Identity, System.Address;
   procedure Open
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.State /= Fresh or else Key.Adapter = 0 or else Key.Session = 0 or else
        Key.Allocation = 0 or else Key.Output_Epoch = 0 or else Key.Serial = 0 or else
        Key.Display_Instance = 0 or else Key.Consumer_Instance = 0
      then return; end if;
      Object.Key := Key;
      Object.State := Admitting;
      Accepted := True;
   end Open;
   procedure Reserve
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Kind : Domain; Token : in out Obligation; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.State /= Admitting or else Object.Key /= Key or else Token.Active or else
        Object.Last_Issued = Unsigned_64'Last or else Object.Pending (Kind) = Unsigned_64'Last
      then return; end if;
      Object.Last_Issued := Object.Last_Issued + 1;
      Object.Pending (Kind) := Object.Pending (Kind) + 1;
      Token.Origin := Object'Address;
      Token.Serial := Object.Last_Issued;
      Token.Kind := Kind;
      Token.Active := True;
      Accepted := True;
   end Reserve;
   procedure Stop
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Accepted : out Boolean) is
   begin
      Accepted := Object.State in Admitting | Closing and then Object.Key = Key;
      if Accepted then Object.State := Closing; end if;
   end Stop;
   procedure Complete
     (Object : in out Ledger; Key : Intel_GPU_Image_Lease.Identity;
      Token : in out Obligation; Confirmed : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Confirmed or else Object.State not in Admitting | Closing or else
        Object.Key /= Key or else not Token.Active or else Token.Origin /= Object'Address or else
        Token.Serial = 0 or else Token.Serial > Object.Last_Issued
      then return; end if;
      if Object.Pending (Token.Kind) = 0 then Object.State := Failed; return; end if;
      Object.Pending (Token.Kind) := Object.Pending (Token.Kind) - 1;
      Token.Active := False;
      Token.Origin := System.Null_Address;
      Token.Serial := 0;
      Accepted := True;
   end Complete;
   function Drained
     (Object : Ledger; Key : Intel_GPU_Image_Lease.Identity) return Boolean is
     (Object.State = Closing and then Object.Key = Key and then
      (for all Count of Object.Pending => Count = 0));
   procedure Quarantine (Object : in out Ledger) is
   begin Object.State := Failed; end Quarantine;
end Intel_GPU_Image_Consumers;
