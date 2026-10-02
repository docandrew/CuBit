package body Intel_GPU_Budget_Query with SPARK_Mode is
   function Pending (Object : Query) return Boolean is (Object.Active);
   function Result (Object : Query) return B.Budget_Snapshot is (Object.Value);
   procedure Cancel (Object : in out Query) is
   begin
      Object.Active := False;
      Object.Value := (others => <>);
   end Cancel;
   procedure Start (Object : in out Query; Now : Unsigned_64;
                    Owner : Boolean; Token : out Unsigned_64) is
   begin
      Token := 0;
      if Object.Active then return; end if;
      Object.Value := (others => <>);
      if not Owner or else Now > Unsigned_64'Last - Timeout or else
        Object.Serial = Unsigned_32'Last then return; end if;
      Object.Serial := Object.Serial + 1;
      Object.Started := Now; Object.Previous := Now;
      Object.Active := True;
      Token := Token_Base + Unsigned_64 (Object.Serial);
   end Start;
   procedure Tick (Object : in out Query; Now : Unsigned_64; Owner : Boolean) is
   begin
      if not Owner then Cancel (Object); return; end if;
      if not Object.Active then return; end if;
      if Now = Unsigned_64'Last or else Now < Object.Previous or else
        Now - Object.Started >= Timeout then
         Cancel (Object);
      else
         Object.Previous := Now;
      end if;
   end Tick;
   procedure Complete
     (Object : in out Query; Token, Now : Unsigned_64; Owner, Transport_OK : Boolean;
      Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : B.Budget_Words; Consumed : out Boolean) is
   begin
      Consumed := Object.Active and then
        Token = Token_Base + Unsigned_64 (Object.Serial);
      Tick (Object, Now, Owner);
      if not Consumed or else not Object.Active then return; end if;
      Object.Active := False;
      Object.Value := (others => <>);
      if Transport_OK then
         Object.Value := B.Decode_Budget (Label, Length, Flags, Reserved, Data);
      end if;
   end Complete;
end Intel_GPU_Budget_Query;
