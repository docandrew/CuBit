package body Compositor_Surface_State with SPARK_Mode is
   procedure Configure (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := not S.Closing and then S.Requested < Maximum_Generation;
      if Accepted then
         S.Requested := S.Requested + 1;
         for I in Slot loop
            if S.Buffers (I).Status = Candidate then
               S.Buffers (I).Status := Retiring;
            end if;
         end loop;
      end if;
   end Configure;
   procedure Stage (S : in out State; I : Slot; Epoch : Generation; Accepted : out Boolean) is
   begin
      Accepted := not S.Closing and then S.Issued < Natural'Last and then Epoch > 0 and then Epoch = S.Requested and then S.Buffers (I).Status = Empty
        and then (for all J in Slot => S.Buffers (J).Status /= Candidate);
      if Accepted then
         S.Issued := S.Issued + 1;
         S.Buffers (I) := (Candidate, Epoch, S.Issued);
      end if;
   end Stage;
   procedure Present (S : in out State; I : Slot; Epoch : Generation; Ticket : Natural; Accepted : out Boolean) is
      Other : constant Slot := (if I = 1 then 2 else 1);
   begin
      Accepted := not S.Closing and then Epoch > 0 and then Epoch = S.Requested and then
        S.Buffers (I).Status = Candidate and then S.Buffers (I).Epoch = Epoch and then
        S.Buffers (I).Ticket = Ticket;
      if Accepted then
         if S.Buffers (Other).Status = Visible then
            S.Buffers (Other).Status := Retiring;
         end if;
         S.Buffers (I).Status := Visible;
      end if;
   end Present;
   procedure Discard (S : in out State; I : Slot; Ticket : Natural) is
   begin
      if S.Buffers (I).Status = Candidate and then S.Buffers (I).Ticket = Ticket then S.Buffers (I).Status := Retiring; end if;
   end Discard;
   procedure Close (S : in out State) is
   begin
      S.Closing := True;
      if S.Buffers (1).Status /= Empty then
         S.Buffers (1).Status := Retiring;
      end if;
      if S.Buffers (2).Status /= Empty then
         S.Buffers (2).Status := Retiring;
      end if;
   end Close;
   procedure Retire (S : in out State; I : Slot; Ticket : Natural; Readers_Retired : Boolean) is
   begin
      if Readers_Retired and then S.Buffers (I).Status = Retiring and then S.Buffers (I).Ticket = Ticket then
         S.Buffers (I) := (Empty, 0, 0);
      end if;
   end Retire;
end Compositor_Surface_State;
