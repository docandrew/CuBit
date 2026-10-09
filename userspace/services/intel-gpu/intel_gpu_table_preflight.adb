package body Intel_GPU_Table_Preflight is
   function Status (State : Controller) return Phase is (State.Value);
   procedure Cancel (State : in out Controller) is
   begin State.Value := Failed; end Cancel;
   procedure Start (State : in out Controller; Count : Natural; Accepted : out Boolean) is
   begin
      Accepted := False;
      if State.Busy then State.Value := Failed; return; end if;
      if State.Value = Running then return; end if;
      State.Value := Failed;
      if Count = 0 or else not Current then return; end if;
      State.Count := Count; State.Cursor := 0;
      State.Value := Running; Accepted := True;
   end Start;
   procedure Step (State : in out Controller) is
      OK : Boolean;
   begin
      if State.Busy then State.Value := Failed; return; end if;
      if State.Value /= Running then return; end if;
      if not Current then State.Value := Failed; return; end if;
      for Work in 1 .. 32 loop
         State.Busy := True;
         OK := Valid_Table (State.Cursor + 1);
         State.Busy := False;
         if not OK or else not Current or else State.Value /= Running then
            State.Value := Failed; return;
         end if;
         State.Cursor := State.Cursor + 1;
         if State.Cursor = State.Count then State.Value := Complete; return; end if;
      end loop;
   end Step;
end Intel_GPU_Table_Preflight;
