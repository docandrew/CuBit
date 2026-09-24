package body Boot_Output with SPARK_Mode => Off is
   Text_Handler : Text_Sink := null;
   Panic_Handler : Panic_Sink := null;
   Retire_Handler : Retire_Sink := null;
   Installed : Boolean := False with Atomic;
   Closed : Boolean := False with Atomic;
   procedure Install (Text : Text_Sink; Panic : Panic_Sink; Retire : Retire_Sink) is
   begin
      if Installed or Closed then return; end if;
      Text_Handler := Text;
      Panic_Handler := Panic;
      Retire_Handler := Retire;
      Installed := True;
   end;
   function Is_Retired return Boolean is (Closed);
   procedure Append (C : Character) is
   begin
      if not Closed and then Installed and then Text_Handler /= null then
         Text_Handler (C);
      end if;
   end;
   procedure Panic (Message : System.Address) is
   begin
      if not Closed and then Installed and then Panic_Handler /= null then
         Panic_Handler (Message);
      end if;
   end;
   procedure Retire is
   begin
      Closed := True;
      -- Always drain even on a second call: another CPU may still be inside
      -- the first retirement. The renderer serializes both at its paint lock.
      if Installed and then Retire_Handler /= null then Retire_Handler.all; end if;
   end;
end Boot_Output;
