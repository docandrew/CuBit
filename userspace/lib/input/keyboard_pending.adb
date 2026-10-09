package body Keyboard_Pending with SPARK_Mode is
   procedure Append_Byte (S : in out State; Value : Byte;
                          Added : out Frame_Length; Lost : out Boolean) is
      N : Positive range 1 .. 6;
      Ignored : Boolean;
   begin
      Added := 0; Lost := False;
      if S.Used = 0 then
         S.Partial (1) := Value;
         if Value = 16#E0# or else Value = 16#E1# then
            S.Expected := (if Value = 16#E0# then 2 else 6);
            S.Used := 1;
            return;
         end if;
         N := 1;
      else
         -- Repeated E0 must not commit a group ending in a prefix: otherwise
         -- local overflow could later retain its suffix as an ordinary key.
         if S.Expected = 2 and then Value = 16#E0# then return; end if;
         S.Partial (S.Used + 1) := Value;
         if S.Used + 1 < S.Expected then
            S.Used := S.Used + 1;
            return;
         end if;
         N := S.Expected;
      end if;
      S.Used := 0;
      if S.Discard_Partial then
         S.Discard_Partial := False;
         return;
      end if;
      Lost := Count (S) > Input_Pending.Capacity - N;
      if Lost then Input_Pending.Reset (S.Pending); end if;
      declare
         Before : constant Input_Pending.Queue := S.Pending with Ghost;
      begin
         for I in 1 .. N loop
            Input_Pending.Append (S.Pending, Word (S.Partial (I)), Ignored);
            pragma Assert (not Ignored);
            pragma Loop_Invariant (Count (S) = Input_Pending.Count (Before) + I);
            pragma Loop_Invariant
              (for all J in Input_Pending.Offset_Type =>
                 (if J < Input_Pending.Count (Before) then
                    Element (S, J) = Input_Pending.Element (Before, J)));
            pragma Loop_Invariant (if Lost then Element (S, 0).Recover);
         end loop;
      end;
      Added := N;
   end Append_Byte;
   procedure Acknowledge (S : in out State) is
   begin
      Input_Pending.Acknowledge (S.Pending);
   end Acknowledge;
   procedure Reset_Consumer (S : in out State) is
   begin
      Input_Pending.Reset (S.Pending);
      S.Discard_Partial := S.Used > 0;
   end Reset_Consumer;
end Keyboard_Pending;
