package body Desktop_Input_Diagnostic with SPARK_Mode is
   use Interfaces;
   procedure Increment (N : in out Word) is
   begin
      if N < Word'Last then N := N + 1; end if;
   end Increment;
   procedure Observe_Raw (S : in out State; Is_Source : Boolean;
      Label : Unsigned_32; Header, Payload, Snapshot, Now_Us : Word) is
      Stamp : constant Word := Shift_Right (Snapshot, 8);
   begin
      if Is_Source then
         case Header and 255 is
            when 1 => Increment (S.Raw_Key);
            when 2 =>
               Increment (S.Raw_Pointer);
               S.Payload_Buttons := Payload and 255;
               S.Snapshot_Buttons := Snapshot and 255;
               S.Last_Age := 0;
               if Stamp = 0 or else Now_Us = Word'Last then
                  S.Age_Status := No_Time;
               elsif Now_Us / 1_000 < Stamp - 1 then
                  S.Age_Status := Backward;
               else
                  S.Age_Status := OK;
                  S.Last_Age := Now_Us / 1_000 - (Stamp - 1);
                  S.Max_Age := Word'Max (S.Max_Age, S.Last_Age);
               end if;
            when others => null;
         end case;
      elsif Label = 1 then
         Increment (S.Legacy_Key);
      elsif Label = 2 then
         Increment (S.Legacy_Pointer);
      end if;
   end Observe_Raw;
   procedure Reject (S : in out State; Why : Rejection) is
   begin
      case Why is
         when Malformed => Increment (S.Invalid_Wire);
         when Delivery => Increment (S.Bad_Delivery);
         when Replayed => Increment (S.Duplicate);
         when Table_Full => Increment (S.Full);
      end case;
   end Reject;
   procedure New_Source (S : in out State) is
   begin Increment (S.Sources); end New_Source;
   procedure Accepted (S : in out State; Seat : Word; Gap : Boolean) is
   begin
      S.Seat_Buttons := Seat;
      if Gap then Increment (S.Gaps); end if;
   end Accepted;
   procedure Observe_Drops (S : in out State; Value : Word) is
   begin
      S.Drops_Valid := Value /= Word'Last;
      if S.Drops_Valid then S.Drops := Value; end if;
   end Observe_Drops;
end Desktop_Input_Diagnostic;
