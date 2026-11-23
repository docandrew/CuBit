pragma Ada_2022;
package body HID_Reset_Policy with SPARK_Mode is
   procedure Unsolicited_Reset
     (Consecutive : in out Spurious_Reset_Count; Total : in out Word;
      Tolerated : out Boolean) is
   begin
      Tolerated := Consecutive < Maximum_Spurious_Resets;
      if Total < Word'Last then Total := Total + 1; end if;
      if Tolerated then Consecutive := Consecutive + 1; end if;
   end Unsolicited_Reset;
   procedure Ack_Deadline (Now, Overall : Word; Until_Time : out Word; Accepted : out Boolean) is
   begin
      Until_Time:=0;Accepted:=False;
      if Now>=Overall or else Overall=Word'Last then return;end if;
      if Overall-Now<Ack_Window then Until_Time:=Overall;
      else Until_Time:=Now+Ack_Window;end if;
      Accepted:=True;
   end Ack_Deadline;
end HID_Reset_Policy;
