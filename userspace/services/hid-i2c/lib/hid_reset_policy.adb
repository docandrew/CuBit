pragma Ada_2022;
package body HID_Reset_Policy with SPARK_Mode is
   procedure Ack_Deadline (Now, Overall : Word; Until_Time : out Word; Accepted : out Boolean) is
   begin
      Until_Time:=0;Accepted:=False;
      if Now>=Overall or else Overall=Word'Last then return;end if;
      if Overall-Now<Ack_Window then Until_Time:=Overall;
      else Until_Time:=Now+Ack_Window;end if;
      Accepted:=True;
   end Ack_Deadline;
end HID_Reset_Policy;
