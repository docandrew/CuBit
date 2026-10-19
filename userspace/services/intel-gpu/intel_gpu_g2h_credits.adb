package body Intel_GPU_G2H_Credits with SPARK_Mode is
   procedure Configure (Object : in out Pool; Words : Word_Count) is
   begin
      Object := (Limit => Words, Used => 0);
   end Configure;
   procedure Exchange
     (Object : in out Pool; Before, After : Word_Count; Accepted : out Boolean) is
   begin
      Accepted := Before <= Object.Used and then
        After <= Object.Limit - (Object.Used - Before);
      if Accepted then
         Object.Used := Object.Used - Before + After;
      end if;
   end Exchange;
end Intel_GPU_G2H_Credits;
