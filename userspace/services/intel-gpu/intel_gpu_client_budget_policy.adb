package body Intel_GPU_Client_Budget_Policy with SPARK_Mode is
   procedure Reserve
     (Charged : in out Unsigned_64; Limit, Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Bytes = 0 or else Bytes mod 4096 /= 0 or else
        Charged > Limit or else Bytes > Limit - Charged
      then
         return;
      end if;
      Charged := Charged + Bytes;
      Accepted := True;
   end Reserve;

   procedure Release
     (Charged : in out Unsigned_64; Bytes : Unsigned_64;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Bytes = 0 or else Bytes mod 4096 /= 0 or else Bytes > Charged then
         return;
      end if;
      Charged := Charged - Bytes;
      Accepted := True;
   end Release;
end Intel_GPU_Client_Budget_Policy;
