package body Process_Memory_Accounts with SPARK_Mode is
   procedure Open
     (Object : in out Account; New_Identity : Unsigned_64; OK : out Boolean)
   is
   begin
      OK := Reusable (Object) and then New_Identity > Object.Token;
      if OK then
         Object := (Token => New_Identity, Live => True, Budget => <>);
      end if;
   end Open;

   procedure Close
     (Object : in out Account; Expected : Unsigned_64; OK : out Boolean)
   is
   begin
      OK := Object.Live and then Expected = Object.Token;
      if OK then
         Object.Live := False;
      end if;
   end Close;

   procedure Adopt
     (Object : in out Account; Expected, Pages : Unsigned_64; OK : out Boolean)
   is
   begin
      OK := Object.Live and then Expected = Object.Token;
      if OK then
         Process_Memory_Budget.Adopt (Object.Budget, Pages, OK);
      end if;
   end Adopt;

   procedure Reserve
     (Object : in out Account; Expected : Unsigned_64;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
   is
   begin
      OK := Object.Live and then Expected = Object.Token;
      if OK then
         Process_Memory_Budget.Reserve (Object.Budget, Kind, Pages, OK);
      end if;
   end Reserve;

   procedure Refund
     (Object : in out Account; Expected : Unsigned_64;
      Kind : Process_Memory_Budget.Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean)
   is
   begin
      OK := Expected /= 0 and then Expected = Object.Token;
      if OK then
         Process_Memory_Budget.Release (Object.Budget, Kind, Pages, OK);
      end if;
   end Refund;
end Process_Memory_Accounts;
