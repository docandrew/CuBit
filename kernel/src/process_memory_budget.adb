package body Process_Memory_Budget with SPARK_Mode is
   function Used (Object : Ledger) return Unsigned_64 is (Object.Total);
   function Charged (Object : Ledger; Kind : Charge_Kind) return Unsigned_64 is
     (Object.Counts (Kind));
   function Limit (Object : Ledger) return Unsigned_64 is (Object.Ceiling);
   procedure Adopt (Object : in out Ledger; Pages : Unsigned_64; OK : out Boolean) is
   begin
      OK := Pages = 0 or else Object.Total <= Pages;
      if OK then Object.Ceiling := Pages; end if;
   end Adopt;
   procedure Reserve
     (Object : in out Ledger; Kind : Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean) is
   begin
      OK := Pages /= 0 and then Pages <= Unsigned_64'Last - Object.Total and then
        Pages <= Unsigned_64'Last - Object.Counts (Kind) and then
        (Object.Ceiling = 0 or else
          (Object.Total <= Object.Ceiling and then Pages <= Object.Ceiling - Object.Total));
      if OK then
         Object.Total := Object.Total + Pages;
         Object.Counts (Kind) := Object.Counts (Kind) + Pages;
      end if;
   end Reserve;
   procedure Release
     (Object : in out Ledger; Kind : Charge_Kind; Pages : Unsigned_64;
      OK : out Boolean) is
   begin
      OK := Pages /= 0 and then Pages <= Object.Counts (Kind) and then Pages <= Object.Total;
      if OK then
         Object.Total := Object.Total - Pages;
         Object.Counts (Kind) := Object.Counts (Kind) - Pages;
      end if;
   end Release;
end Process_Memory_Budget;
