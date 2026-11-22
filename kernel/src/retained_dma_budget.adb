package body Retained_DMA_Budget with SPARK_Mode is
   procedure Reserve
     (Limit : Unsigned_64; Charged : in out Unsigned_64;
      Pages : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := Can_Reserve (Limit, Charged, Pages);
      if Accepted then Charged := Charged + Pages; end if;
   end Reserve;
   procedure Cancel_Unpublished
     (Charged : in out Unsigned_64; Pages : Unsigned_64) is
   begin
      Charged := Charged - Pages;
   end Cancel_Unpublished;
end Retained_DMA_Budget;
