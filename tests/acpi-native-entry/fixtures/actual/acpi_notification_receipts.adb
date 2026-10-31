pragma Ada_2022;
package body ACPI_Notification_Receipts with SPARK_Mode is
   -- Arithmetic uses the unconstrained wire base type. A zero-capacity
   -- instance must not generate a statically impossible Number increment.
   function Successor (Value : Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64
   is (Value + 1)
     with Pre => Value < Interfaces.Unsigned_64'Last;

   procedure Issue (S : in out State) is
   begin
      -- A zero-capacity instance cannot satisfy Can_Issue. Keep its generated
      -- checked body free of an unconditional out-of-range increment as well.
      if Final_Receipt > 0 then
         S.Issued := Number (Successor (Interfaces.Unsigned_64 (S.Issued)));
         S.Active := S.Issued;
      end if;
   end Issue;
   procedure Acknowledge (S : in out State; ID : Number; Result : out Ack_Result) is
   begin
      Result := Invalid;
      if ID = 0 then return; end if;
      if ID = S.Active then
         S.Active := 0;
         S.Acknowledged := ID;
         Result := Accepted;
      elsif ID = S.Acknowledged then
         Result := Repeated;
      end if;
   end Acknowledge;
end ACPI_Notification_Receipts;
