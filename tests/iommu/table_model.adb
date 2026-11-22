pragma Ada_2022;
package body Table_Model with SPARK_Mode is

   function Number_Of (Table : Page_Address) return Table_Count is
     (if Table mod Page_Bytes = 0 and then Table / Page_Bytes in 1 .. Unsigned_64 (Used)
      then Table_Count (Table / Page_Bytes) else 0);

   function Read (Table : Page_Address; Index : Table_Index) return Unsigned_64 is
      N : constant Table_Count := Number_Of (Table);
   begin
      return (if N = 0 then 0 else Store (N) (Index));
   end Read;

   procedure Write (Table : Page_Address; Index : Table_Index; Value : Unsigned_64) is
      N : constant Table_Count := Number_Of (Table);
   begin
      if N /= 0 then
         Store (N) (Index) := Value;
         if Writes < Natural'Last then
            Writes := Writes + 1;
         end if;
      end if;
   end Write;

   procedure New_Table (Table : out Page_Address; Success : out Boolean) is
   begin
      if Used = Capacity then
         Table := 0;
         Success := False;
         return;
      end if;
      Used := Used + 1;
      Store (Used) := [others => 0];
      Table := Address_Of (Used);
      Success := True;
   end New_Table;

   procedure Reset is
   begin
      Store := [others => [others => 0]];
      Used := 0;
      Writes := 0;
   end Reset;
end Table_Model;
