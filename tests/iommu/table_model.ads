--  A model store of VT-d second-level tables for the hosted walker test:
--  table N lives at "physical" address N * 4096. IO_Page_Walk is
--  instantiated over it (Walk), which is also how the walker is proved.
pragma Ada_2022;
with Interfaces; use Interfaces;
with IO_Page_Tables; use IO_Page_Tables;
with IO_Page_Walk;

package Table_Model with SPARK_Mode is
   Capacity : constant := 512;
   subtype Table_Count is Natural range 0 .. Capacity;
   subtype Table_Number is Positive range 1 .. Capacity;
   type Table is array (Table_Index) of Unsigned_64;
   type Table_Store is array (Table_Number) of Table;

   Store : Table_Store := [others => [others => 0]];
   Used  : Table_Count := 0;
   Writes : Natural := 0;

   function Address_Of (N : Table_Number) return Page_Address is
     (Unsigned_64 (N) * Page_Bytes);

   function Read (Table : Page_Address; Index : Table_Index) return Unsigned_64
   with Global => (Input => (Store, Used));
   procedure Write (Table : Page_Address; Index : Table_Index; Value : Unsigned_64)
   with Global => (In_Out => (Store, Writes), Input => Used);
   procedure New_Table (Table : out Page_Address; Success : out Boolean)
   with Global => (In_Out => (Store, Used));

   procedure Reset with Global => (Output => (Store, Used, Writes));

   package Walk is new IO_Page_Walk (Read, Write, New_Table);
end Table_Model;
