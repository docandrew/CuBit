with Interfaces;
package Native_Test_Control is
   Maximum_Test_Table_Bytes : constant := 8192;
   type Bytes is array (Positive range <>) of Interfaces.Unsigned_8;
   Data : Bytes (1 .. Maximum_Test_Table_Bytes) := [others => 0];
   Length : Natural := 0;
   Infos, Descriptions, Copies, Clock_Reads : Natural := 0;
   Complete_Seen : Boolean := False;
   function Mode return String;
end Native_Test_Control;
