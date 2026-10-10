with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Filesystem_Queues;
with Files_Mock_Service;

--  Hosted: the queue's regions are memory this process shares with the mock
--  filesystem service's task (Files_Mock_Service).
package body Files_Link is
   package FQ renames CuBit.Filesystem_Queues;
   PAGE : constant := FQ.Page_Bytes;

   type Region is array (Storage_Offset range <>) of Storage_Element with Alignment => PAGE;
   type Region_Access is access Region;
   Client_Memory, Server_Memory, Arena_Memory : Region_Access;
   Opened : Boolean := False;

   procedure Open (Arena_Pages : Positive; Success : out Boolean) is
   begin
      if not Opened then
         Client_Memory := new Region'(1 .. FQ.Client_Pages * PAGE => 0);
         Server_Memory := new Region'(1 .. FQ.Server_Pages * PAGE => 0);
         Arena_Memory := new Region'(1 .. Storage_Offset (Arena_Pages) * PAGE => 0);
         Files_Mock_Service.Start (Client_Region, Server_Region, Arena, Arena_Bytes);
         Opened := True;
      end if;
      Success := True;
   end Open;

   function Client_Region return System.Address is (Client_Memory.all'Address);
   function Server_Region return System.Address is (Server_Memory.all'Address);
   function Arena return System.Address is (Arena_Memory.all'Address);
   function Arena_Bytes return Unsigned_64 is (Unsigned_64 (Arena_Memory'Length));

   procedure Kick is
   begin
      Files_Mock_Service.Kick;
   end Kick;

   procedure Arm_Wake is
   begin
      Files_Mock_Service.Arm_Wake;
   end Arm_Wake;

   procedure Close is
   begin
      if Opened then
         Files_Mock_Service.Stop;
         Opened := False;
      end if;
   end Close;
end Files_Link;
