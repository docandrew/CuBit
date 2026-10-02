with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Request;
procedure GuC_Deregister_Request_Tests is
   use Intel_GPU_GuC_Context_Request;
begin
   for ID in Unsigned_32 range 0 .. 65534 loop
      pragma Assert (Deregister (ID) = [16#20004503#, ID]);
      pragma Assert (Deregister (ID) /= Schedule (ID));
   end loop;
   pragma Assert (Deregister (65535) = [0,0]);
   pragma Assert (Deregister (65536) = [0,0]);
   pragma Assert (Deregister (Unsigned_32'Last) = [0,0]);
   Ada.Text_IO.Put_Line ("GuC deregister encoding PASS: 65535 IDs; no send or reclaim");
end GuC_Deregister_Request_Tests;
