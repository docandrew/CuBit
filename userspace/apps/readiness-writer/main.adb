------------------------------------------------------------------------------
--  KERN-013 regression: the readiness page is read-only to its process.
--  Reads it (the version must be published), then stores to it. The store
--  must fault: the kernel ends the program, so "wrote" is never printed.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with System.Storage_Elements;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Process_Calls;
with Readiness_Publication;

procedure Main is
   package RP renames Readiness_Publication;
   Page : RP.Readiness_Page
     with Import, Volatile, Async_Writers,
          Address => System.Storage_Elements.To_Address (RP.Page_Address);
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   if Page.Version = RP.Layout_Version then
      debugPrint ("readiness-writer: page readable PASS" & ASCII.LF);
   else
      debugPrint ("readiness-writer: page readable FAIL" & ASCII.LF);
   end if;
   debugPrint ("readiness-writer: storing to the page" & ASCII.LF);
   Page.Wake_Generation := 0;
   debugPrint ("readiness-writer: wrote FAIL" & ASCII.LF);
   CuBit.Process_Calls.Exit_Process (CuBit.Process_Calls.Failure);
end Main;
