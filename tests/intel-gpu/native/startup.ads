with CuBit.Handles;

--  Stands in for intel-gpu's generated startup package. These fixtures run
--  in devmgr's place (boot roots, no startup table), so the device-manager
--  endpoint the driver code submits to is this process itself, badged as
--  devmgr badges the GPU driver's endpoint.
package Startup is
   Driver_Badge : constant := 16#4947#;
   --  A badged copy of the boot roots' self endpoint, derived on first use;
   --  the null handle if the kernel refuses.
   function Device_Manager return CuBit.Handles.Endpoint;
end Startup;
