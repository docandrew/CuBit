with Interfaces; use Interfaces;
package Intel_GPU_ADS_Header with SPARK_Mode is
   type Header_Bytes is array (Natural range 0 .. 4571) of Unsigned_8;
   type Register_Descriptors is array (Natural range 0 .. 4095) of Unsigned_8;
   type Capture_Pointers is array (Natural range 0 .. 263) of Unsigned_8;
   type Class_Values is array (Natural range 0 .. 15) of Unsigned_32;
   function Encode
     (Registers : Register_Descriptors;
      Policies, System_Info, Private_Data : Unsigned_32;
      Golden_Addresses, State_Sizes : Class_Values;
      Capture : Capture_Pointers) return Header_Bytes;
   -- ADL-N GuC70.49.4 early header: control_data and workaround KLV remain
   -- zero. Pure byte composition, NOT pointer validation or publication.
   -- Caller must derive all addresses from one owned/mapped ADS layout and
   -- keep recovery disabled until golden context contents are initialized.
end Intel_GPU_ADS_Header;
