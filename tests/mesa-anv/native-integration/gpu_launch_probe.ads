with CuBit.Messages;
package GPU_Launch_Probe is
   -- Private fixture only: server co-locates synthetic GPU and broker roles.
   -- The real devmgr/GPU remain distinct and use trusted bootstrap binding.
   procedure Client;
   procedure Server (Sender : CuBit.Messages.Process_ID;
                     Request : CuBit.Messages.Message);
end GPU_Launch_Probe;
