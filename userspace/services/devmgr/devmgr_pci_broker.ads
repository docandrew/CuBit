pragma Ada_2022;
with CuBit.Messages;
with Interfaces;
with CuBit.Handles;
with Devmgr_PCI_Claims;
package Devmgr_PCI_Broker is
   package Q renames Devmgr_PCI_Claims.Q;
   package C renames Devmgr_PCI_Claims;
   type Observation is record
      Reader : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Request : Q.Request;
      Status : Q.Disposition := Q.Malformed;
      Record_Number : Q.Record_ID := Q.No_Record;
      Delivery : C.Delivery := C.Refused;
   end record;
   -- Called only by devmgr's serialized receive loop. The complete MCFG is
   -- copied from the kernel's sealed snapshot. Sender/tag and fresh root inspection
   -- determine authority. A reply never carries Grant or Write rights.
   procedure Serve
     (From : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Snapshot : CuBit.Handles.Firmware_Snapshot; Result : out Observation);
   type Exclusion_Status is (Collected, No_MCFG, Source_Unavailable,
      Malformed_MCFG, Invalid_MCFG_Allocation, Store_Refused);
   function Name (Status : Exclusion_Status) return String;
   -- Internal firmware-data exclusion facts only. A partial callback prefix
   -- must never be used unless the final status is Collected or No_MCFG.
   -- Record_Exclusion is bounded and returns normally. A refusal stops further
   -- callbacks and returns Store_Refused; it cannot commit a partial inventory.
   generic
      with procedure Record_Exclusion
        (First, Bytes : Interfaces.Unsigned_64; Accepted : out Boolean);
   procedure Read_Exclusions
     (Snapshot : CuBit.Handles.Firmware_Snapshot; Status : out Exclusion_Status);
   function Reader_Claims (Key : Q.Bus_Key) return Natural;
   function Count return Natural;
   function Claim_At (Index : Positive) return C.Claim with Pre => Index <= Count;
end Devmgr_PCI_Broker;
