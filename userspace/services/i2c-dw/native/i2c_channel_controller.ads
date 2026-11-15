pragma Ada_2022;
with DW_I2C_Transfer;
with DW_I2C_Owner;
with I2C_Channel;
with I2C_Channel_Wire;
with I2C_Channel_Badges;
with DW_I2C_Registers;
with DW_I2C_Configuration;
with Intel_LPSS_I2C_Reset;
generic
   with procedure Clock
     (Now : out DW_I2C_Transfer.Tick; Available : out Boolean);
   with procedure Reset_Read
     (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : out Intel_LPSS_I2C_Reset.Word;
      Status : out Intel_LPSS_I2C_Reset.Access_Status);
   with procedure Reset_Write
     (ID : Intel_LPSS_I2C_Reset.Register_ID; Value : Intel_LPSS_I2C_Reset.Word;
      Status : out Intel_LPSS_I2C_Reset.Access_Status);
   with procedure Config_Read
     (ID : DW_I2C_Configuration.Register_ID; Value : out DW_I2C_Configuration.Word;
      Status : out DW_I2C_Configuration.Access_Status);
   with procedure Config_Write
     (ID : DW_I2C_Configuration.Register_ID; Value : DW_I2C_Configuration.Word;
      Status : out DW_I2C_Configuration.Access_Status);
   with procedure Data_Read
     (ID : DW_I2C_Registers.Read_Register; Value : out DW_I2C_Registers.Word;
      Status : out DW_I2C_Registers.Access_Status);
   with procedure Data_Write
     (ID : DW_I2C_Registers.Write_Register; Value : DW_I2C_Registers.Word;
      Status : out DW_I2C_Registers.Access_Status);
package I2C_Channel_Controller is
   package T renames DW_I2C_Transfer;
   package RC renames I2C_Channel.RC;
   -- Internal byte-provider bridges for the owner instance below. No remote
   -- interface exposes these procedures; they share the singleton's payload.
   procedure Read_Payload (Index : T.Count; Value : out T.Byte; Accepted : out Boolean);
   procedure Write_Payload (Index : T.Count; Value : T.Byte; Accepted : out Boolean);
   package Controller is new DW_I2C_Owner
     (T.Limit'Last, Clock, Reset_Read, Reset_Write, Config_Read, Config_Write,
      Data_Read, Data_Write, True, Read_Payload, Write_Payload);
   -- One physical controller and one active borrowed channel at a time.
   procedure Start (Parameters : Controller.Startup; Result : out Controller.Admission);
   procedure Advance;
   procedure Prepare_Interrupt_Mode (Deadline : T.Tick; Result : out Controller.Interrupt_Result);
   procedure Reject_Interrupt_Wait;
   procedure Arm_Interrupt_Wait (Result : out Controller.Interrupt_Result);
   procedure Disarm_Interrupt_Wait (Result : out Controller.Interrupt_Result);
   function Waiting_Deadline return T.Tick;
   -- Delegates to the same retained physical owner; no separate IRQ/MMIO
   -- owner or fabricated ready flag is accepted from the event-loop glue.
   function Observe return Controller.Report;
   function Has_Payload return Boolean;
   type Submission is (Accepted, Busy, Invalid_Metadata, Invalid_Channel,
      Unsupported_Channel, Invalid_Views, Owner_Refused, Retained);
   procedure Submit
     (Admitted_Badge : I2C_Channel_Badges.Word; Header : I2C_Channel_Wire.Raw_Request;
      Input, Output : RC.Payload_View; ID : out T.Ticket;
      Result : out Submission; Admission : out Controller.Admission);
   -- Admitted_Badge comes from the stored authenticated channel admission.
   -- Neither target, speed, work budget nor addresses come from Header.
   procedure Take_Completion (Item : out Controller.Completion; Result : out Controller.Collection);
   type Releasing is (Released, No_Payload, Busy, Retained);
   procedure Release (Result : out Releasing);
   -- Release gates the byte adapter using this actual owner's Payload_Status.
   -- Failure remains Retained even after collecting the completion metadata.
   -- Caller returns channel mappings only after Released/No_Payload and must
   -- serialize all methods on the controller's native event loop.
end I2C_Channel_Controller;
