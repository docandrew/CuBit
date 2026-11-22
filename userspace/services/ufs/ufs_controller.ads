------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The UFS host controller's registers and DMA region: controller enable,
--  UIC commands (link startup, attribute get/set), request list setup, and
--  posting requests to transfer slots and task slots. Nothing here waits
--  for a transfer: callers poll Completed (async), only the bring-up steps
--  (enable, UIC) spin with explicit deadlines.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with SCSI;
with UFSHCI; use UFSHCI;
with UFS_UPIU; use UFS_UPIU;
with UFS_Layout; use UFS_Layout;

package UFS_Controller is
   --  Registers mapped at Registers; the DMA region mapped at DMA_Virtual,
   --  physically at DMA_Physical.
   procedure Attach
     (Registers : System.Address; DMA_Virtual : System.Address;
      DMA_Physical : Unsigned_64);

   function Read (Item : Register) return Register_Value;
   procedure Write (Item : Register; Value : Register_Value);

   type Step_Result is (Step_Done, Step_Timed_Out, Step_Failed);

   --  Resets and enables the controller (HCE), then waits for it to accept
   --  UIC commands.
   procedure Enable (Result : out Step_Result);

   --  One UIC command: Argument_1 (attribute and selector), Argument_3
   --  (value to set). Value: UICCMDARG3 after completion (a DME_GET's
   --  attribute value). Fails unless the configuration result is success.
   procedure UIC
     (Operation  : UIC_Operation;
      Argument_1 : Register_Value;
      Argument_3 : Register_Value;
      Value      : out Register_Value;
      Result     : out Step_Result);

   --  The configuration result code of the last UIC command that completed.
   function Last_UIC_Result return Register_Value;

   --  Programs both request lists' base addresses and starts them.
   procedure Start_Lists (Result : out Step_Result);

   --  Transfer slots. Post writes the request UPIU, its transfer descriptor
   --  and (with data) one region descriptor over the slot's buffer, then
   --  rings the slot's doorbell.
   procedure Post
     (Item      : Slot;
      Request   : Request_Frame;
      Direction : Transfer_Direction;
      Bytes     : Natural)
     with Pre => Bytes <= Buffer_Bytes
                 and then (Direction = No_Transfer) = (Bytes = 0)
                 and then Bytes mod Word_Bytes = 0;

   --  The controller has finished the slot's request (its doorbell bit is
   --  clear).
   function Completed (Item : Slot) return Boolean;
   function Status (Item : Slot) return Command_Status;

   --  The response UPIU and data segment the device wrote.
   subtype Response_Area is SCSI.Bytes (1 .. Response_Bytes);
   function Response (Item : Slot) return Response_Area;

   --  The slot's bounce buffer in the driver's address space.
   function Buffer (Item : Slot) return System.Address;

   --  Asks the device to abort Item's command on Unit (task slot 0), then
   --  clears the slot from the transfer list. Aborted: the device answered.
   procedure Abort_Slot
     (Item : Slot; Unit : Logical_Unit; Aborted : out Boolean);

   --  Pending interrupt sources; Acknowledge clears the given ones.
   function Pending_Interrupts return Register_Value;
   procedure Acknowledge (Sources : Register_Value);
end UFS_Controller;
