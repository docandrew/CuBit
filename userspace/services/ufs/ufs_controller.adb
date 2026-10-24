------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Busy_Poll;

package body UFS_Controller is
   use type System.Address;

   Register_Words : constant := Register_Block_Bytes / Word_Bytes;
   type Register_Array is array (0 .. Register_Words - 1) of Register_Value
     with Volatile_Components;

   Registers_Base : System.Address := System.Null_Address;
   DMA_Base       : System.Address := System.Null_Address;
   DMA_Physical_Base : Unsigned_64 := 0;

   --  Bring-up deadlines, in microseconds (Linux ufshcd uses the same
   --  bounds: 1 ms enable delay, 10 tries of 1 ms, 500 ms UIC command,
   --  100 ms task management).
   Enable_Settle_Microseconds : constant := 1_000;
   Disable_Microseconds       : constant := 10_000;
   Enable_Microseconds        : constant := 100_000;
   UIC_Ready_Microseconds     : constant := 100_000;
   UIC_Command_Microseconds   : constant := 500_000;
   List_Ready_Microseconds    : constant := 100_000;
   Task_Microseconds          : constant := 100_000;
   Clear_Microseconds         : constant := 100_000;

   function Register_File return System.Address is (Registers_Base);

   UIC_Result : Register_Value := UIC_Success;
   function Last_UIC_Result return Register_Value is (UIC_Result);

   procedure Attach
     (Registers : System.Address; DMA_Virtual : System.Address;
      DMA_Physical : Unsigned_64) is
   begin
      Registers_Base := Registers;
      DMA_Base := DMA_Virtual;
      DMA_Physical_Base := DMA_Physical;
   end Attach;

   function Read (Item : Register) return Register_Value is
      File : Register_Array with Import, Address => Register_File;
   begin
      return File (Offset (Item) / Word_Bytes);
   end Read;

   procedure Write (Item : Register; Value : Register_Value) is
      File : Register_Array with Import, Address => Register_File;
   begin
      File (Offset (Item) / Word_Bytes) := Value;
   end Write;

   --  Spins until Register has all of Mask set (Set) or clear (not Set),
   --  or Micros pass.
   function Wait_For
     (Item : Register; Mask : Register_Value; Set : Boolean;
      Micros : Unsigned_64) return Boolean
   is
      Start : constant Unsigned_64 := CuBit.Busy_Poll.Now;
   begin
      loop
         declare
            Bits : constant Register_Value := Read (Item) and Mask;
         begin
            if (if Set then Bits = Mask else Bits = 0) then
               return True;
            end if;
         end;
         exit when not CuBit.Busy_Poll.Within (Start, Micros);
         CuBit.Busy_Poll.Relax;
      end loop;
      return False;
   end Wait_For;

   procedure Pause (Micros : Unsigned_64) is
      Start : constant Unsigned_64 := CuBit.Busy_Poll.Now;
   begin
      while CuBit.Busy_Poll.Within (Start, Micros) loop
         CuBit.Busy_Poll.Relax;
      end loop;
   end Pause;

   procedure Enable (Result : out Step_Result) is
   begin
      if (Read (Controller_Enable) and Enable_Controller) /= 0 then
         Write (Controller_Enable, 0);
         if not Wait_For (Controller_Enable, Enable_Controller, False,
                          Disable_Microseconds)
         then
            Result := Step_Timed_Out;
            return;
         end if;
      end if;
      Write (Controller_Enable, Enable_Controller);
      Pause (Enable_Settle_Microseconds);
      if not Wait_For (Controller_Enable, Enable_Controller, True,
                       Enable_Microseconds)
        or else not Wait_For (Controller_Status, UIC_Command_Ready, True,
                              UIC_Ready_Microseconds)
      then
         Result := Step_Timed_Out;
         return;
      end if;
      Result := Step_Done;
   end Enable;

   procedure UIC
     (Operation  : UIC_Operation;
      Argument_1 : Register_Value;
      Argument_3 : Register_Value;
      Value      : out Register_Value;
      Result     : out Step_Result) is
   begin
      Value := 0;
      if not Wait_For (Controller_Status, UIC_Command_Ready, True,
                       UIC_Ready_Microseconds)
      then
         Result := Step_Timed_Out;
         return;
      end if;
      Write (Interrupt_Status, UIC_Command_Completed);
      Write (UIC_Argument_1, Argument_1);
      Write (UIC_Argument_2, 0);
      Write (UIC_Argument_3, Argument_3);
      Write (UIC_Command, Command_Word (Operation));
      if not Wait_For (Interrupt_Status, UIC_Command_Completed, True,
                       UIC_Command_Microseconds)
      then
         Result := Step_Timed_Out;
         return;
      end if;
      Write (Interrupt_Status, UIC_Command_Completed);
      Value := Read (UIC_Argument_3);
      UIC_Result := Read (UIC_Argument_2) and Result_Code_Mask;
      Result := (if UIC_Result = UIC_Success then Step_Done else Step_Failed);
   end UIC;

   Low_Word : constant Unsigned_64 := 16#FFFF_FFFF#;

   procedure Start_Lists (Result : out Step_Result) is
      Transfer_List : constant Unsigned_64 :=
        DMA_Physical_Base + Transfer_List_Offset;
      Task_List : constant Unsigned_64 := DMA_Physical_Base + Task_List_Offset;
   begin
      if not Wait_For (Controller_Status,
                       Transfer_List_Ready or Task_List_Ready, True,
                       List_Ready_Microseconds)
      then
         Result := Step_Timed_Out;
         return;
      end if;
      Write (Transfer_List_Base_Low, Register_Value (Transfer_List and Low_Word));
      Write (Transfer_List_Base_High,
             Register_Value (Shift_Right (Transfer_List, 32)));
      Write (Task_List_Base_Low, Register_Value (Task_List and Low_Word));
      Write (Task_List_Base_High, Register_Value (Shift_Right (Task_List, 32)));
      Write (Transfer_List_Run_Stop, List_Running);
      Write (Task_List_Run_Stop, List_Running);
      Result := Step_Done;
   end Start_Lists;

   function Slot_Bit (Item : Slot) return Register_Value is (2 ** Item);

   function At_Offset (Offset : Natural) return System.Address is
     (DMA_Base + Storage_Offset (Offset));

   procedure Post
     (Item      : Slot;
      Request   : Request_Frame;
      Direction : Transfer_Direction;
      Bytes     : Natural)
   is
      Table : constant Natural := Command_Table_At (Item);
      Request_View : Request_Frame
        with Import, Volatile, Address => At_Offset (Table + Request_Offset);
      Regions_View : Region_Words
        with Import, Volatile, Address => At_Offset (Table + Regions_Offset);
      Descriptor_View : Descriptor_Words
        with Import, Volatile,
             Address => At_Offset (Transfer_Descriptor_At (Item));
      Response_View : Response_Area
        with Import, Volatile, Address => At_Offset (Table + Response_Offset);
   begin
      Response_View := [others => 0];
      Request_View := Request;
      if Bytes > 0 then
         Regions_View := Region
           (DMA_Physical_Base + Unsigned_64 (Buffer_At (Item)), Bytes);
      end if;
      Descriptor_View := Transfer_Descriptor
        (Direction       => Direction,
         Command_Table   => DMA_Physical_Base + Unsigned_64 (Table),
         Response_Offset => Response_Offset_Words,
         Response_Words  => Response_Length_Words,
         Regions_Offset  => Regions_Offset_Words,
         Regions         => (if Bytes > 0 then 1 else 0));
      --  The doorbell write orders after the descriptor stores (volatile,
      --  and x86 keeps stores in order).
      Write (Transfer_Doorbell, Slot_Bit (Item));
   end Post;

   function Completed (Item : Slot) return Boolean is
     ((Read (Transfer_Doorbell) and Slot_Bit (Item)) = 0);

   function Status (Item : Slot) return Command_Status is
      Descriptor_View : Descriptor_Words
        with Import, Volatile,
             Address => At_Offset (Transfer_Descriptor_At (Item));
      Copy : constant Descriptor_Words := Descriptor_View;
   begin
      return Status_Of (Copy);
   end Status;

   function Response (Item : Slot) return Response_Area is
      Response_View : Response_Area
        with Import, Volatile,
             Address => At_Offset (Command_Table_At (Item) + Response_Offset);
   begin
      return Response_View;
   end Response;

   function Buffer (Item : Slot) return System.Address is
     (At_Offset (Buffer_At (Item)));

   --  The one task slot this driver uses.
   Task_Bit : constant Register_Value := 2 ** 0;

   procedure Abort_Slot
     (Item : Slot; Unit : Logical_Unit; Aborted : out Boolean)
   is
      Task_View : Task_Descriptor
        with Import, Volatile, Address => At_Offset (Task_List_Offset);
      --  Tag the abort request past the transfer slots' tags.
      Abort_Tag : constant Task_Tag := Driver_Slots;
   begin
      Task_View := Abort_Request (Abort_Tag, Unit, Task_Tag (Item));
      Write (Task_Doorbell, Task_Bit);
      Aborted := Wait_For (Task_Doorbell, Task_Bit, False, Task_Microseconds);
      if Aborted then
         declare
            Answer : constant Task_Descriptor := Task_View;
         begin
            Aborted := Decode_Task_Response (Answer, Abort_Tag) = Abort_Completed;
         end;
      end if;
      --  Writing zero to a slot's bit in UTRLCLR removes it from the list.
      Write (Transfer_List_Clear, not Slot_Bit (Item));
      if not Wait_For (Transfer_Doorbell, Slot_Bit (Item), False,
                       Clear_Microseconds)
      then
         Aborted := False;
      end if;
   end Abort_Slot;

   function Pending_Interrupts return Register_Value is
     (Read (Interrupt_Status));

   procedure Acknowledge (Sources : Register_Value) is
   begin
      Write (Interrupt_Status, Sources);
   end Acknowledge;
end UFS_Controller;
