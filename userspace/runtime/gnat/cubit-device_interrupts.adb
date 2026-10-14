------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Message-signaled device interrupts - implementation
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Control;
with CuBit.Handles.IPC;

package body CuBit.Device_Interrupts is

   use type CuBit.Handles.ABI.Kind;

   function Block_Of (Item : CuBit.Handles.Interrupt) return Interrupt_Block is
      D : constant CuBit.Handles.IPC.Description := CuBit.Handles.IPC.Describe (Item);
   begin
      if not D.Current or else D.Kind /= CuBit.Handles.ABI.Interrupt
        or else D.Object not in First_Vector .. Last_Vector
        or else D.Parameter not in 1 .. Max_Block_Vectors
        or else D.Parameter - 1 > Last_Vector - D.Object
      then
         return (others => <>);
      end if;
      return (Valid => True,
              Head  => Device_Vector (D.Object),
              Count => Block_Size (D.Parameter));
   end Block_Of;

   function Fired (Event : Message; Block : Interrupt_Block; Index : Vector_Index)
     return Boolean
   is
      Vector : constant Device_Vector := Block.Head + Index;
   begin
      return Is_Doorbell (Event)
        and then (Event.words (Word_Of (Vector)) and Mask_Of (Vector)) /= 0;
   end Fired;

   function Allocate
     (Owner : Process_ID; Space : CuBit.Handles.Cspace; Count : Block_Size;
      CPU : Natural; Authority : CuBit.Handles.Interrupt) return Allocation
   is
      Block : constant CuBit.Handles.Control.Vector_Block :=
        CuBit.Handles.Control.Allocate_Vectors
          (Owner, Space, Unsigned_64 (Count), Unsigned_64 (CPU), Authority);
   begin
      if not Block.Allocated then
         return (Why => Block.Why, others => <>);
      elsif not Valid_Allocation (Block.Allocation) then
         return (others => <>);
      end if;
      return (Valid        => True,
              Head         => Head_Of (Block.Allocation),
              Count        => Count_Of (Block.Allocation),
              Destination  => Destination_Of (Block.Allocation),
              Owner_Handle => Block.Owner,
              Why          => CuBit.Handles.ABI.Invalid_Argument);
   end Allocate;

   function Release
     (Head : Device_Vector; Authority : CuBit.Handles.Interrupt) return Boolean is
     (CuBit.Handles.Control.Release_Vectors (Unsigned_64 (Head), Authority));

   function Read
     (Vector : Device_Vector; Field : Read_Field; Authority : CuBit.Handles.Interrupt)
      return Unsigned_64 is
     (CuBit.Handles.Control.Read_Vector
        (Unsigned_64 (Vector), Unsigned_64 (Read_Field'Enum_Rep (Field)), Authority));

end CuBit.Device_Interrupts;
