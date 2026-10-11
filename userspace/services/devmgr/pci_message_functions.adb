------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  MSI / MSI-X function lifecycle - implementation
------------------------------------------------------------------------------
package body PCI_Message_Functions with SPARK_Mode => On is

   procedure Execute (Index : Entry_Index; Plan : Entry_Plan) is
   begin
      for I in 1 .. Plan.Length loop
         Write_Entry (Index, Plan.Writes (I).Register, Plan.Writes (I).Value);
      end loop;
   end Execute;

   procedure Execute (Plan : Config_Plan) is
   begin
      for I in 1 .. Plan.Length loop
         case Plan.Writes (I).Width is
            when Word_16 =>
               Write_Config_16 (Plan.Writes (I).Offset,
                                Unsigned_16 (Plan.Writes (I).Value and 16#FFFF#));
            when Word_32 =>
               Write_Config_32 (Plan.Writes (I).Offset, Plan.Writes (I).Value);
         end case;
      end loop;
   end Execute;

   procedure Program_MSIX
     (Head : Device_Vector; Count : Block_Size; Destination : Destination_ID)
   is
      Control : constant Unsigned_16 := Read_Config_16 (MSIX_CONTROL_OFFSET);
   begin
      --  Function masked first: no entry can send while any is half-written.
      Write_Config_16 (MSIX_CONTROL_OFFSET, MSIX_Enabled_Masked (Control));
      for I in 0 .. Count - 1 loop
         Execute (I, Program_Entry (Destination, Head + I, Unmask => False));
      end loop;
   end Program_MSIX;

   procedure Open_MSIX (Count : Block_Size) is
   begin
      for I in 0 .. Count - 1 loop
         Write_Entry (I, Vector_Control, VECTOR_UNMASKED);
      end loop;
      Write_Config_16 (MSIX_CONTROL_OFFSET,
                       MSIX_Function_Unmasked (Read_Config_16 (MSIX_CONTROL_OFFSET)));
   end Open_MSIX;

   procedure Close_MSIX (Count : Block_Size) is
   begin
      --  Each entry's own mask, the one PCI defines for this: the device
      --  holds anything it raises in the PBA, and MSI-X stays enabled so
      --  the function never falls back to its INTx line.
      for I in 0 .. Count - 1 loop
         Execute (I, Mask_Entry);
      end loop;
   end Close_MSIX;

   procedure Program_MSI (Vector : Device_Vector; Destination : Destination_ID) is
   begin
      Execute (PCI_Message_Interrupts.Program_MSI
                 (Read_Config_16 (MSI_CONTROL_OFFSET), Destination, Vector));
   end Program_MSI;

   procedure Prepare_MSI (Vector : Device_Vector; Destination : Destination_ID) is
   begin
      Execute (PCI_Message_Interrupts.Prepare_MSI
                 (Read_Config_16 (MSI_CONTROL_OFFSET), Destination, Vector));
   end Prepare_MSI;

   procedure Close_MSI is
   begin
      Execute (Disable_MSI (Read_Config_16 (MSI_CONTROL_OFFSET)));
   end Close_MSI;

end PCI_Message_Functions;
