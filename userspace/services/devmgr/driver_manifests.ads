------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a driver's manifest asks of its device (docs/msi-interrupts.md)
--
--  @description
--  A driver's typed CCL manifest (manifest.ccl, Executable_Manifest) is
--  compiled into its ELF: the .cubit.resources section lists its device
--  requests, each with the capability slot its generated bindings
--  (CCL_Manifest_Bindings.Slot_<name>) refer to. devmgr finds that section
--  in the image it spawns, decodes it with the proved startup decoder
--  (CCL.Resource_Sections.Decode), and installs the driver's interrupt
--  capability in the slot the manifest named. No slot number is written by
--  hand on either side.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CCL.Resource_Sections;

package Driver_Manifests with SPARK_Mode => On is
   use CCL.Resource_Sections;

   type Interrupt_Request is record
      Present : Boolean := False;
      Slot    : Slot_Number := Slot_Number'First;
      Mode    : Interrupt_Mode := MSI_X;
      Vectors : Positive range 1 .. MAX_INTERRUPT_VECTORS := 1;
   end record;

   --  The image's first message-signaled interrupt request (MSI or MSI-X),
   --  or Present False: no section, a section the decoder refuses, or no
   --  such request.
   function Interrupt_Of (Image : Byte_Array) return Interrupt_Request
   with Pre => Image'First = 1 and then Image'Last < Natural'Last;

end Driver_Manifests;
