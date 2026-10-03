with Desktop_Backdrop_Pixels;
with Desktop_Backdrop_Style;
with Compositor_Upload;
with System;
package body Desktop_Backdrop_Upload with SPARK_Mode is
   procedure Start
     (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Asset : CuBit.Appearance.Background; Result : out D.Source_Result)
   is
      use type D.Source_Result;
      Plan : Compositor_Upload.Plan;
      Write : D.Write_Ticket;
      Mapping : System.Address;
      Complete, Cancelled : Boolean;
   begin
      Result := D.Source_Rejected;
      if not Desktop_Backdrop_Style.Has_Image (Asset) then return; end if;
      D.Begin_Write (Index, Lease, Write, Plan, Mapping, Result);
      if Result /= D.Source_Accepted then return; end if;
      Desktop_Backdrop_Pixels.Copy_Chunk
        (Asset, Mapping, Compositor_Upload.Capacity (Plan), Plan, Complete);
      if not Complete then
         D.Cancel_Write (Write, True, Cancelled);
         Result := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         return;
      end if;
      -- Copy_Chunk is synchronous and retains no source or target pointer.
      D.Submit_Write (Write, True, Result);
   end Start;
end Desktop_Backdrop_Upload;
