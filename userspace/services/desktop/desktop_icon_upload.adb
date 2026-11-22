with Desktop_Icon_Mapping;
with Compositor_Upload;
with System;
package body Desktop_Icon_Upload with SPARK_Mode is
   procedure Start (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Item : Desktop_Icon_Pixels.Asset; Result : out D.Source_Result) is
      use type D.Source_Result;
      Plan : Compositor_Upload.Plan;
      Write : D.Write_Ticket;
      Mapping : System.Address;
      Complete, Cancelled : Boolean;
   begin
      D.Begin_Write (Index, Lease, Write, Plan, Mapping, Result);
      if Result /= D.Source_Accepted then return; end if;
      Desktop_Icon_Mapping.Copy_Chunk (Item, Mapping, Compositor_Upload.Capacity (Plan), Plan, Complete);
      if not Complete then
         D.Cancel_Write (Write, True, Cancelled);
         Result := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         return;
      end if;
      -- The mapping call is synchronous and retains no CPU writer.
      D.Submit_Write (Write, True, Result);
   end Start;
   procedure Start_Atlas (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Kind : Desktop_Icon_Pixels.Family; Result : out D.Source_Result) is
      use type D.Source_Result;
      Plan : Compositor_Upload.Plan;
      Write : D.Write_Ticket;
      Mapping : System.Address;
      Complete, Cancelled : Boolean;
   begin
      D.Begin_Write (Index, Lease, Write, Plan, Mapping, Result);
      if Result /= D.Source_Accepted then return; end if;
      Desktop_Icon_Mapping.Copy_Atlas (Kind, Mapping, Compositor_Upload.Capacity (Plan), Plan, Complete);
      if not Complete then
         D.Cancel_Write (Write, True, Cancelled);
         Result := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         return;
      end if;
      -- The mapping call is synchronous and retains no CPU writer.
      D.Submit_Write (Write, True, Result);
   end Start_Atlas;
end Desktop_Icon_Upload;
