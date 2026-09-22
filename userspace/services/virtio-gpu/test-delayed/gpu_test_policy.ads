with Interfaces;
package GPU_Test_Policy is
   --  Hold head 0 between transfer and scanout, after its DMA command completes.
   Delay_First_Output_Ms : constant Interfaces.Unsigned_64 := 250;
end GPU_Test_Policy;
