with Boot_Modules;
with Physical_Ranges;
package Multiboot is
   procedure Describe_Physical_Ranges (M : in out Physical_Ranges.Map);
   function Boot_Module_Count return Natural is (0);
   function Boot_Module (Index : Positive) return Boot_Modules.Image;
end Multiboot;
