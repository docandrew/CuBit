package body Multiboot is
   procedure Describe_Physical_Ranges (M : in out Physical_Ranges.Map) is
      pragma Unreferenced (M);
   begin
      null;
   end Describe_Physical_Ranges;
   function Boot_Module (Index : Positive) return Boot_Modules.Image is
      pragma Unreferenced (Index);
   begin
      raise Program_Error with "no hosted boot modules";
      return (others => <>);
   end Boot_Module;
end Multiboot;
