pragma Ada_2022;
with GPIO_Native_Resources;
with Launch_Configuration;
package GPIO_Native_Launch is
   type Outcome is (Validated, No_Configuration, Wrong_Controller,
      Invalid_Identity, Wrong_Window_Count, Invalid_Window,
      Missing_Authority, Invalid_Authority, Window_Mismatch, Overlapping_Windows);
   type Report is record
      Status : Outcome := No_Configuration;
      Parent : Launch_Configuration.ACPI_Parent_Line;
      At_Resource : GPIO_Native_Resources.G.Memory_Resource_Ordinal := 0;
   end record;
   -- Read the kernel-installed ACPI launch configuration and inspect all four
   -- required handles before ANY map/probe/register access. Configuration is
   -- descriptive; only the matching typed capabilities supply authority.
   procedure Validate (Caps : GPIO_Native_Resources.Authorities; Result : out Report);
end GPIO_Native_Launch;
