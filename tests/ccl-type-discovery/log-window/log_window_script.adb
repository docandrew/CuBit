package body Log_Window_Script is
   function Instance
     (Source : Unsigned_64; Name : String; Records : Natural; First_Time, Spacing : Logs.Boot_Milliseconds;
      Gap_Before : Natural := 0; Gap_Count : Unsigned_64 := 0) return Scripted_Instance
   is
      Result : Scripted_Instance :=
        (Source => Source, Length => Name'Length, Records => Records, First_Time => First_Time,
         Spacing => Spacing, Gap_Before => Gap_Before, Gap_Count => Gap_Count, others => <>);
   begin
      Result.Name (1 .. Name'Length) := Name;
      return Result;
   end Instance;

   --  acpi: one instance of 151 records (the laptop's count), 5 not
   --  retained before the record at 1500. wifi: two instances. many: ten
   --  instances, more than one result shows (CCL_Log_Selection's
   --  MAXIMUM_INSTANCES), newest first, two records each.
   ACPI_RECORDS : constant := 151;
   ACPI : constant Instance_List :=
     [1 => Instance (7, "acpi", ACPI_RECORDS, 1_000, 10, Gap_Before => 51, Gap_Count => 5)];
   WIFI : constant Instance_List := [Instance (12, "wifi", 3, 500, 10), Instance (11, "wifi", 3, 100, 10)];
   MANY_INSTANCES : constant := 10;
   MANY : constant Instance_List :=
     [for J in 1 .. MANY_INSTANCES =>
        Instance (Unsigned_64 (40 - J + 1), "many", 2, Logs.Boot_Milliseconds (1_000 * (MANY_INSTANCES - J + 1)), 1)];
   --  busy: one instance holding a whole retention plan's history (4096
   --  records, the laptop's logs.retention per source). flood: more records
   --  than one result shows (CCL.Interfaces.Logs.RESULT_ITEM_CEILING) by
   --  FLOOD_OVER.
   BUSY_RECORDS : constant := 4_096;
   BUSY : constant Instance_List := [1 => Instance (21, "busy", BUSY_RECORDS, 10_000, 1)];
   FLOOD : constant Instance_List :=
     [1 => Instance (22, "flood", Logs.RESULT_ITEM_CEILING + FLOOD_OVER, 100_000, 1)];
   NONE : constant Instance_List (1 .. 0) := [others => <>];

   function Known (Service : String) return Boolean is
     (Service = "acpi" or else Service = "wifi" or else Service = "many" or else Service = "busy" or else
      Service = "flood");

   function Instances_Of (Service : String) return Instance_List is
     (if Service = "acpi" then ACPI elsif Service = "wifi" then WIFI elsif Service = "many" then MANY
      elsif Service = "busy" then BUSY elsif Service = "flood" then FLOOD else NONE);

   function Step_Of (Instance : Scripted_Instance; Index : Positive) return Scripted_Step is
      Image : constant String := Positive'Image (Index - 1);
      Text : constant String := Instance.Name (1 .. Instance.Length) & ": step" & Image;
      Result : Scripted_Step :=
        (Time => Instance.First_Time + Instance.Spacing * Logs.Boot_Milliseconds (Index - 1),
         Not_Retained => (if Index = Instance.Gap_Before then Instance.Gap_Count else 0), others => <>);
   begin
      Result.Value.Length := Text'Length;
      Result.Value.Text (1 .. Text'Length) := Text;
      return Result;
   end Step_Of;
end Log_Window_Script;
