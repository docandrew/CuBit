--  The places the drawer lists (docs/files-app.md, "Places"): what the
--  service answers Queue_List_Places with, named for the person. A
--  volume's root gets its kind's caption ("@nvme:0/" is "NVMe volume 0",
--  "@mem:0/" the live workspace); a scope below a root keeps its path.
package Files_Places with SPARK_Mode is
   type Place_Kind is (Live_Workspace, NVMe_Volume, ATA_Volume, Optical_Disc, Boot_Image, Other_Volume);

   --  The volume names the service registers (its volume list), up to the
   --  ':' and the volume's number.
   MEMORY_DEVICE : constant String := "mem";
   NVME_DEVICE : constant String := "nvme";
   ATA_DEVICE : constant String := "ata";
   OPTICAL_DEVICE : constant String := "cd";
   BOOT_ARCHIVE : constant String := "boot";
   VOLUME_MARK : constant Character := '@';
   NUMBER_MARK : constant Character := ':';
   ROOT_END : constant Character := '/';

   --  Prefix's kind, by the volume it names (Other_Volume: none known).
   function Kind_Of (Prefix : String) return Place_Kind;
   --  Prefix is a volume's root ("@name/" or "@name:N/").
   function Is_Root (Prefix : String) return Boolean;
   --  What the drawer shows for Prefix.
   function Caption (Prefix : String) return String
     with Pre => Prefix'Length < Positive'Last / 2;
end Files_Places;
