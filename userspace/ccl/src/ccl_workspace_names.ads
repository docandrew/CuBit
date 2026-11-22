package CCL_Workspace_Names with SPARK_Mode => On is
   subtype Revision is Natural range 0 .. 9_999;
   subtype Saved_Revision is Revision range 1 .. Revision'Last;
   function Filename (Number : Saved_Revision; Pending : Boolean) return String;
   procedure Decode
     (Name : String; Number : out Revision; Pending : out Boolean);
end CCL_Workspace_Names;
