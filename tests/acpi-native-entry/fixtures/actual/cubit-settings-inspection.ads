------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config by key, for the programs whose manifest declares config-all
--  (the shell, the Config inspector, the CCL config built-ins): their
--  generated Startup package gives them an Inspector; nothing else does
--  (CAP-003). Config still checks every key against the program's scopes.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Config_Inspection;
with CuBit.Handles.Services;

package CuBit.Settings.Inspection is

   type Inspector (<>) is private;
   --  For the generated Startup package of a config-all manifest.
   function Using (Through : CuBit.Handles.Services.Config.Endpoint) return Inspector;

   --  Present (Item holds the stored text), Absent, Denied or Unavailable.
   procedure Read (Through : Inspector; Key : String; Item : out Text; Result : out Outcome);
   procedure Write (Through : Inspector; Key : String; Item : String; Result : out Change_Outcome);
   procedure Delete (Through : Inspector; Key : String; Result : out Change_Outcome);

   --  Visit each key under Prefix ("" for every key) Config lets this
   --  program see. Result: Present when listed, else why not.
   generic
      with procedure Visit (Key : String);
   procedure For_Each (Through : Inspector; Prefix : String; Result : out Outcome);

   --  The read-only inspection protocol (CuBit.Config_Inspection): one
   --  stored value (Read_Value), the NUL-separated keys under Key
   --  (List_Keys), or whether the program may inspect (Probe), in one
   --  Context. Config checks each against the program's read scopes. A
   --  result over Config_Inspection.Maximum_Text is Too_Large, never
   --  truncated.
   procedure Inspect
     (Through : Inspector; Op : CuBit.Config_Inspection.Operation; Key : String;
      Context : CuBit.Config_Inspection.Context_ID;
      Value : out CuBit.Config_Inspection.Text; Result : out CuBit.Config_Inspection.Status);

private
   type Inspector is record
      Service : CuBit.Handles.Services.Config.Endpoint;
   end record;
   function Using (Through : CuBit.Handles.Services.Config.Endpoint) return Inspector is ((Service => Through));
end CuBit.Settings.Inspection;
