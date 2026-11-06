------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Mission profiles and the tools they grant (AGENT-002, docs/agent-harness.md
--  "Sessions and missions", "Tools are typed CCL operations"). A mission's
--  tool set is its authority: a tool it does not grant is absent from the
--  request's tool list and from the binding view the calls run in.
--
--  @description
--  Each tool is one typed CCL operation; its input schema is generated from
--  the operation's CCL signature (Agent_Schemas), and a call's input is
--  decoded into the operation's parameter type (Agent_Decoders). This
--  package holds what the signature does not say: the name the model sees
--  (the API allows no '.'), a description, the label of a scalar
--  parameter, and whether the call names a place that must lie inside the
--  mission's read-only places.
--
--  Profiles name their model explicitly: Diagnose asks claude-opus-5-5
--  (user, 2026-11-04: the default model); a request may name another.
------------------------------------------------------------------------------
pragma Ada_2022;

package Agent_Missions with Pure, SPARK_Mode is

   type Profile is (Diagnose);

   --  Every tool any profile may grant, read-only all (proposal tools come
   --  with the approval path, AGENT-002 step 4).
   type Tool is (Logs_Recent, Processes_List, Files_List, Files_Read);

   function Granted (Mission : Profile; Item : Tool) return Boolean is
     (case Mission is
         when Diagnose => True);

   --  Whether a tool only observes. Every tool here does.
   function Read_Only (Item : Tool) return Boolean is (True);

   Diagnose_Model : constant String := "claude-opus-5-5";

   --  Each tool result fed back to the model is at most this long; a
   --  longer one is cut and labelled (Agent_Results.Truncation_Label with
   --  the full length), as Claude Code caps a tool's output. A named
   --  limit (LIM-001): the model reads long files in pieces.
   Maximum_Tool_Result_Bytes : constant := 16_384;
   function Model_Of (Mission : Profile) return String is
     (case Mission is when Diagnose => Diagnose_Model);
   function Profile_Name (Mission : Profile) return String is
     (case Mission is when Diagnose => "Diagnose");

   function Tool_Name (Item : Tool) return String is
     (case Item is
         when Logs_Recent => "logs_recent",
         when Processes_List => "processes_list",
         when Files_List => "files_list",
         when Files_Read => "files_read");
   --  The CCL operation the tool is.
   function Operation (Item : Tool) return String is
     (case Item is
         when Logs_Recent => "logs.recent",
         when Processes_List => "proc.list",
         when Files_List => "fs.list",
         when Files_Read => "fs.read");
   function Description (Item : Tool) return String is
     (case Item is
         when Logs_Recent =>
           "The recent log records of one CuBit service (time, severity, source process, message), " &
           "newest last. The service is its log name, such as acpi, devmgr, llm or tls.",
         when Processes_List =>
           "Every running CuBit process: pid, name, manifest identity, scheduler state, memory in " &
           "bytes, the pid that launched it, and its age in milliseconds. Takes no input.",
         when Files_List =>
           "The entries of a directory in one of the mission's read-only places: name, kind, size, " &
           "modification time. root is the place's root exactly as given; path is relative to it, " &
           "with '/' between names and no '.' or '..'.",
         when Files_Read =>
           "Up to length bytes (at most 8192) of a file in one of the mission's read-only places, " &
           "from offset; fewer at the file's end. child.place.root is the place's root exactly as " &
           "given, child.place.path the directory relative to it, child.name the file's name. " &
           "Read a long file in pieces.");
   --  The property name of a scalar (non-record) parameter.
   function Parameter_Label (Item : Tool) return String is
     (case Item is
         when Logs_Recent => "service",
         when Processes_List => "value",
         when Files_List | Files_Read => "value");
   --  The call names a place, checked against the mission's places.
   function Names_Place (Item : Tool) return Boolean is (Item in Files_List | Files_Read);

   --  The tool the model named, if a tool has that name at all.
   procedure Find (Name : String; Item : out Tool; Found : out Boolean)
   with Post => (if Found then Tool_Name (Item) = Name);

   ---------------------------------------------------------------------------
   --  Places.
   ---------------------------------------------------------------------------
   Maximum_Path : constant := 255;

   --  A path inside a place: names separated by single '/', no '.' or '..'
   --  name, no leading or trailing '/', printable characters only. The
   --  empty path is the place's root itself.
   function Confined_Path (Path : String) return Boolean
   with Post => (if Confined_Path'Result then
                   Path'Length <= Maximum_Path and then
                   (for all C of Path => C in ' ' .. '~' and then C /= '\'));

   --  A name within a directory: one path component.
   function Confined_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Path and then Name /= "." and then Name /= ".." and then
      (for all C of Name => C in ' ' .. '~' and then C not in '/' | '\'));
end Agent_Missions;
