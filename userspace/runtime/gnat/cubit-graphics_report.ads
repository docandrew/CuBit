------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The renderer report (GPU-014): which path renders each output and each
--  GPU or Vulkan consumer (hardware Vulkan, Venus, or software), and when
--  it is software, why, as a typed reason. devmgr holds it; the parts that
--  know a path post their entries to it; observers read it.
--
--  @description
--  devmgr hosts the report because it runs on every image and outlives the
--  components most likely to fail (Desktop died at start on the T580,
--  BUG-009): the report must still answer then, with Desktop's last
--  entries marked stale.
--
--  Contributors post one-way (Post_Label, a send, never a call: the IPC
--  graph only flows down, docs/ipc-delivery.md): Desktop for its outputs,
--  procmgr for each launch's render admission, the toolkit's GPU canvas for
--  each app's path and Vulkan result. An entry is keyed by its subject
--  (kind and id); a newer post replaces it. When its poster exits, devmgr
--  keeps the last value and marks it Stale.
--
--  A message carries four words, an entry Entry_Words: it goes in pages.
--  Posting is two calls from one poster, each acknowledged at once with an
--  empty reply of its own label: Post_Label with the entry's words
--  0 .. 3, then Post_End_Label with words 4 .. 7; devmgr pairs them by the
--  sender and keeps the entry only when the pair decodes (Valid). The badge names a poster; devmgr
--  ignores a post from a badge that is not Poster_Tag, and an end without
--  its matching start.
--
--  procmgr, devmgr's parent, posts on its own endpoint (no badge needed;
--  devmgr knows its pid) and calls Gone_Label (Gone_Words: the pid that
--  exited) when a program ends: that poster's entries turn Stale, and the
--  program's own entry with them.
--
--  Readers, holding devmgr's endpoint badged Observer_Tag (catalog role
--  Observer_Role, "graphics-observer", minted by procmgr for a manifest that
--  requests it, startup plan only):
--  Summary_Label (Request_Words): word 0 the request id (never zero).
--    Reply Summary_Label: the id, the generation, the entry count, the
--    posts dropped for want of room.
--  Entry_Label (Entry_Request_Words): the id, the generation the summary
--    gave, the index (1 .. count), the page (0 .. Pages - 1). Reply
--    Entry_Label: the id, the generation, the page's Page_Words words
--    (the entry's words 2 * page and 2 * page + 1).
--  Refusals: Refused_Label, the id and Refusal'Enum_Rep.
--  A reader reads every page against one generation; a report that
--  changed meanwhile is refused Stale_Generation and the reader starts
--  again.
--
--  Every word on the wire is at most Integer_64'Last (a CCL integer), and
--  the decoders accept exactly what the encoders write.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Graphics_Report with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  The contract.
   ---------------------------------------------------------------------------

   --  What is rendered: an output (Desktop's composition of it), a
   --  program that asked for a GPU or Vulkan path, or a GPU itself (devmgr's
   --  own entry: the device and its driver, the chain's root).
   type Subject_Kind is (Output, Program, GPU);

   type Path_Kind is
     (Hardware,    --  Vulkan on a GPU driver CuBit runs (intel-gpu)
      Venus,       --  Vulkan forwarded to the VM host's GPU (virtio-gpu 3D)
      Software);   --  the CPU (the compositor's, or an app's own renderer)

   --  Why a path is software (None for Hardware and Venus).
   type Reason is
     (None,
      No_GPU_Device,          --  no display controller a GPU driver could drive
      No_Driver_Bound,        --  a GPU is present; no driver for it runs
      Driver_Failed,          --  its driver stopped (Detail: the Stage)
      Venus_Not_Offered,      --  the VM's GPU has no 3D (Detail: Venus_Gap)
      Manifest_Lacks_Render,  --  the program's manifest asks no render session
      Policy,                 --  a setting chose software (Detail: Policy_Setting)
      Admission_Refused,      --  the render broker said no (Detail: Admission_Outcome)
      ICD_Init_Failed,        --  Mesa or the ICD failed (Detail: the VkResult)
      Startup_Timed_Out,      --  GPU start-up did not finish in its budget
      Fell_Back);             --  ran on the GPU, then fell back (Detail: Fallback_Cause)

   type Driver_Stage is (Probe, Firmware, Init, Modeset, Render);
   type Venus_Gap is (No_3D, No_Capset, No_Hostmem);
   type Policy_Setting is
     (Render_GPU_Off,     --  desktop.render.gpu is off
      Composition_CPU);   --  desktop.composition is CPU: the designed default (LAT-002)
   type Admission_Outcome is (Rejected, Unanswered, Uncertain);
   type Fallback_Cause is (Device_Lost, Scene_Stalled, Surface_Declined, Readback_Failed);

   --  A reason's detail: a stage, gap, setting, outcome or cause by
   --  position, or the VkResult itself (negative for errors).
   Maximum_Detail : constant := 2 ** 31 - 1;
   subtype Detail_Value is Integer_32 range -Maximum_Detail .. Maximum_Detail;

   --  A short printable name ("output-0", "com.cubit.neon-blast" cut to
   --  fit, "intel-gpu"), padded with spaces.
   Name_Capacity : constant := 16;
   subtype Name_Length is Natural range 0 .. Name_Capacity;
   subtype Name_Character is Character range ' ' .. '~';
   subtype Name_Position is Positive range 1 .. Name_Capacity;
   type Name_Text is array (Name_Position) of Character;
   type Name is record
      Length : Name_Length := 0;
      Text   : Name_Text := [others => ' '];
   end record
     with Predicate => (for all C of Name.Text => C in Name_Character);
   No_Name : constant Name := (Length => 0, Text => [others => ' ']);
   function Image (Item : Name) return String is (String (Item.Text (1 .. Item.Length)));
   --  Text cut to Name_Capacity, unprintable characters as '?'.
   function To_Name (Text : String) return Name;

   --  A program's pid is its identity's word (CuBit.Process_IDs.To_Word):
   --  every word here stays a CCL integer.
   subtype Subject_Id is Unsigned_64 range 0 .. Unsigned_64 (Integer_64'Last);
   Maximum_Milliseconds : constant := 2 ** 62;
   subtype Milliseconds is Unsigned_64 range 0 .. Maximum_Milliseconds;

   type Report_Entry is record
      Kind    : Subject_Kind := Output;
      Id      : Subject_Id := 0;          --  the output's index, the program's pid, the GPU's PCI location
      Subject : Name := No_Name;          --  the output's or program's name
      Path    : Path_Kind := Hardware;
      Why     : Reason := None;
      Detail  : Detail_Value := 0;
      Device  : Name := No_Name;          --  Hardware: the GPU; Software: the renderer
      Since   : Milliseconds := 0;        --  monotonic, when this path was taken
      Stale   : Boolean := False;         --  its poster is gone: the last value
   end record
     with Predicate => (Path = Software) = (Why /= None);

   --  Names for log lines (the runtime discards enumeration images).
   function Path_Name (Path : Path_Kind) return String is
     (case Path is when Hardware => "hardware", when Venus => "venus", when Software => "software");
   function Reason_Name (Why : Reason) return String is
     (case Why is
        when None => "none", when No_GPU_Device => "no-gpu-device", when No_Driver_Bound => "no-driver-bound",
        when Driver_Failed => "driver-failed", when Venus_Not_Offered => "venus-not-offered",
        when Manifest_Lacks_Render => "manifest-lacks-render", when Policy => "policy",
        when Admission_Refused => "admission-refused", when ICD_Init_Failed => "icd-init-failed",
        when Startup_Timed_Out => "startup-timed-out", when Fell_Back => "fell-back");

   --  The same subject (a newer post replaces the entry).
   function Same_Subject (A, B : Report_Entry) return Boolean is (A.Kind = B.Kind and then A.Id = B.Id);

   --  How Desktop's Deskbar shows an entry (the coordinator's three cases,
   --  2026-11-25): Desktop's own CPU composition is the designed default,
   --  in the report but never a warning; a consumer on software by policy
   --  is dim; one on software because something is missing, failed,
   --  refused or timed out is amber.
   type Indicator is (Quiet, Dim, Amber);
   function Indicator_Of (Item : Report_Entry) return Indicator is
     (if Item.Path /= Software then Quiet
      elsif Item.Why = Policy and then Item.Kind = Output
        and then Item.Detail = Policy_Setting'Pos (Composition_CPU) then Quiet
      elsif Item.Why = Policy then Dim
      else Amber);

   ---------------------------------------------------------------------------
   --  The wire.
   ---------------------------------------------------------------------------
   Observer_Role : constant := 50;
   Observer_Tag  : constant Unsigned_64 := 16#4752_5052_5254_0001#;
   function Is_Observer (Tag : Unsigned_64) return Boolean is (Tag = Observer_Tag);
   Poster_Role : constant := 51;
   Poster_Tag  : constant Unsigned_64 := 16#4752_5052_5254_0002#;
   function Is_Poster (Tag : Unsigned_64) return Boolean is (Tag = Poster_Tag);

   Post_Label     : constant Unsigned_32 := 16#0290#;
   Post_End_Label : constant Unsigned_32 := 16#0294#;
   Summary_Label  : constant Unsigned_32 := 16#0291#;
   Entry_Label    : constant Unsigned_32 := 16#0292#;
   Refused_Label  : constant Unsigned_32 := 16#0293#;
   Gone_Label     : constant Unsigned_32 := 16#0295#;
   Gone_Words     : constant := 1;
   function Is_Report_Label (Label : Unsigned_32) return Boolean is
     (Label in Post_Label | Post_End_Label | Summary_Label | Entry_Label | Gone_Label);

   subtype Word is Unsigned_64;
   Maximum_Word : constant Word := Word (Integer_64'Last);
   subtype Wire_Word is Word range 0 .. Maximum_Word;

   Entry_Words : constant := 8;
   subtype Entry_Position is Natural range 0 .. Entry_Words - 1;
   type Entry_Wire is array (Entry_Position) of Wire_Word;

   function Encode (Item : Report_Entry) return Entry_Wire;
   function Valid (Wire : Entry_Wire) return Boolean;
   function Decode (Wire : Entry_Wire) return Report_Entry
     with Pre => Valid (Wire);

   type Generation is new Unsigned_64 range 0 .. Unsigned_64 (Integer_64'Last);

   type Refusal is
     (Not_Approved,       --  not the observer's badge
      Malformed,          --  framing, id zero, an index of nothing
      Stale_Generation);  --  the report changed: read the summary again
   for Refusal use (Not_Approved => 1, Malformed => 2, Stale_Generation => 3);

   Request_Word        : constant := 0;
   Generation_Word     : constant := 1;
   Index_Word          : constant := 2;
   Page_Word           : constant := 3;
   Count_Word          : constant := 2;
   Dropped_Word        : constant := 3;
   Payload_Word        : constant := 2;
   Request_Words       : constant := 1;
   Entry_Request_Words : constant := 4;
   Summary_Words       : constant := 4;
   Page_Words          : constant := 2;
   Pages               : constant := (Entry_Words + Page_Words - 1) / Page_Words;
   Reply_Words         : constant := Payload_Word + Page_Words;
   Refusal_Words       : constant := 2;
   Post_Words          : constant := 4;
   subtype Page_Index is Natural range 0 .. Pages - 1;

   pragma Compile_Time_Error (Pages * Page_Words /= Entry_Words, "an entry is whole pages");

   --  Word Offset of an entry's page.
   function Page_Word_Of (Wire : Entry_Wire; Page : Page_Index; Offset : Natural) return Wire_Word is
     (Wire (Page_Words * Page + Offset))
     with Pre => Offset < Page_Words;

end CuBit.Graphics_Report;
