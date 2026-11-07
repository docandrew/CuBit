------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Deskbar panel (UI-027) drawn by the native toolkit (CuBit.UI): the
--  clock, Remote access (while a remote session holds Desktop's
--  display-capture endpoint: who is paired and the "Allow screen capture"
--  switch, REMOTE-001), Quick settings (volume slider and mute, do not disturb), the
--  Network section (wired and Wi-Fi rows, the inline passphrase field, the
--  addressing editor) and the notification history, rendered into a
--  pixel buffer Desktop owns and presents as a retained layer.
--
--  @description
--  Immediate mode, as every toolkit view: Render draws every control from
--  Desktop's state and reports what the user did as Actions; Desktop
--  applies them to its models (Desktop_Network_Model,
--  Desktop_Address_Editor, ...) and renders again only when something
--  changed (damage), never per frame. Pointer input is panel-local; keys
--  move the toolkit's focus (CuBit.UI.Focus: Tab order is drawing order),
--  type into the focused toolkit field (CuBit.UI.Editor; the passphrase
--  field is Sensitive: masked, wiped once handed to wifi.svc, never
--  shown), and Enter/Space activate the focused control.
------------------------------------------------------------------------------
with Interfaces;
with System;
with CuBit.UI.Editor;
with CuBit.CPU_Load;
with CuBit.Net_Manager_Report;
with CuBit.Wifi_Control;
with Desktop_Address_Editor;
with Desktop_Network_Model;
with Desktop_Network_Rows;
with Desktop_Notifications;

package Desktop_Status_Panel is
   package DAE renames Desktop_Address_Editor;
   package DNM renames Desktop_Network_Model;
   package DNR renames Desktop_Network_Rows;
   package DN renames Desktop_Notifications;
   package NMR renames CuBit.Net_Manager_Report;

   subtype Percent is Natural range 0 .. 100;
   package CL renames CuBit.CPU_Load;
   type CPU_Loads is array (CL.CTP.Processor) of CL.Load;

   --  The small facts Render shows (the large models are passed as they are).
   type Short_Text is record
      Data   : String (1 .. 48) := [others => ' '];
      Length : Natural range 0 .. 48 := 0;
   end record;
   function To_Short (S : String) return Short_Text;
   function Image (T : Short_Text) return String is (T.Data (1 .. T.Length));

   type View is record
      Clock      : String (1 .. 5) := "--:--";
      Date       : Short_Text;
      Audio      : Boolean := False;    --  a mixer answered
      Level      : Percent := 0;
      Muted      : Boolean := False;
      Quiet      : Boolean := False;
      Note_Rows  : Natural := 0;        --  notifications that fit
      Wifi_Shown : Boolean := False;
      Wifi_State : CuBit.Wifi_Control.State := CuBit.Wifi_Control.No_Radio;
      Joined     : Short_Text;          --  the network joined ("" none)
      Wifi_Address : Interfaces.Unsigned_32 := 0;
      Wifi_Bars  : DNM.Signal_Bars := 0;
      --  The System section (CPU and memory, Desktop_Load_Bars): shown
      --  while the kernel's CPU-times page is granted and published.
      System_Shown : Boolean := False;
      CPU_Count  : Natural range 0 .. CL.CTP.Maximum_Processors := 0;
      CPU_All    : CL.Load;
      CPUs       : CPU_Loads := [others => CL.Unknown];
      Memory_Known : Boolean := False;
      Memory_Used  : Percent := 0;
      --  The Remote access section (Desktop_Capture): shown while a remote
      --  session announced itself on the display-capture endpoint.
      Remote_Shown   : Boolean := False;
      Remote_Paired  : Boolean := False;
      Remote_Peer    : Short_Text;          --  the paired host's address
      Capture_Allowed : Boolean := False;   --  the person's switch
      --  procmgr's "Allow app replacement" switch (staged launches of apps
      --  built on this machine; Desktop_Status_Refresh), shown in the
      --  Remote access section: its own state, not the capture switch's.
      Replacement_Allowed : Boolean := False;
   end record;

   --  The panel's height for this content.
   function Height (Net : DNR.Layout; Note_Rows : Natural; System_Shown, Remote_Shown : Boolean)
     return Natural;

   ---------------------------------------------------------------------------
   --  What the user did.
   ---------------------------------------------------------------------------
   type Action_Kind is
     (Set_Volume, Toggle_Mute, Set_Quiet, Clear_Notes, Dismiss_Note,
      Toggle_Editor, Choose_Network, Forget_Network, Enter_Passphrase, Cancel_Passphrase,
      Choose_Method, Field_Edited, Apply, Revert, Close_Editor,
      --  "Allow screen capture" (Flag: the new state).
      Set_Capture,
      --  "Allow app replacement" (Flag: the new state).
      Set_Replacement);

   type Action is record
      Kind   : Action_Kind := Set_Volume;
      Index  : Natural := 0;               --  row, network, interface
      Level  : Percent := 0;
      Flag   : Boolean := False;
      Note   : DN.Serial := DN.No_Notification;
      Method : DAE.Method := DAE.Automatic;
      Field  : DAE.Field_Name := DAE.Address_Field;
   end record;

   --  Named ceiling: one render or key reports at most this many.
   Maximum_Actions : constant := 8;
   type Action_Array is array (1 .. Maximum_Actions) of Action;
   type Action_List is record
      Items : Action_Array;
      Count : Natural range 0 .. Maximum_Actions := 0;
   end record;

   ---------------------------------------------------------------------------
   --  Drawing and input.
   ---------------------------------------------------------------------------
   --  Draw the whole panel (Width x Height) into the buffer; actions taken
   --  this render are appended to Acts.
   procedure Render
     (V : View; Notes : DN.History; Net : DNR.Layout; Report : NMR.Report;
      Networks : DNM.Network_List; Network_Count : DNM.Network_Count;
      Join : DNM.Join_State; Editor : DAE.Editor; Acts : in out Action_List);

   --  The buffer last rendered (BGRA, one pixel per logical pixel).
   function Buffer return System.Address;
   function Buffer_Width return Natural;
   function Buffer_Height return Natural;
   function Buffer_Pitch return Natural;

   --  Pointer at (X, Y) in panel coordinates; render after.
   procedure Pointer (X, Y : Natural; Down, Pressed, Released : Boolean);

   --  A key (scan code, its character, Shift): Handled when the panel took
   --  it; Desktop renders after a handled key. Escape outside a field is
   --  not handled (Desktop closes the panel).
   procedure Key (Code : Interfaces.Unsigned_8; Ch : Character; Shift : Boolean;
                  Acts : in out Action_List; Handled : out Boolean);

   --  The panel opened: focus starts on the first control.
   procedure Opened;

   --  Where the last render drew the "Allow app replacement" switch, in
   --  panel coordinates (its centre); Drawn False when it was not drawn.
   procedure Replacement_Switch (X, Y : out Natural; Drawn : out Boolean);

   --  Focus the passphrase field once the next render draws it.
   procedure Focus_Passphrase;

   ---------------------------------------------------------------------------
   --  The toolkit fields.
   ---------------------------------------------------------------------------
   --  The passphrase field (Sensitive); Store_Passphrase takes it and wipes.
   function Passphrase return access CuBit.UI.Editor.Edit_State;
   procedure Wipe_Passphrase;

   --  The address fields show E's values (the editor opened or reverted).
   procedure Load_Fields (E : DAE.Editor);
   function Field_Content (F : DAE.Field_Name) return String;
end Desktop_Status_Panel;
