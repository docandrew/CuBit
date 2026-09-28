------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Binding to the embedded NetSurf engine (userspace/c/netsurf/
--  netsurf-embed-cubit.c). NetSurf fetches, lays out and paints pages into
--  a view the shell provides; the shell owns every piece of UI. Engine
--  notifications arrive through the exported callbacks below and are kept
--  here until the shell collects them.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with CuBit.UI;
--  The C fetcher keeps its network channel rings with these entry points.
with CuBit.Channel_Rings_C;
pragma Warnings (Off, CuBit.Channel_Rings_C);

package Browser_Engine is
   package C renames Interfaces.C;

   ---------------------------------------------------------------------------
   --  Engine calls
   ---------------------------------------------------------------------------

   --  Initialise NetSurf, open URL (the build's homepage when Length is 0)
   --  in a Width x Height view, and fetch https: through the tls.svc
   --  endpoint in TLS_Slot. Zero on success.
   function Start
     (URL : System.Address; Length : C.size_t; Width, Height : C.int;
      TLS_Slot : Unsigned_64) return C.int
     with Import, Convention => C, External_Name => "cubit_netsurf_start";

   function Navigate (Text : System.Address; Length : C.size_t) return C.int
     with Import, Convention => C, External_Name => "cubit_netsurf_navigate";

   procedure Back
     with Import, Convention => C, External_Name => "cubit_netsurf_back";
   procedure Forward
     with Import, Convention => C, External_Name => "cubit_netsurf_forward";
   procedure Reload
     with Import, Convention => C, External_Name => "cubit_netsurf_reload";
   procedure Stop
     with Import, Convention => C, External_Name => "cubit_netsurf_stop";
   function Can_Go_Back return C.int
     with Import, Convention => C,
          External_Name => "cubit_netsurf_can_go_back";
   function Can_Go_Forward return C.int
     with Import, Convention => C,
          External_Name => "cubit_netsurf_can_go_forward";

   procedure Resize (Width, Height : C.int)
     with Import, Convention => C, External_Name => "cubit_netsurf_resize";
   procedure Scroll_To (X, Y : C.int)
     with Import, Convention => C, External_Name => "cubit_netsurf_scroll_to";

   --  Paint the damaged part (CX, CY, CW, CH) of a view whose top-left pixel
   --  is at Pixels, with Pitch bytes per row. 32-bit XRGB.
   procedure Redraw
     (Pixels : System.Address; Width, Height, Pitch : C.int;
      CX, CY, CW, CH : C.int)
     with Import, Convention => C, External_Name => "cubit_netsurf_redraw";

   --  Kind: 0 move, 1 press, 2 release, 3 leave. View coordinates.
   procedure Pointer (Kind, X, Y, Primary : C.int)
     with Import, Convention => C, External_Name => "cubit_netsurf_pointer";
   procedure Wheel (X, Y, Wheel_Delta : C.int)
     with Import, Convention => C, External_Name => "cubit_netsurf_wheel";
   --  Desktop scancode and modifiers (1 shift, 2 ctrl).
   procedure Key (Scancode, Modifiers : C.unsigned)
     with Import, Convention => C, External_Name => "cubit_netsurf_key";
   procedure Text (Code_Point : Unsigned_32)
     with Import, Convention => C, External_Name => "cubit_netsurf_text";

   --  Run due timers and fetch polling; milliseconds until the next
   --  scheduled callback, or negative for none.
   function Poll return C.int
     with Import, Convention => C, External_Name => "cubit_netsurf_poll";

   ---------------------------------------------------------------------------
   --  Engine state, updated by the callbacks
   ---------------------------------------------------------------------------

   Max_Text : constant := 2_048;
   type Text_Buffer is record
      Data : String (1 .. Max_Text) := [others => ' '];
      Length : Natural := 0;
   end record;
   function Value (Item : Text_Buffer) return String is
     (Item.Data (1 .. Item.Length));

   URL, Title, Status : Text_Buffer;
   URL_Changed, Title_Changed : Boolean := False;
   Busy : Boolean := False;
   Extent_Width, Extent_Height : Natural := 0;
   Scroll_X, Scroll_Y : Natural := 0;
   Cursor : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;

   --  Pending page damage in view coordinates (Damage_All: the whole view),
   --  and whether chrome that mirrors engine state (buttons, scrollbar,
   --  status) needs repainting. Cleared by the shell when collected.
   Damage : CuBit.UI.Rect := (others => 0);
   Damage_All : Boolean := False;
   Chrome_Changed : Boolean := False;
   Cursor_Changed : Boolean := False;

   ---------------------------------------------------------------------------
   --  Callbacks from the engine
   ---------------------------------------------------------------------------

   procedure On_Invalidate (X, Y, W, H : C.int)
     with Export, Convention => C,
          External_Name => "cubit_browser_invalidate";
   procedure On_URL (Text : System.Address; Length : C.size_t)
     with Export, Convention => C,
          External_Name => "cubit_browser_url_changed";
   procedure On_Title (Text : System.Address; Length : C.size_t)
     with Export, Convention => C,
          External_Name => "cubit_browser_title_changed";
   procedure On_Status (Text : System.Address; Length : C.size_t)
     with Export, Convention => C,
          External_Name => "cubit_browser_status_changed";
   procedure On_Extent (Width, Height : C.int)
     with Export, Convention => C,
          External_Name => "cubit_browser_extent_changed";
   procedure On_Scroll (X, Y : C.int)
     with Export, Convention => C,
          External_Name => "cubit_browser_scroll_changed";
   procedure On_Busy (Busy_Now : C.int)
     with Export, Convention => C,
          External_Name => "cubit_browser_busy_changed";
   procedure On_Pointer (Shape : C.int)
     with Export, Convention => C,
          External_Name => "cubit_browser_pointer_changed";
end Browser_Engine;
