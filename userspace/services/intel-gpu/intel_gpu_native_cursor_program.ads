with System;
with Intel_GPU_Cursor_Program;
--  Native writer for one pipe's cursor plane (one instance per pipe). Register_Page is the virtual
--  address of a writable mapping of that pipe's cursor register page
--  (Intel_GPU_Cursor_Program.Page), granted by the broker like the display
--  power pages. Power_Held is the pipe's retained power reference. Owner is
--  trusted retained display ownership, never an IPC argument.
generic
   Register_Page : System.Address;
   with function Power_Held return Boolean;
package Intel_GPU_Native_Cursor_Program is
   type Outcome is (Rejected, Power_Unavailable, Written);
   --  Full update (shape, size, base, position) or disable.
   function Program
     (Owner : Boolean; Values : Intel_GPU_Cursor_Program.Register_Values)
      return Outcome;
   --  Position-only update of an enabled cursor.
   function Move
     (Owner : Boolean; Values : Intel_GPU_Cursor_Program.Register_Values)
      return Outcome;
   function Writes return Natural;
end Intel_GPU_Native_Cursor_Program;
