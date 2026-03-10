------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  PS/2 Mouse client API implementation
------------------------------------------------------------------------------
with CuBit.Messages; use CuBit.Messages;

package body CuBit.Mouse is

   --  Internal ring buffer for mouse events
   BUFFER_SIZE : constant := 64;
   type BufferIndex is mod BUFFER_SIZE;

   eventBuf    : array (BufferIndex) of MouseEvent;
   bufHead     : BufferIndex := 0;
   bufTail     : BufferIndex := 0;
   bufCount    : Natural := 0;
   initialized : Boolean := False;

   procedure bufPush (ev : MouseEvent);
   function bufPop (ev : out MouseEvent) return Boolean;

   procedure bufPush (ev : MouseEvent) is
   begin
      if bufCount < BUFFER_SIZE then
         eventBuf (bufHead) := ev;
         bufHead := bufHead + 1;
         bufCount := bufCount + 1;
      end if;
   end bufPush;

   function bufPop (ev : out MouseEvent) return Boolean is
   begin
      if bufCount = 0 then
         return False;
      end if;
      ev := eventBuf (bufTail);
      bufTail := bufTail + 1;
      bufCount := bufCount - 1;
      return True;
   end bufPop;

   ---------------------------------------------------------------------------
   --  init
   ---------------------------------------------------------------------------
   procedure init is
      ignore : Unsigned_64;
   begin
      if not initialized then
         ignore := registerDriver (DRIVER_MOUSE);
         initialized := True;
      end if;
   end init;

   ---------------------------------------------------------------------------
   --  poll
   ---------------------------------------------------------------------------
   procedure poll is
      msg   : Message;
      found : Boolean;
      ev    : MouseEvent;
      packed : Unsigned_64;
      dxRaw  : Unsigned_16;
      dyRaw  : Unsigned_16;
      dzRaw  : Unsigned_8;
   begin
      loop
         found := receiveEventNB (msg);
         exit when not found;

         --  Mouse events have label=2 (keyboard events have label=1)
         if msg.tag.label /= 2 then
            --  Not a mouse event, skip
            null;
         else
            packed := msg.words (0);

            ev.buttons := Unsigned_8 (packed and 16#FF#);

            --  Sign-extend 12-bit dx
            dxRaw := Unsigned_16 ((Shift_Right (packed, 8)) and 16#FFF#);
            if (dxRaw and 16#800#) /= 0 then
               dxRaw := dxRaw or 16#F000#;
            end if;
            ev.dx := Integer_16 (dxRaw);

            --  Sign-extend 12-bit dy
            dyRaw := Unsigned_16 ((Shift_Right (packed, 20)) and 16#FFF#);
            if (dyRaw and 16#800#) /= 0 then
               dyRaw := dyRaw or 16#F000#;
            end if;
            ev.dy := Integer_16 (dyRaw);

            --  Sign-extend 8-bit dz
            dzRaw := Unsigned_8 ((Shift_Right (packed, 32)) and 16#FF#);
            ev.dz := Integer_8 (dzRaw);

            ev.hasWheel := ((Shift_Right (packed, 40)) and 1) /= 0;

            bufPush (ev);
         end if;
      end loop;
   end poll;

   ---------------------------------------------------------------------------
   --  get
   ---------------------------------------------------------------------------
   function get (ev : out MouseEvent) return Boolean is
   begin
      poll;
      return bufPop (ev);
   end get;

end CuBit.Mouse;
