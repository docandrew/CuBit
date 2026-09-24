pragma Ada_2022;
with Boot_Font;
with Boot_Panel;
with Boot_Output;
with Interfaces; use Interfaces;
with Spinlocks;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Virtmem;

--  Trusted hardware/synchronization boundary. Pure content, glyph addressing
--  and framebuffer arithmetic are checked separately; this is not a proof of
--  concurrent memory, device visibility or firmware truth.
package body Boot_Diagnostics with SPARK_Mode => Off is
   use type Boot_Panel.Phase;
   use type Boot_Panel.Row;
   Lock : Spinlocks.Spinlock;
   Enabled : Boolean := False with Atomic;
   Closed : Boolean := False with Atomic;
   Model : Boot_Panel.State;
   Layout : Boot_Framebuffer.Description;
   Base : System.Address := System.Null_Address;
   Scale : Positive range 1 .. 2 := 1;
   Background : constant Unsigned_32 := 16#001B2028#;
   Foreground : constant Unsigned_32 := 16#00E0E6ED#;
   Accent : constant Unsigned_32 := 16#008DCED9#;
   Error_Color : constant Unsigned_32 := 16#00FF9D8F#;

   function Try_Enter return Boolean is
      Acquired : Boolean;
   begin
      Spinlocks.tryEnterCriticalSection (Lock, Acquired);
      return Acquired;
   end Try_Enter;

   procedure Pixel (X, Y : Natural; Color : Unsigned_32) is
   begin
      --  The panel clips on small firmware modes, rather than imposing a new
      --  minimum boot resolution or writing beyond the admitted mapping.
      if X < Layout.Width and then Y < Layout.Height then
         declare
            Value : Unsigned_32 with Import, Volatile,
              Address => Base + Storage_Offset (Boot_Framebuffer.Pixel_Offset (Layout, X, Y));
         begin
            Value := Color;
         end;
      end if;
   end Pixel;

   procedure Text (Value : String; Line : Natural; Color : Unsigned_32) is
      Cell : Natural := 0;
      Left : Natural;
      Top : constant Natural := 16 + Line * (Boot_Font.Height + 3) * Scale;
   begin
      for C of Value loop
         exit when Cell = Boot_Panel.Columns;
         Left := 16 + Cell * (Boot_Font.Width + 1) * Scale;
         exit when Left >= Layout.Width;
         for Y in Boot_Font.Row loop
            for X in 0 .. Boot_Font.Width loop
               for DY in 0 .. Scale - 1 loop
                  for DX in 0 .. Scale - 1 loop
                     Pixel (Left + X * Scale + DX, Top + Y * Scale + DY,
                       (if X < Boot_Font.Width and then Boot_Font.Pixel (C, X, Y)
                        then Color else Background));
                  end loop;
               end loop;
            end loop;
         end loop;
         Cell := Cell + 1;
      end loop;
   end Text;

   procedure Paint (R : Boot_Panel.Row) is
   begin
      Text (Boot_Panel.Content (Model, R), Boot_Panel.Row'Pos (R) * 2 + 1,
            (if R = Boot_Panel.First_Error then Error_Color else Foreground));
      --  Fence on the WRITING CPU before unlocking, including write-combined
      --  mappings; a retiring CPU cannot drain another CPU's WC buffer.
      System.Machine_Code.Asm ("sfence", Clobber => "memory", Volatile => True);
   end Paint;

   procedure Setup (Item : Boot_Framebuffer.Description) is
   begin
      if Closed or Boot_Output.Is_Retired then return; end if;
      Spinlocks.enterCriticalSection (Lock);
      if not Closed and then Boot_Panel.Lifecycle (Model) = Boot_Panel.Unavailable then
         Layout := Item;
         Base := Virtmem.P2Va (Integer_Address (Item.Base));
         Scale := (if Item.Width >= 1024 and Item.Height >= 600 then 2 else 1);
         Boot_Panel.Initialize (Model);
         --  Clear only the fixed panel once. Subsequent writes touch one text
         --  row, never move old pixels or repaint a full-screen backbuffer.
         for Y in 0 .. Natural'Min (Item.Height, 16 + 11 * 16 * Scale) - 1 loop
            for X in 0 .. Natural'Min (Item.Width, 32 + Boot_Panel.Columns * 9 * Scale) - 1 loop
               Pixel (X, Y, Background);
            end loop;
         end loop;
         Text ("BOOT", 0, Accent);
         Text ("CURRENT STEP", 2, Accent);
         Text ("LAST COMPLETED", 4, Accent);
         Text ("LATEST DIAGNOSTIC (best effort)", 6, Accent);
         Text ("FIRST FAILURE", 8, Accent);
         for R in Boot_Panel.Row loop Paint (R); end loop;
         Enabled := True;
         Boot_Output.Install (Append'Access, Panic'Access, Retire'Access);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Setup;

   procedure Begin_Step (Text : String) is
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Begin_Step (Model, Text);
         Paint (Boot_Panel.Current_Step);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Begin_Step;
   procedure Complete_Step (Text : String) is
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Complete_Step (Model, Text);
         Paint (Boot_Panel.Last_Completed);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Complete_Step;
   procedure Append (C : Character) is
      Changed : Boolean;
   begin
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         Boot_Panel.Append (Model, C, Changed);
         if Changed then Paint (Boot_Panel.Latest_Detail); end if;
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Append;
   procedure Panic (Message : System.Address) is
      Value : Boot_Panel.Line := [others => ' '];
   begin
      --  Never wait for another CPU or re-enter a renderer which faulted. A
      --  failed try leaves serial reporting available to the last-chance path.
      if not Enabled or else not Try_Enter then return; end if;
      if not Closed then
         for I in Value'Range loop
            declare
               C : Character with Import, Address => Message + Storage_Offset (I - 1);
            begin
               exit when C = ASCII.NUL;
               Value (I) := C;
            end;
         end loop;
         Boot_Panel.Fail (Model, Value);
         Paint (Boot_Panel.Current_Step);
         Paint (Boot_Panel.First_Error);
      end if;
      Spinlocks.exitCriticalSection (Lock);
   end Panic;
   procedure Retire is
   begin
      --  Close admission before waiting. Writers hold this same IRQ-masking
      --  lock until their final device store, and perform no IPC/allocation.
      --  Their second Closed check handles admission racing this store.
      Closed := True;
      Enabled := False;
      Spinlocks.enterCriticalSection (Lock);
      Enabled := False;
      Boot_Panel.Retire (Model);
      --  Drain WC stores before authorizing another owner to touch hardware.
      --  This is a CPU store fence, not evidence of native GPU DMA completion.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      Base := System.Null_Address;
      Spinlocks.exitCriticalSection (Lock);
   end Retire;
end Boot_Diagnostics;
