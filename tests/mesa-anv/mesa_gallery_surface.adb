with Mesa_Triangle_Surface;
with Client_Frame_Pair;
with Client_Frame_Damage;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.Messages;
with System.Storage_Elements; use System.Storage_Elements;
package body Mesa_Gallery_Surface is
   package F renames Client_Frame_Pair;
   package D renames CuBit.Desktop_Protocol;
   package M renames CuBit.Messages;
   use type System.Address;
   use type D.Status_Code, D.Input_Event_Kind, D.Pixel_Extent;
   Owner : F.Owner;
   Surface : Unsigned_64 := 0;
   Serial : Unsigned_64 := 0;
   Closing : Boolean := False;
   procedure Rate (Milli_FPS : Unsigned_64) is
      Text : String := "20 pots 0000.0 FPS";
      Whole : Natural := Natural (Unsigned_64'Min (9999, Milli_FPS / 1000));
      Request : M.Message;
   begin
      if Surface = 0 or Closing then return; end if;
      for I in reverse 9 .. 12 loop
         Text (I) := Character'Val (Character'Pos ('0') + Whole mod 10);
         Whole := Whole / 10;
      end loop;
      for I in 9 .. 11 loop
         exit when Text (I) /= '0';
         Text (I) := ' ';
      end loop;
      Text (14) := Character'Val (Character'Pos ('0') + Natural ((Milli_FPS / 100) mod 10));
      Request := CuBit.Desktop_Messages.From_Wire
        (D.Encode_Title ((D.Live_Surface_Name (Surface), D.Make_Title
          ((if Milli_FPS = Unsigned_64'Last then "20 pots clock N/A" else Text)))));
      Request.tag := M.capCall (M.CapabilitySlot (Mesa_Triangle_Surface.Desktop_Slot), Request);
   end Rate;
   function Frame (Source : System.Address; Width, Height, Pitch : Unsigned_32)
     return Unsigned_32 is
      OK : Boolean;
      Repair : Client_Frame_Damage.Box;
      Config : F.Pub.Configuration_Result;
      function Copy (Dst, Src : System.Address; Bytes : Unsigned_64) return System.Address
        with Import, Convention => C, External_Name => "memcpy";
      Ignored : System.Address;
      Request : M.Message;
      Event : D.Input_Result;
   begin
      if Closing or Source = System.Null_Address or Width /= 800 or Height /= 600 or Pitch /= 3200 then return 10; end if;
      if Surface = 0 then
         Surface := Mesa_Triangle_Surface.Create (Width, Height);
         if Surface = 0 then return 11; end if;
      end if;
      -- Bounded input drain; window controls remain responsive under load.
      for I in 1 .. 32 loop
         Request := CuBit.Desktop_Messages.From_Wire
           (D.Encode_Input_Request ((D.Poll_Input, D.Live_Surface_Name (Surface), Serial)));
         Request.tag := M.capCall (M.CapabilitySlot (Mesa_Triangle_Surface.Desktop_Slot), Request);
         Event := D.Decode_Input_Result (CuBit.Desktop_Messages.To_Wire (Request), D.Poll_Input);
         if Event.Status /= D.Success then return 12; end if;
         if Event.Value.Serial > Serial then Serial := Event.Value.Serial; end if;
         if Event.Value.Kind = D.Close_Requested then return 2; end if;
         exit when Event.Value.Kind = D.No_Input or else not Event.Value.More_Pending;
      end loop;
      F.Configure (Owner, D.Live_Surface_Name (Surface), OK);
      if not OK then return 13; end if;
      Config := F.Configuration (Owner);
      F.Begin_Paint (Owner, (0, 0, Natural (Config.Value.Width), Natural (Config.Value.Height)), Repair, OK);
      if not OK then return 4; end if; -- Prior immutable frame may still be held.
      if F.Address (Owner) = System.Null_Address then return 14; end if;
      if Config.Value.Layout.Width = 800 and Config.Value.Layout.Height = 600 and
        Config.Value.Layout.Pitch = 3200 then
         Ignored := Copy (F.Address (Owner), Source, 800 * 600 * 4);
      else
         -- CPU presentation baseline only: adapt to decorations, resize and
         -- monitor density. GPU render target remains800x600 for comparability.
         declare
            type Pixels is array (Natural range <>) of Unsigned_32 with Component_Size => 32;
            Input : Pixels (0 .. 800 * 600 - 1) with Import, Address => Source;
            W : constant Natural := Natural (Config.Value.Layout.Width);
            H : constant Natural := Natural (Config.Value.Layout.Height);
            Stride : constant Natural := Config.Value.Layout.Pitch / 4;
            Output : Pixels (0 .. Stride * H - 1) with Import, Address => F.Address (Owner);
         begin
            for Y in 0 .. H - 1 loop
               for X in 0 .. W - 1 loop
                  Output (Y * Stride + X) := Input ((Y * 600 / H) * 800 + X * 800 / W);
               end loop;
               for X in W .. Stride - 1 loop Output (Y * Stride + X) := 0; end loop;
            end loop;
         end;
      end if;
      F.Publish (Owner, (0, 0, Natural (Config.Value.Width), Natural (Config.Value.Height)), OK, Serial);
      return (if OK then 0 else 15);
   end Frame;
   function Close return Unsigned_32 is
      OK : Boolean;
      Destroyed : Unsigned_32;
   begin
      if not Closing then
         Closing := True;
         if Surface /= 0 then
            Destroyed := Mesa_Triangle_Surface.Destroy (Surface);
            -- Do not replay ambiguous destruction; tracked grants decide
            -- whether backing can be released on this and later calls.
         end if;
      end if;
      F.Close (Owner, OK);
      return (if OK then 0 else 1);
   end Close;
end Mesa_Gallery_Surface;
