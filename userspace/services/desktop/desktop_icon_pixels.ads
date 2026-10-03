with Interfaces;
with Desktop_Icons;
with Desktop_Window_Icons;
with Compositor_Upload;
-- Immutable embedded icons retain straight alpha. Copy only on initial upload;
-- never preblend against a background or create a second premultiplied atlas.
package Desktop_Icon_Pixels with SPARK_Mode is
   package U renames Compositor_Upload;
   type Family is (Application, Window_Control);
   type Asset (Kind : Family := Application) is record
      case Kind is
         when Application => Icon : Desktop_Icons.Icon_ID := Desktop_Icons.Start;
         when Window_Control => Control : Desktop_Window_Icons.Icon_ID := Desktop_Window_Icons.Close;
      end case;
   end record;
   function Size (Item : Asset) return Positive is
     (if Item.Kind = Application then Desktop_Icons.ICON_SIZE else Desktop_Window_Icons.ICON_SIZE);
   function Pixel (Item : Asset; X, Y : Natural) return Interfaces.Unsigned_32
     with Pre => X < Size (Item) and Y < Size (Item);
   type Pixels is array (Natural range <>) of Interfaces.Unsigned_32;
   -- Target is the actual staging mapping, not an intermediate CPU allocation.
   -- Validated plans may include offsets, clipped rows and destination padding.
   procedure Copy_Chunk (Item : Asset; Target : in out Pixels;
      Plan : U.Plan; Complete : out Boolean)
     with Pre => Target'First = 0 and Target'Last < U.Byte_Count'Last / 4,
       Post => (if not Complete then Target = Target'Old);
end Desktop_Icon_Pixels;
