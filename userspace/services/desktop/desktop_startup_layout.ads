with Interfaces;
-- Startup layout admission for the current BGRA image/readback backend.
-- Numeric validation grants no authority over a mapping or GPU allocation.
package Desktop_Startup_Layout with SPARK_Mode, Pure is
   subtype Wide is Interfaces.Unsigned_64;
   use type Wide;
   Maximum_Edge : constant Wide := 4096;
   Maximum_Bytes : constant Wide := 16 * 1024 * 1024;
   function Supported (Width, Height : Wide) return Boolean is
     (Width in 1 .. Maximum_Edge and then Height in 1 .. Maximum_Edge
      and then Width * Height * 4 <= Maximum_Bytes);
   -- Zero is rejection. Multiplication only follows bounded-edge validation.
   function Required_Bytes (Width, Height : Wide) return Natural
     with Post =>
       (if Supported (Width, Height) then
          Required_Bytes'Result in 1 .. Natural (Maximum_Bytes) and
          Wide (Required_Bytes'Result) = Width * Height * 4
        else Required_Bytes'Result = 0);
end Desktop_Startup_Layout;
