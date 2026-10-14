with Interfaces; use Interfaces;
--  Page-granularity authority over one pipe's register page 0x70000 +
--  0x1000 * pipe: PIPEDSL, PIPE_FRMCOUNT, the cursor registers (CURCNTR,
--  CURBASE, CURPOS, CURSURFLIVE, CUR_WM, CUR_BUF_CFG) and the primary
--  plane's registers (PLANE_CTL .. PLANE_SURFLIVE at 0x70180..). Not
--  per-register isolation; a grant is not a power reference. Requested only
--  for the pipe that scans out the firmware framebuffer (DISP-003).
package Intel_GPU_Pipe_Pages with SPARK_Mode, Pure is
   Request_Label : constant := 16#023A#;
   Virtual_Base : constant Unsigned_64 := 16#6160_0000#;
   Page_Bytes : constant := 4_096;
   Pipe_A_Offset : constant := 16#70000#;
   Pipe_Stride : constant := 16#1000#;
   --  Pipe A .. D (Intel_GPU_Display_Topology.Pipe'Pos).
   subtype Page_Index is Natural range 0 .. 3;
   function Offset (Item : Page_Index) return Unsigned_64 is
     (Pipe_A_Offset + Pipe_Stride * Unsigned_64 (Item))
     with Post => Offset'Result mod Page_Bytes = 0 and Offset'Result < 16#200000#;
   function Address (Item : Page_Index) return Unsigned_64 is
     (Virtual_Base + Page_Bytes * Unsigned_64 (Item));
end Intel_GPU_Pipe_Pages;
