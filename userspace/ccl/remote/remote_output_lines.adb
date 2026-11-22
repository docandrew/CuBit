pragma Ada_2022;

package body Remote_Output_Lines with SPARK_Mode is

   LF : constant Character := ASCII.LF;

   procedure Reset (A : out Assembler) is
   begin
      A := (Data => [others => ' '], Length => 0);
   end Reset;

   procedure Feed (A : in out Assembler; Chunk : String) is
   begin
      for C of Chunk loop
         if C = LF then
            Emit (A.Data (1 .. A.Length));
            A.Length := 0;
         else
            if A.Length = Maximum_Line_Bytes then
               Emit (A.Data);
               A.Length := 0;
            end if;
            A.Length := A.Length + 1;
            A.Data (A.Length) := C;
         end if;
      end loop;
   end Feed;

   procedure Finish (A : in out Assembler) is
   begin
      if A.Length > 0 then
         Emit (A.Data (1 .. A.Length));
         A.Length := 0;
      end if;
   end Finish;

end Remote_Output_Lines;
