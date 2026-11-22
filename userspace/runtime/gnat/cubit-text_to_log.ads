pragma Ada_2022;
with CuBit.Log_Records;
with CuBit.Protocols;

--  Bounded incremental LF/CRLF framing. No implicit publication authority.
package CuBit.Text_To_Log with Pure, SPARK_Mode is
   package Logs renames CuBit.Log_Records;
   Maximum_Chunk_Bytes : constant := 512;
   --  Byte chunks of one UTF-8 text stream, NOT independently valid strings.
   --  A scalar or CRLF may span chunks. Validate completed lines here.
   Input_Contract : constant CuBit.Protocols.Schema_Contract :=
     (Identity => 16#4355_4254_4348_0001#, Version => 1,
      Sizing => CuBit.Protocols.Bounded_Size,
      Wire_Size => Maximum_Chunk_Bytes);
   type Adapter (Level : Logs.Severity := Logs.Information) is limited private;
   type Step_Kind is (Need_More, Record_Ready, Line_Dropped);
   type Drop_Reason is
     (Oversized_Line, Invalid_UTF8_Or_Control, Upstream_Gap);
   --  A definite result container: callers cannot constrain the internal
   --  variant and make a different emission fail its discriminant check.
   type Step is private;
   function Kind (Item : Step) return Step_Kind;
   function Value (Item : Step) return Logs.Log_Record
     with Pre => Kind (Item) = Record_Ready;
   function Reason (Item : Step) return Drop_Reason
     with Pre => Kind (Item) = Line_Dropped;
   --  Pure in-process byte feeding, not an IPC call per byte. The caller feeds
   --  admitted chunks and must handle each output or explicitly report loss.
   procedure Feed
     (Item : in out Adapter; Byte : Character; At_Time : Logs.Timestamp;
      Output : out Step);
   --  EOF emits a final unterminated nonempty line. An incomplete UTF-8 scalar
   --  or bare CR is a reported invalid line. No empty phantom EOF record.
   procedure Finish
     (Item : in out Adapter; At_Time : Logs.Timestamp; Output : out Step);
   --  Report a discontinuity and discard until the next LF (or EOF). This
   --  May lose a complete line conservatively, but never splices fragments.
   procedure Report_Gap (Item : out Adapter; Output : out Step);
private
   type Step_Data (Kind : Step_Kind := Need_More) is record
      case Kind is
         when Need_More => null;
         when Record_Ready => Value : Logs.Log_Record;
         when Line_Dropped => Reason : Drop_Reason;
      end case;
   end record;
   type Step is record
      Data : Step_Data;
   end record;
   type Adapter (Level : Logs.Severity := Logs.Information) is limited record
      Data : String (1 .. Logs.Maximum_Text_Bytes) :=
        [others => Character'Val (0)];
      Length : Logs.Text_Count := 0;
      Pending_CR : Boolean := False;
      Oversized : Boolean := False;
      Resynchronizing : Boolean := False;
   end record;
end CuBit.Text_To_Log;
