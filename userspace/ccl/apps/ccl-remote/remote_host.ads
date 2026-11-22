------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The remote session's CCL environment: the shared interpreter over the
--  read-only interfaces this process's manifest was granted (logs.recent,
--  logs.between, logs.minimum, proc.list, config.get/keys, files.home/enter/up/list,
--  images, clock; display.capture with the display-capture grant). It is
--  the Diagnose principal: nothing here writes.
--
--  @description
--  One persistent session for the paired host, so its definitions and kept
--  values last across requests, as in a console. Evaluation is bounded by
--  fuel; a host call blocks on its service (no peer calls back: IPC DAG).
--  logs.set-minimum is published with the logs interface but refused here
--  even when log-control is present: changing what logstore keeps is not
--  diagnosis.
------------------------------------------------------------------------------
pragma Ada_2022;

package Remote_Host is
   procedure Initialize (Success : out Boolean);
   --  Evaluate Source: its result as the console shows it ("Type: value"),
   --  or the diagnostic, is kept whole (on the heap, as long as it is)
   --  until the next evaluation, and read in parts (Remote_Wire's
   --  Read_Result).
   procedure Evaluate (Source : String; Succeeded : out Boolean);
   --  The kept result's length, and up to Count of its characters from
   --  Offset (0-based; "" at or past its end).
   function Result_Length return Natural;
   function Result_Part (Offset, Count : Natural) return String;
end Remote_Host;
