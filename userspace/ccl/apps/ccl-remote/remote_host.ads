------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The remote session's CCL environment: the shared interpreter over the
--  read-only interfaces this process's manifest was granted (logs.recent,
--  logs.minimum, proc.list, config.get/keys, files.home/enter/up/list,
--  images, clock). It is the Diagnose principal: nothing here writes.
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
   --  Source's result as the console shows it ("Type: value"), or the
   --  diagnostic; Text (1 .. Length) holds as much as fits.
   procedure Evaluate
     (Source : String; Succeeded : out Boolean; Text : out String; Length : out Natural)
     with Pre => Text'First = 1;
end Remote_Host;
