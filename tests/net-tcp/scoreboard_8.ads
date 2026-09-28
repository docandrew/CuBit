pragma SPARK_Mode (On);
with TCP_Scoreboard;
--  A small window keeps the executable contracts (which quantify over
--  every offset) fast; Scoreboard_Full is proved at the real size.
package Scoreboard_8 is new TCP_Scoreboard (8, 2 ** 16);
