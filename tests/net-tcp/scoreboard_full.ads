pragma SPARK_Mode (On);
with TCP_Scoreboard;
package Scoreboard_Full is new TCP_Scoreboard (32, 2 ** 30);
