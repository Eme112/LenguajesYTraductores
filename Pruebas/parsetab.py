
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'PROGRAMAAND ASIGNACION BEGIN DIFERENTE DO DOS_PUNTOS ELSE ELSIF END END_IF END_LOOP ENTRE FLOAT FOR FUNCTION ID IF IGUAL INT MAIN MAS MAYOR MAYOR_IGUAL MENOR MENOR_IGUAL MENOS MODULO NOT OR PARENTESIS_DERECHO PARENTESIS_IZQUIERDO POR PROCEDURE PUNTO_COMA RETURN VALOR_FLOAT VALOR_INT WHILE\n  empty :\n  C : MENOR\n         | MAYOR\n         | MENOR_IGUAL\n         | MAYOR_IGUAL\n         | IGUAL\n         | DIFERENTEE : E2\n         | E2 C E2E2 : E3\n          | E2 OR E3\n          | E2 MAS E3\n          | E2 MENOS E3E3 : E4\n          | E3 AND E4\n          | E3 POR E4\n          | E3 ENTRE E4\n          | E3 MODULO E4E4 : T\n          | NOT TT : PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO\n         | FUNCTION\n         | ID_COMPLETO\n         | VALOR_INT\n         | VALOR_FLOATID_COMPLETO : IDV : TIPO DOS_PUNTOS A PUNTO_COMA\n         | TIPO DOS_PUNTOS ID PUNTO_COMATIPO : INT\n            | FLOATV_M : empty\n           | V V_MA : ID ASIGNACION VALOR_INT\n         | ID ASIGNACION VALOR_FLOAT\n         | ID ASIGNACION E\n         | ID ASIGNACION ID_COMPLETO PARENTESIS_IZQUIERDO PARENTESIS_DERECHOEST : empty\n           | LOOP END_LOOP PUNTO_COMA EST\n           | IF_ END_IF PUNTO_COMA EST\n           | A PUNTO_COMA EST\n           | PROCEDURE PUNTO_COMA EST\n           | FUNCTION PUNTO_COMA EST\n           | ID PARENTESIS_IZQUIERDO  PARENTESIS_DERECHO PUNTO_COMA ESTLOOP : DO_WHILE\n            | WHILE_\n            | FOR_DO_WHILE : DO DOS_PUNTOS EST WHILE E PUNTO_COMAWHILE_ : WHILE E DOS_PUNTOS ESTFOR_ : FOR EST PUNTO_COMA E PUNTO_COMA EST DOS_PUNTOS ESTIF_ : IF E DOS_PUNTOS EST ELSIF_ELSIF_ : END IF PUNTO_COMA\n              | ELSE DOS_PUNTOS EST END IF PUNTO_COMA\n              | ELSIF DOS_PUNTOS E EST ELSIF_P : PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE\n         | PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMAF : TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMAMP : PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA\n         | PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMAPROGRAMA : MP PROGRAMA_HPROGRAMA_H : empty\n                  | P PROGRAMA_H\n                  | F PROGRAMA_H '
    
_lr_action_items = {'PROCEDURE':([0,2,6,7,37,40,41,56,62,63,67,68,71,72,73,74,77,78,79,80,81,82,89,95,96,100,115,118,121,124,125,128,130,131,132,133,134,135,136,137,138,143,152,155,157,161,165,166,167,],[3,8,8,8,42,42,42,42,42,93,42,42,-8,-10,-14,-19,-22,-23,-24,-25,-26,42,121,42,42,42,-20,42,-54,-57,145,42,-9,-11,-12,-13,-15,-16,-17,-18,-21,153,42,-58,42,-55,42,42,-56,]),'$end':([1,2,4,5,6,7,13,14,121,124,155,161,167,],[0,-1,-59,-60,-1,-1,-61,-62,-54,-57,-58,-55,-56,]),'INT':([2,6,7,23,24,25,28,57,58,121,124,155,161,167,],[10,10,10,10,10,10,10,-27,-28,-54,-57,-58,-55,-56,]),'FLOAT':([2,6,7,23,24,25,28,57,58,121,124,155,161,167,],[11,11,11,11,11,11,11,-27,-28,-54,-57,-58,-55,-56,]),'MAIN':([3,],[12,]),'ID':([8,9,10,11,34,37,40,41,53,55,56,59,62,67,68,71,72,73,74,75,76,77,78,79,80,81,82,91,95,96,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,118,119,128,130,131,132,133,134,135,136,137,138,139,152,157,158,165,166,],[15,16,-29,-30,39,49,49,49,81,81,49,81,49,49,49,-8,-10,-14,-19,81,81,-22,-23,-24,-25,-26,49,123,49,49,49,81,81,81,81,-2,-3,-4,-5,-6,-7,81,81,81,81,-20,49,81,49,-9,-11,-12,-13,-15,-16,-17,-18,-21,81,49,49,81,49,49,]),'DOS_PUNTOS':([10,11,20,21,22,29,44,54,62,67,68,70,71,72,73,74,77,78,79,80,81,83,92,95,96,97,98,115,126,127,128,130,131,132,133,134,135,136,137,138,146,149,150,152,160,],[-29,-30,23,24,25,34,-37,82,-1,-1,-1,100,-8,-10,-14,-19,-22,-23,-24,-25,-26,118,-41,-1,-1,-40,-42,-20,-38,-39,-1,-9,-11,-12,-13,-15,-16,-17,-18,-21,-43,157,158,-1,166,]),'PARENTESIS_IZQUIERDO':([12,15,16,49,53,55,59,75,76,81,88,101,102,103,104,105,106,107,108,109,110,111,112,113,114,119,139,158,],[17,18,19,69,76,76,76,76,76,-26,120,76,76,76,76,-2,-3,-4,-5,-6,-7,76,76,76,76,76,76,76,]),'PARENTESIS_DERECHO':([17,18,19,69,71,72,73,74,77,78,79,80,81,115,116,120,130,131,132,133,134,135,136,137,138,],[20,21,22,99,-8,-10,-14,-19,-22,-23,-24,-25,-26,-20,138,142,-9,-11,-12,-13,-15,-16,-17,-18,-21,]),'BEGIN':([23,24,25,26,27,28,30,31,33,57,58,],[-1,-1,-1,32,-31,-1,35,36,-32,-27,-28,]),'PUNTO_COMA':([32,35,36,38,39,42,44,47,48,56,62,64,65,66,67,68,71,72,73,74,77,78,79,80,81,84,85,86,87,88,90,92,93,95,96,97,98,99,115,123,126,127,128,130,131,132,133,134,135,136,137,138,141,142,145,146,151,153,156,162,171,],[37,40,41,57,58,62,-37,67,68,-1,-1,94,95,96,-1,-1,-8,-10,-14,-19,-22,-23,-24,-25,-26,119,-24,-25,-35,-23,122,-41,124,-1,-1,-40,-42,128,-20,144,-38,-39,-1,-9,-11,-12,-13,-15,-16,-17,-18,-21,152,-36,155,-43,159,161,163,167,173,]),'FUNCTION':([37,40,41,53,55,56,59,62,67,68,71,72,73,74,75,76,77,78,79,80,81,82,95,96,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,118,119,128,130,131,132,133,134,135,136,137,138,139,152,154,157,158,165,166,],[48,48,48,77,77,48,77,48,48,48,-8,-10,-14,-19,77,77,-22,-23,-24,-25,-26,48,48,48,48,77,77,77,77,-2,-3,-4,-5,-6,-7,77,77,77,77,-20,48,77,48,-9,-11,-12,-13,-15,-16,-17,-18,-21,77,48,162,48,77,48,48,]),'END':([37,40,43,44,60,62,67,68,71,72,73,74,77,78,79,80,81,92,94,95,96,97,98,100,115,122,126,127,128,129,130,131,132,133,134,135,136,137,138,144,146,157,164,165,169,],[-1,-1,63,-37,89,-1,-1,-1,-8,-10,-14,-19,-22,-23,-24,-25,-26,-41,125,-1,-1,-40,-42,-1,-20,143,-38,-39,-1,148,-9,-11,-12,-13,-15,-16,-17,-18,-21,154,-43,-1,168,-1,148,]),'RETURN':([37,40,41,43,44,60,61,62,67,68,92,95,96,97,98,126,127,128,146,],[-1,-1,-1,64,-37,90,91,-1,-1,-1,-41,-1,-1,-40,-42,-38,-39,-1,-43,]),'IF':([37,40,41,56,62,67,68,71,72,73,74,77,78,79,80,81,82,95,96,100,115,118,128,130,131,132,133,134,135,136,137,138,148,152,157,165,166,168,],[53,53,53,53,53,53,53,-8,-10,-14,-19,-22,-23,-24,-25,-26,53,53,53,53,-20,53,53,-9,-11,-12,-13,-15,-16,-17,-18,-21,156,53,53,53,53,171,]),'DO':([37,40,41,56,62,67,68,71,72,73,74,77,78,79,80,81,82,95,96,100,115,118,128,130,131,132,133,134,135,136,137,138,152,157,165,166,],[54,54,54,54,54,54,54,-8,-10,-14,-19,-22,-23,-24,-25,-26,54,54,54,54,-20,54,54,-9,-11,-12,-13,-15,-16,-17,-18,-21,54,54,54,54,]),'WHILE':([37,40,41,44,56,62,67,68,71,72,73,74,77,78,79,80,81,82,92,95,96,97,98,100,115,117,118,126,127,128,130,131,132,133,134,135,136,137,138,146,152,157,165,166,],[55,55,55,-37,55,55,55,55,-8,-10,-14,-19,-22,-23,-24,-25,-26,55,-41,55,55,-40,-42,55,-20,139,55,-38,-39,55,-9,-11,-12,-13,-15,-16,-17,-18,-21,-43,55,55,55,55,]),'FOR':([37,40,41,56,62,67,68,71,72,73,74,77,78,79,80,81,82,95,96,100,115,118,128,130,131,132,133,134,135,136,137,138,152,157,165,166,],[56,56,56,56,56,56,56,-8,-10,-14,-19,-22,-23,-24,-25,-26,56,56,56,56,-20,56,56,-9,-11,-12,-13,-15,-16,-17,-18,-21,56,56,56,56,]),'ASIGNACION':([39,49,],[59,59,]),'ELSE':([44,62,67,68,71,72,73,74,77,78,79,80,81,92,95,96,97,98,100,115,126,127,128,129,130,131,132,133,134,135,136,137,138,146,165,169,],[-37,-1,-1,-1,-8,-10,-14,-19,-22,-23,-24,-25,-26,-41,-1,-1,-40,-42,-1,-20,-38,-39,-1,149,-9,-11,-12,-13,-15,-16,-17,-18,-21,-43,-1,149,]),'ELSIF':([44,62,67,68,71,72,73,74,77,78,79,80,81,92,95,96,97,98,100,115,126,127,128,129,130,131,132,133,134,135,136,137,138,146,165,169,],[-37,-1,-1,-1,-8,-10,-14,-19,-22,-23,-24,-25,-26,-41,-1,-1,-40,-42,-1,-20,-38,-39,-1,150,-9,-11,-12,-13,-15,-16,-17,-18,-21,-43,-1,150,]),'END_LOOP':([44,45,50,51,52,62,67,68,92,95,96,97,98,118,126,127,128,140,146,159,166,170,],[-37,65,-44,-45,-46,-1,-1,-1,-41,-1,-1,-40,-42,-1,-38,-39,-1,-48,-43,-47,-1,-49,]),'END_IF':([46,147,163,172,173,],[66,-50,-51,-53,-52,]),'NOT':([53,55,59,76,101,102,103,104,105,106,107,108,109,110,111,112,113,114,119,139,158,],[75,75,75,75,75,75,75,75,-2,-3,-4,-5,-6,-7,75,75,75,75,75,75,75,]),'VALOR_INT':([53,55,59,75,76,101,102,103,104,105,106,107,108,109,110,111,112,113,114,119,139,158,],[79,79,85,79,79,79,79,79,79,-2,-3,-4,-5,-6,-7,79,79,79,79,79,79,79,]),'VALOR_FLOAT':([53,55,59,75,76,101,102,103,104,105,106,107,108,109,110,111,112,113,114,119,139,158,],[80,80,86,80,80,80,80,80,80,-2,-3,-4,-5,-6,-7,80,80,80,80,80,80,80,]),'OR':([71,72,73,74,77,78,79,80,81,85,86,88,115,130,131,132,133,134,135,136,137,138,],[102,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,102,-11,-12,-13,-15,-16,-17,-18,-21,]),'MAS':([71,72,73,74,77,78,79,80,81,85,86,88,115,130,131,132,133,134,135,136,137,138,],[103,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,103,-11,-12,-13,-15,-16,-17,-18,-21,]),'MENOS':([71,72,73,74,77,78,79,80,81,85,86,88,115,130,131,132,133,134,135,136,137,138,],[104,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,104,-11,-12,-13,-15,-16,-17,-18,-21,]),'MENOR':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[105,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'MAYOR':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[106,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'MENOR_IGUAL':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[107,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'MAYOR_IGUAL':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[108,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'IGUAL':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[109,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'DIFERENTE':([71,72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[110,-10,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,-11,-12,-13,-15,-16,-17,-18,-21,]),'AND':([72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[111,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,111,111,111,-15,-16,-17,-18,-21,]),'POR':([72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[112,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,112,112,112,-15,-16,-17,-18,-21,]),'ENTRE':([72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[113,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,113,113,113,-15,-16,-17,-18,-21,]),'MODULO':([72,73,74,77,78,79,80,81,85,86,88,115,131,132,133,134,135,136,137,138,],[114,-14,-19,-22,-23,-24,-25,-26,-24,-25,-23,-20,114,114,114,-15,-16,-17,-18,-21,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMA':([0,],[1,]),'MP':([0,],[2,]),'PROGRAMA_H':([2,6,7,],[4,13,14,]),'empty':([2,6,7,23,24,25,28,37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[5,5,5,27,27,27,27,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,]),'P':([2,6,7,],[6,6,6,]),'F':([2,6,7,],[7,7,7,]),'TIPO':([2,6,7,23,24,25,28,],[9,9,9,29,29,29,29,]),'V_M':([23,24,25,28,],[26,30,31,33,]),'V':([23,24,25,28,],[28,28,28,28,]),'A':([34,37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[38,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,]),'EST':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[43,60,61,84,92,97,98,117,126,127,129,140,146,160,164,169,170,]),'LOOP':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,]),'IF_':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,]),'DO_WHILE':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,]),'WHILE_':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,]),'FOR_':([37,40,41,56,62,67,68,82,95,96,100,118,128,152,157,165,166,],[52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,]),'E':([53,55,59,76,119,139,158,],[70,83,87,116,141,151,165,]),'E2':([53,55,59,76,101,119,139,158,],[71,71,71,71,130,71,71,71,]),'E3':([53,55,59,76,101,102,103,104,119,139,158,],[72,72,72,72,72,131,132,133,72,72,72,]),'E4':([53,55,59,76,101,102,103,104,111,112,113,114,119,139,158,],[73,73,73,73,73,73,73,73,134,135,136,137,73,73,73,]),'T':([53,55,59,75,76,101,102,103,104,111,112,113,114,119,139,158,],[74,74,74,115,74,74,74,74,74,74,74,74,74,74,74,74,]),'ID_COMPLETO':([53,55,59,75,76,101,102,103,104,111,112,113,114,119,139,158,],[78,78,88,78,78,78,78,78,78,78,78,78,78,78,78,78,]),'C':([71,],[101,]),'ELSIF_':([129,169,],[147,172,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMA","S'",1,None,None,None),
  ('empty -> <empty>','empty',0,'p_empty','pruebas.py',130),
  ('C -> MENOR','C',1,'p_C','pruebas.py',136),
  ('C -> MAYOR','C',1,'p_C','pruebas.py',137),
  ('C -> MENOR_IGUAL','C',1,'p_C','pruebas.py',138),
  ('C -> MAYOR_IGUAL','C',1,'p_C','pruebas.py',139),
  ('C -> IGUAL','C',1,'p_C','pruebas.py',140),
  ('C -> DIFERENTE','C',1,'p_C','pruebas.py',141),
  ('E -> E2','E',1,'p_E','pruebas.py',145),
  ('E -> E2 C E2','E',3,'p_E','pruebas.py',146),
  ('E2 -> E3','E2',1,'p_E2','pruebas.py',150),
  ('E2 -> E2 OR E3','E2',3,'p_E2','pruebas.py',151),
  ('E2 -> E2 MAS E3','E2',3,'p_E2','pruebas.py',152),
  ('E2 -> E2 MENOS E3','E2',3,'p_E2','pruebas.py',153),
  ('E3 -> E4','E3',1,'p_E3','pruebas.py',157),
  ('E3 -> E3 AND E4','E3',3,'p_E3','pruebas.py',158),
  ('E3 -> E3 POR E4','E3',3,'p_E3','pruebas.py',159),
  ('E3 -> E3 ENTRE E4','E3',3,'p_E3','pruebas.py',160),
  ('E3 -> E3 MODULO E4','E3',3,'p_E3','pruebas.py',161),
  ('E4 -> T','E4',1,'p_E4','pruebas.py',165),
  ('E4 -> NOT T','E4',2,'p_E4','pruebas.py',166),
  ('T -> PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO','T',3,'p_T','pruebas.py',170),
  ('T -> FUNCTION','T',1,'p_T','pruebas.py',171),
  ('T -> ID_COMPLETO','T',1,'p_T','pruebas.py',172),
  ('T -> VALOR_INT','T',1,'p_T','pruebas.py',173),
  ('T -> VALOR_FLOAT','T',1,'p_T','pruebas.py',174),
  ('ID_COMPLETO -> ID','ID_COMPLETO',1,'p_ID_COMPLETO','pruebas.py',178),
  ('V -> TIPO DOS_PUNTOS A PUNTO_COMA','V',4,'p_V','pruebas.py',182),
  ('V -> TIPO DOS_PUNTOS ID PUNTO_COMA','V',4,'p_V','pruebas.py',183),
  ('TIPO -> INT','TIPO',1,'p_TIPO','pruebas.py',185),
  ('TIPO -> FLOAT','TIPO',1,'p_TIPO','pruebas.py',186),
  ('V_M -> empty','V_M',1,'p_V_M','pruebas.py',188),
  ('V_M -> V V_M','V_M',2,'p_V_M','pruebas.py',189),
  ('A -> ID ASIGNACION VALOR_INT','A',3,'p_A','pruebas.py',194),
  ('A -> ID ASIGNACION VALOR_FLOAT','A',3,'p_A','pruebas.py',195),
  ('A -> ID ASIGNACION E','A',3,'p_A','pruebas.py',196),
  ('A -> ID ASIGNACION ID_COMPLETO PARENTESIS_IZQUIERDO PARENTESIS_DERECHO','A',5,'p_A','pruebas.py',197),
  ('EST -> empty','EST',1,'p_EST','pruebas.py',201),
  ('EST -> LOOP END_LOOP PUNTO_COMA EST','EST',4,'p_EST','pruebas.py',202),
  ('EST -> IF_ END_IF PUNTO_COMA EST','EST',4,'p_EST','pruebas.py',203),
  ('EST -> A PUNTO_COMA EST','EST',3,'p_EST','pruebas.py',204),
  ('EST -> PROCEDURE PUNTO_COMA EST','EST',3,'p_EST','pruebas.py',205),
  ('EST -> FUNCTION PUNTO_COMA EST','EST',3,'p_EST','pruebas.py',206),
  ('EST -> ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST','EST',5,'p_EST','pruebas.py',207),
  ('LOOP -> DO_WHILE','LOOP',1,'p_LOOP','pruebas.py',211),
  ('LOOP -> WHILE_','LOOP',1,'p_LOOP','pruebas.py',212),
  ('LOOP -> FOR_','LOOP',1,'p_LOOP','pruebas.py',213),
  ('DO_WHILE -> DO DOS_PUNTOS EST WHILE E PUNTO_COMA','DO_WHILE',6,'p_DO_WHILE','pruebas.py',215),
  ('WHILE_ -> WHILE E DOS_PUNTOS EST','WHILE_',4,'p_WHILE_','pruebas.py',217),
  ('FOR_ -> FOR EST PUNTO_COMA E PUNTO_COMA EST DOS_PUNTOS EST','FOR_',8,'p_FOR_','pruebas.py',219),
  ('IF_ -> IF E DOS_PUNTOS EST ELSIF_','IF_',5,'p_IF_','pruebas.py',223),
  ('ELSIF_ -> END IF PUNTO_COMA','ELSIF_',3,'p_ELSIF_','pruebas.py',225),
  ('ELSIF_ -> ELSE DOS_PUNTOS EST END IF PUNTO_COMA','ELSIF_',6,'p_ELSIF_','pruebas.py',226),
  ('ELSIF_ -> ELSIF DOS_PUNTOS E EST ELSIF_','ELSIF_',5,'p_ELSIF_','pruebas.py',227),
  ('P -> PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE','P',11,'p_P','pruebas.py',231),
  ('P -> PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA','P',14,'p_P','pruebas.py',232),
  ('F -> TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMA','F',15,'p_F','pruebas.py',236),
  ('MP -> PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA','MP',12,'p_MP','pruebas.py',240),
  ('MP -> PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA','MP',14,'p_MP','pruebas.py',241),
  ('PROGRAMA -> MP PROGRAMA_H','PROGRAMA',2,'p_PROGRAMA','pruebas.py',245),
  ('PROGRAMA_H -> empty','PROGRAMA_H',1,'p_PROGRAMA_H','pruebas.py',248),
  ('PROGRAMA_H -> P PROGRAMA_H','PROGRAMA_H',2,'p_PROGRAMA_H','pruebas.py',249),
  ('PROGRAMA_H -> F PROGRAMA_H','PROGRAMA_H',2,'p_PROGRAMA_H','pruebas.py',250),
]
