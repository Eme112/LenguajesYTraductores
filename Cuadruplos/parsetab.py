
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'PROGRAMAAND ASIGNACION BEGIN COMILLAS CORCHETE_DERECHO CORCHETE_IZQUIERDO DIFERENTE DO DOS_PUNTOS ELSE ELSIF END ENTRE FLOAT FOR FUNCTION ID IF IGUAL INPUT INT LOOP MAIN MAS MAYOR MAYOR_IGUAL MENOR MENOR_IGUAL MENOS MODULO NOT OR PARENTESIS_DERECHO PARENTESIS_IZQUIERDO POR PRINT PROCEDURE PUNTO PUNTO_COMA RETURN TEXTO VALOR_FLOAT VALOR_INT WHILE\n  empty :\n  E : E2\n         | E2 MENOR E2 AUX_MENOR\n         | E2 MAYOR E2 AUX_MAYOR\n         | E2 MENOR_IGUAL E2 AUX_MENOR_IGUAL\n         | E2 MAYOR_IGUAL E2 AUX_MAYOR_IGUAL\n         | E2 IGUAL E2 AUX_IGUAL\n         | E2 DIFERENTE E2 AUX_DIFERENTEAUX_MENOR : emptyAUX_MAYOR : emptyAUX_MENOR_IGUAL : emptyAUX_MAYOR_IGUAL : emptyAUX_IGUAL : emptyAUX_DIFERENTE : emptyE2 : E3\n          | E2 OR E3 AUX_OR\n          | E2 MAS E3 AUX_MAS\n          | E2 MENOS E3 AUX_MENOSAUX_OR : emptyAUX_MAS : emptyAUX_MENOS : emptyE3 : E4\n          | E3 AND E4 AUX_AND\n          | E3 POR E4 AUX_POR\n          | E3 ENTRE E4 AUX_ENTRE\n          | E3 MODULO E4 AUX_MODULOAUX_AND : emptyAUX_POR : emptyAUX_ENTRE : emptyAUX_MODULO : emptyE4 : T\n          | NOT T AUX_NOTAUX_NOT : emptyT : PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO\n         | FUNCTION\n         | ID_COMPLETO\n         | VALOR_INT\n         | VALOR_FLOATID_COMPLETO : ID RANGOSRANGOS : empty\n              | CORCHETE_IZQUIERDO VALOR_INT CORCHETE_DERECHO RANGOSV : TIPO DOS_PUNTOS A PUNTO_COMA\n         | TIPO DOS_PUNTOS ID_COMPLETO PUNTO_COMATIPO : INT\n            | FLOATV_M : empty\n           | V V_MA : ID_COMPLETO ASIGNACION VALOR_INT\n         | ID_COMPLETO ASIGNACION VALOR_FLOAT\n         | ID_COMPLETO ASIGNACION E\n         | ID_COMPLETO ASIGNACION ID_COMPLETO\n         | ID_COMPLETO ASIGNACION ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO\n         | ID_COMPLETO ASIGNACION INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHOEST : empty\n           | LOOP_ END LOOP PUNTO_COMA EST\n           | IF_ END IF PUNTO_COMA EST\n           | A PUNTO_COMA EST\n           | PROCEDURE PUNTO_COMA EST\n           | FUNCTION PUNTO_COMA EST\n           | PRINT PARENTESIS_IZQUIERDO COMILLAS COMILLAS PARENTESIS_DERECHO PUNTO_COMA EST\n           | INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST\n           | ID PARENTESIS_IZQUIERDO  PARENTESIS_DERECHO PUNTO_COMA ESTLOOP_ : DO_WHILE\n             | WHILE_\n             | FOR_DO_WHILE : DO DOS_PUNTOS EST WHILE E PUNTO_COMAWHILE_ : WHILE E DOS_PUNTOS ESTFOR_ : FOR EST E EST DOS_PUNTOS ESTIF_ : IF E DOS_PUNTOS EST ELSIF_ELSIF_ : END IF PUNTO_COMA\n              | ELSE DOS_PUNTOS EST END IF PUNTO_COMA\n              | ELSIF DOS_PUNTOS E EST ELSIF_P : PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA\n         | PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMAF : TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMAMP : PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA\n          | PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMAPROGRAMA : MP PROGRAMA_HPROGRAMA_H : empty\n                  | P PROGRAMA_H\n                  | F PROGRAMA_H '
    
_lr_action_items = {'PROCEDURE':([0,2,6,7,37,40,41,42,60,64,65,69,70,75,76,77,78,81,82,83,84,85,86,90,100,108,122,130,131,134,138,139,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,168,169,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,211,213,215,218,222,224,],[3,8,8,8,43,-1,43,43,43,-39,-40,43,104,-2,-15,-22,-31,-35,-36,-37,-38,43,43,43,135,43,-1,43,43,-1,-76,171,43,43,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,43,43,-41,-73,209,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,43,-77,43,43,-74,43,-75,]),'$end':([1,2,4,5,6,7,13,14,138,168,211,218,224,],[0,-1,-78,-79,-1,-1,-80,-81,-76,-73,-77,-74,-75,]),'INT':([2,6,7,23,24,25,28,61,62,138,168,211,218,224,],[10,10,10,10,10,10,10,-42,-43,-76,-73,-77,-74,-75,]),'FLOAT':([2,6,7,23,24,25,28,61,62,138,168,211,218,224,],[11,11,11,11,11,11,11,-42,-43,-76,-73,-77,-74,-75,]),'MAIN':([3,],[12,]),'ID':([8,9,10,11,34,37,40,41,42,45,48,59,60,63,64,65,69,75,76,77,78,79,80,81,82,83,84,85,86,90,92,102,103,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,124,125,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,162,167,172,173,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,208,213,214,215,222,223,],[15,16,-44,-45,40,53,-1,53,53,-54,40,40,53,97,-39,-40,53,-2,-15,-22,-31,40,40,-35,-36,-37,-38,53,53,53,40,137,-58,53,40,40,40,40,40,40,40,40,40,40,40,40,40,-1,-57,-59,53,53,-1,53,53,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,53,53,40,-41,-55,-56,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,53,53,40,53,53,-60,]),'DOS_PUNTOS':([10,11,20,21,22,29,40,45,58,64,65,69,74,75,76,77,78,81,82,83,84,85,86,91,103,122,124,125,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,164,167,172,173,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,215,223,],[-44,-45,23,24,25,34,-1,-54,90,-39,-40,-1,108,-2,-15,-22,-31,-35,-36,-37,-38,-1,-1,130,-58,-1,-57,-59,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-1,-1,208,-41,-55,-56,213,214,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,-1,-60,]),'PARENTESIS_IZQUIERDO':([12,15,16,45,48,51,52,53,59,60,63,69,79,80,85,86,92,97,98,103,109,110,111,112,113,114,115,116,117,118,119,120,121,124,125,140,141,160,161,162,172,173,205,206,214,215,223,],[17,18,19,-54,80,87,88,89,80,-1,80,-1,80,80,-1,-1,80,132,133,-58,80,80,80,80,80,80,80,80,80,80,80,80,80,-57,-59,-1,-1,-1,-1,80,-55,-56,-61,-62,80,-1,-60,]),'PARENTESIS_DERECHO':([17,18,19,40,64,65,75,76,77,78,81,82,83,84,88,89,122,123,132,133,134,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,167,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[20,21,22,-1,-39,-40,-2,-15,-22,-31,-35,-36,-37,-38,127,128,-1,158,165,166,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,204,-41,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'BEGIN':([23,24,25,26,27,28,30,31,33,61,62,],[-1,-1,-1,32,-46,-1,35,36,-47,-42,-43,]),'PUNTO_COMA':([32,35,36,38,39,40,43,49,50,64,65,71,75,76,77,78,81,82,83,84,93,94,95,96,97,101,104,106,107,122,127,128,134,135,137,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,165,166,167,171,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,207,209,212,219,227,],[37,41,42,61,62,-1,69,85,86,-39,-40,105,-2,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-50,-1,136,138,140,141,-1,160,161,-1,168,170,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-52,-53,-41,211,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,215,216,218,220,224,229,]),'FUNCTION':([37,40,41,42,45,48,59,60,63,64,65,69,75,76,77,78,79,80,81,82,83,84,85,86,90,92,103,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,124,125,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,162,167,172,173,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,208,210,213,214,215,222,223,],[50,-1,50,50,-54,81,81,50,81,-39,-40,50,-2,-15,-22,-31,81,81,-35,-36,-37,-38,50,50,50,81,-58,50,81,81,81,81,81,81,81,81,81,81,81,81,81,-1,-57,-59,50,50,-1,50,50,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,50,50,81,-41,-55,-56,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,50,219,50,81,50,50,-60,]),'PRINT':([37,40,41,42,60,64,65,69,75,76,77,78,81,82,83,84,85,86,90,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,213,215,222,],[51,-1,51,51,51,-39,-40,51,-2,-15,-22,-31,-35,-36,-37,-38,51,51,51,51,-1,51,51,-1,51,51,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,51,51,-41,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,51,51,51,51,]),'INPUT':([37,40,41,42,60,63,64,65,69,75,76,77,78,81,82,83,84,85,86,90,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,213,215,222,],[52,-1,52,52,52,98,-39,-40,52,-2,-15,-22,-31,-35,-36,-37,-38,52,52,52,52,-1,52,52,-1,52,52,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,52,52,-41,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,52,52,52,52,]),'END':([37,40,41,44,45,46,47,54,55,56,64,65,67,69,75,76,77,78,81,82,83,84,85,86,103,105,108,122,124,125,130,134,136,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,163,167,170,172,173,174,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,208,213,215,216,217,220,221,222,223,226,228,229,],[-1,-1,-1,70,-54,72,73,-63,-64,-65,-39,-40,100,-1,-2,-15,-22,-31,-35,-36,-37,-38,-1,-1,-58,139,-1,-1,-57,-59,-1,-1,169,-1,-1,175,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-1,-1,-67,-41,210,-55,-56,-69,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,-1,-1,-1,-66,-68,-70,225,-1,-60,175,-72,-71,]),'RETURN':([37,41,42,44,45,67,68,69,85,86,103,124,125,140,141,160,161,172,173,205,206,215,223,],[-1,-1,-1,71,-54,101,102,-1,-1,-1,-58,-57,-59,-1,-1,-1,-1,-55,-56,-61,-62,-1,-60,]),'IF':([37,40,41,42,60,64,65,69,73,75,76,77,78,81,82,83,84,85,86,90,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,175,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,213,215,222,225,],[48,-1,48,48,48,-39,-40,48,107,-2,-15,-22,-31,-35,-36,-37,-38,48,48,48,48,-1,48,48,-1,48,48,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,48,48,-41,212,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,48,48,48,48,227,]),'DO':([37,40,41,42,60,64,65,69,75,76,77,78,81,82,83,84,85,86,90,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,213,215,222,],[58,-1,58,58,58,-39,-40,58,-2,-15,-22,-31,-35,-36,-37,-38,58,58,58,58,-1,58,58,-1,58,58,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,58,58,-41,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,58,58,58,58,]),'WHILE':([37,40,41,42,45,60,64,65,69,75,76,77,78,81,82,83,84,85,86,90,103,108,122,124,125,129,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,172,173,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,208,213,215,222,223,],[59,-1,59,59,-54,59,-39,-40,59,-2,-15,-22,-31,-35,-36,-37,-38,59,59,59,-58,59,-1,-57,-59,162,59,59,-1,59,59,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,59,59,-41,-55,-56,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,59,59,59,59,-60,]),'FOR':([37,40,41,42,60,64,65,69,75,76,77,78,81,82,83,84,85,86,90,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,208,213,215,222,],[60,-1,60,60,60,-39,-40,60,-2,-15,-22,-31,-35,-36,-37,-38,60,60,60,60,-1,60,60,-1,60,60,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,60,60,-41,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,60,60,60,60,]),'ASIGNACION':([39,40,53,57,64,65,134,167,],[63,-1,-1,63,-39,-40,-1,-41,]),'CORCHETE_IZQUIERDO':([40,53,97,134,],[66,66,66,66,]),'AND':([40,64,65,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,196,197,198,199,200,201,202,203,],[-1,-39,-40,118,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,118,118,118,-1,-1,-1,-1,-32,-33,-34,-41,-23,-27,-24,-28,-25,-29,-26,-30,]),'POR':([40,64,65,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,196,197,198,199,200,201,202,203,],[-1,-39,-40,119,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,119,119,119,-1,-1,-1,-1,-32,-33,-34,-41,-23,-27,-24,-28,-25,-29,-26,-30,]),'ENTRE':([40,64,65,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,196,197,198,199,200,201,202,203,],[-1,-39,-40,120,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,120,120,120,-1,-1,-1,-1,-32,-33,-34,-41,-23,-27,-24,-28,-25,-29,-26,-30,]),'MODULO':([40,64,65,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,196,197,198,199,200,201,202,203,],[-1,-39,-40,121,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,121,121,121,-1,-1,-1,-1,-32,-33,-34,-41,-23,-27,-24,-28,-25,-29,-26,-30,]),'MENOR':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,109,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'MAYOR':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,110,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'MENOR_IGUAL':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,111,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'MAYOR_IGUAL':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,112,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'IGUAL':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,113,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'DIFERENTE':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,114,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'OR':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,115,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,115,115,115,115,115,115,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'MAS':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,116,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,116,116,116,116,116,116,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'MENOS':([40,64,65,75,76,77,78,81,82,83,84,93,94,95,97,122,134,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,167,190,191,192,193,194,195,196,197,198,199,200,201,202,203,],[-1,-39,-40,117,-15,-22,-31,-35,-36,-37,-38,-36,-37,-38,-1,-1,-1,117,117,117,117,117,117,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-41,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,]),'ELSE':([40,45,64,65,69,75,76,77,78,81,82,83,84,85,86,103,108,122,124,125,134,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,172,173,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,215,222,223,226,],[-1,-54,-39,-40,-1,-2,-15,-22,-31,-35,-36,-37,-38,-1,-1,-58,-1,-1,-57,-59,-1,-1,-1,176,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-1,-1,-41,-55,-56,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,-1,-1,-60,176,]),'ELSIF':([40,45,64,65,69,75,76,77,78,81,82,83,84,85,86,103,108,122,124,125,134,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,160,161,167,172,173,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,205,206,215,222,223,226,],[-1,-54,-39,-40,-1,-2,-15,-22,-31,-35,-36,-37,-38,-1,-1,-58,-1,-1,-57,-59,-1,-1,-1,177,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-32,-33,-34,-1,-1,-41,-55,-56,-3,-9,-4,-10,-5,-11,-6,-12,-7,-13,-8,-14,-16,-19,-17,-20,-18,-21,-23,-27,-24,-28,-25,-29,-26,-30,-61,-62,-1,-1,-60,177,]),'NOT':([45,48,59,60,63,69,80,85,86,92,103,109,110,111,112,113,114,115,116,117,118,119,120,121,124,125,140,141,160,161,162,172,173,205,206,214,215,223,],[-54,79,79,-1,79,-1,79,-1,-1,79,-58,79,79,79,79,79,79,79,79,79,79,79,79,79,-57,-59,-1,-1,-1,-1,79,-55,-56,-61,-62,79,-1,-60,]),'VALOR_INT':([45,48,59,60,63,66,69,79,80,85,86,92,103,109,110,111,112,113,114,115,116,117,118,119,120,121,124,125,140,141,160,161,162,172,173,205,206,214,215,223,],[-54,83,83,-1,94,99,-1,83,83,-1,-1,83,-58,83,83,83,83,83,83,83,83,83,83,83,83,83,-57,-59,-1,-1,-1,-1,83,-55,-56,-61,-62,83,-1,-60,]),'VALOR_FLOAT':([45,48,59,60,63,69,79,80,85,86,92,103,109,110,111,112,113,114,115,116,117,118,119,120,121,124,125,140,141,160,161,162,172,173,205,206,214,215,223,],[-54,84,84,-1,95,-1,84,84,-1,-1,84,-58,84,84,84,84,84,84,84,84,84,84,84,84,84,-57,-59,-1,-1,-1,-1,84,-55,-56,-61,-62,84,-1,-60,]),'LOOP':([72,],[106,]),'COMILLAS':([87,126,],[126,159,]),'CORCHETE_DERECHO':([99,],[134,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'PROGRAMA':([0,],[1,]),'MP':([0,],[2,]),'PROGRAMA_H':([2,6,7,],[4,13,14,]),'empty':([2,6,7,23,24,25,28,37,40,41,42,53,60,69,85,86,90,97,108,122,130,131,134,140,141,143,144,145,146,147,148,149,150,151,152,153,154,155,160,161,208,213,215,222,],[5,5,5,27,27,27,27,45,65,45,45,65,45,45,45,45,45,65,45,157,45,45,65,45,45,179,181,183,185,187,189,191,193,195,197,199,201,203,45,45,45,45,45,45,]),'P':([2,6,7,],[6,6,6,]),'F':([2,6,7,],[7,7,7,]),'TIPO':([2,6,7,23,24,25,28,],[9,9,9,29,29,29,29,]),'V_M':([23,24,25,28,],[26,30,31,33,]),'V':([23,24,25,28,],[28,28,28,28,]),'A':([34,37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[38,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,]),'ID_COMPLETO':([34,37,41,42,48,59,60,63,69,79,80,85,86,90,92,108,109,110,111,112,113,114,115,116,117,118,119,120,121,130,131,140,141,160,161,162,208,213,214,215,222,],[39,57,57,57,82,82,57,93,57,82,82,57,57,57,82,57,82,82,82,82,82,82,82,82,82,82,82,82,82,57,57,57,57,57,57,82,57,57,82,57,57,]),'EST':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[44,67,68,92,103,124,125,129,142,163,164,172,173,205,206,217,221,223,226,]),'LOOP_':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,]),'IF_':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,]),'DO_WHILE':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,]),'WHILE_':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,]),'FOR_':([37,41,42,60,69,85,86,90,108,130,131,140,141,160,161,208,213,215,222,],[56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,]),'RANGOS':([40,53,97,134,],[64,64,64,167,]),'E':([48,59,63,80,92,162,214,],[74,91,96,123,131,207,222,]),'E2':([48,59,63,80,92,109,110,111,112,113,114,162,214,],[75,75,75,75,75,143,144,145,146,147,148,75,75,]),'E3':([48,59,63,80,92,109,110,111,112,113,114,115,116,117,162,214,],[76,76,76,76,76,76,76,76,76,76,76,149,150,151,76,76,]),'E4':([48,59,63,80,92,109,110,111,112,113,114,115,116,117,118,119,120,121,162,214,],[77,77,77,77,77,77,77,77,77,77,77,77,77,77,152,153,154,155,77,77,]),'T':([48,59,63,79,80,92,109,110,111,112,113,114,115,116,117,118,119,120,121,162,214,],[78,78,78,122,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,]),'AUX_NOT':([122,],[156,]),'ELSIF_':([142,226,],[174,228,]),'AUX_MENOR':([143,],[178,]),'AUX_MAYOR':([144,],[180,]),'AUX_MENOR_IGUAL':([145,],[182,]),'AUX_MAYOR_IGUAL':([146,],[184,]),'AUX_IGUAL':([147,],[186,]),'AUX_DIFERENTE':([148,],[188,]),'AUX_OR':([149,],[190,]),'AUX_MAS':([150,],[192,]),'AUX_MENOS':([151,],[194,]),'AUX_AND':([152,],[196,]),'AUX_POR':([153,],[198,]),'AUX_ENTRE':([154,],[200,]),'AUX_MODULO':([155,],[202,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> PROGRAMA","S'",1,None,None,None),
  ('empty -> <empty>','empty',0,'p_empty','cuadruplos.py',171),
  ('E -> E2','E',1,'p_E','cuadruplos.py',177),
  ('E -> E2 MENOR E2 AUX_MENOR','E',4,'p_E','cuadruplos.py',178),
  ('E -> E2 MAYOR E2 AUX_MAYOR','E',4,'p_E','cuadruplos.py',179),
  ('E -> E2 MENOR_IGUAL E2 AUX_MENOR_IGUAL','E',4,'p_E','cuadruplos.py',180),
  ('E -> E2 MAYOR_IGUAL E2 AUX_MAYOR_IGUAL','E',4,'p_E','cuadruplos.py',181),
  ('E -> E2 IGUAL E2 AUX_IGUAL','E',4,'p_E','cuadruplos.py',182),
  ('E -> E2 DIFERENTE E2 AUX_DIFERENTE','E',4,'p_E','cuadruplos.py',183),
  ('AUX_MENOR -> empty','AUX_MENOR',1,'p_AUX_MENOR','cuadruplos.py',185),
  ('AUX_MAYOR -> empty','AUX_MAYOR',1,'p_AUX_MAYOR','cuadruplos.py',188),
  ('AUX_MENOR_IGUAL -> empty','AUX_MENOR_IGUAL',1,'p_AUX_MENOR_IGUAL','cuadruplos.py',191),
  ('AUX_MAYOR_IGUAL -> empty','AUX_MAYOR_IGUAL',1,'p_AUX_MAYOR_IGUAL','cuadruplos.py',194),
  ('AUX_IGUAL -> empty','AUX_IGUAL',1,'p_AUX_IGUAL','cuadruplos.py',197),
  ('AUX_DIFERENTE -> empty','AUX_DIFERENTE',1,'p_AUX_DIFERENTE','cuadruplos.py',200),
  ('E2 -> E3','E2',1,'p_E2','cuadruplos.py',205),
  ('E2 -> E2 OR E3 AUX_OR','E2',4,'p_E2','cuadruplos.py',206),
  ('E2 -> E2 MAS E3 AUX_MAS','E2',4,'p_E2','cuadruplos.py',207),
  ('E2 -> E2 MENOS E3 AUX_MENOS','E2',4,'p_E2','cuadruplos.py',208),
  ('AUX_OR -> empty','AUX_OR',1,'p_AUX_OR','cuadruplos.py',210),
  ('AUX_MAS -> empty','AUX_MAS',1,'p_AUX_MAS','cuadruplos.py',213),
  ('AUX_MENOS -> empty','AUX_MENOS',1,'p_AUX_MENOS','cuadruplos.py',216),
  ('E3 -> E4','E3',1,'p_E3','cuadruplos.py',221),
  ('E3 -> E3 AND E4 AUX_AND','E3',4,'p_E3','cuadruplos.py',222),
  ('E3 -> E3 POR E4 AUX_POR','E3',4,'p_E3','cuadruplos.py',223),
  ('E3 -> E3 ENTRE E4 AUX_ENTRE','E3',4,'p_E3','cuadruplos.py',224),
  ('E3 -> E3 MODULO E4 AUX_MODULO','E3',4,'p_E3','cuadruplos.py',225),
  ('AUX_AND -> empty','AUX_AND',1,'p_AUX_AND','cuadruplos.py',227),
  ('AUX_POR -> empty','AUX_POR',1,'p_AUX_POR','cuadruplos.py',230),
  ('AUX_ENTRE -> empty','AUX_ENTRE',1,'p_AUX_ENTRE','cuadruplos.py',233),
  ('AUX_MODULO -> empty','AUX_MODULO',1,'p_AUX_MODULO','cuadruplos.py',236),
  ('E4 -> T','E4',1,'p_E4','cuadruplos.py',241),
  ('E4 -> NOT T AUX_NOT','E4',3,'p_E4','cuadruplos.py',242),
  ('AUX_NOT -> empty','AUX_NOT',1,'p_AUX_NOT','cuadruplos.py',244),
  ('T -> PARENTESIS_IZQUIERDO E PARENTESIS_DERECHO','T',3,'p_T','cuadruplos.py',249),
  ('T -> FUNCTION','T',1,'p_T','cuadruplos.py',250),
  ('T -> ID_COMPLETO','T',1,'p_T','cuadruplos.py',251),
  ('T -> VALOR_INT','T',1,'p_T','cuadruplos.py',252),
  ('T -> VALOR_FLOAT','T',1,'p_T','cuadruplos.py',253),
  ('ID_COMPLETO -> ID RANGOS','ID_COMPLETO',2,'p_ID_COMPLETO','cuadruplos.py',269),
  ('RANGOS -> empty','RANGOS',1,'p_RANGOS','cuadruplos.py',273),
  ('RANGOS -> CORCHETE_IZQUIERDO VALOR_INT CORCHETE_DERECHO RANGOS','RANGOS',4,'p_RANGOS','cuadruplos.py',274),
  ('V -> TIPO DOS_PUNTOS A PUNTO_COMA','V',4,'p_V','cuadruplos.py',278),
  ('V -> TIPO DOS_PUNTOS ID_COMPLETO PUNTO_COMA','V',4,'p_V','cuadruplos.py',279),
  ('TIPO -> INT','TIPO',1,'p_TIPO','cuadruplos.py',283),
  ('TIPO -> FLOAT','TIPO',1,'p_TIPO','cuadruplos.py',284),
  ('V_M -> empty','V_M',1,'p_V_M','cuadruplos.py',287),
  ('V_M -> V V_M','V_M',2,'p_V_M','cuadruplos.py',288),
  ('A -> ID_COMPLETO ASIGNACION VALOR_INT','A',3,'p_A','cuadruplos.py',292),
  ('A -> ID_COMPLETO ASIGNACION VALOR_FLOAT','A',3,'p_A','cuadruplos.py',293),
  ('A -> ID_COMPLETO ASIGNACION E','A',3,'p_A','cuadruplos.py',294),
  ('A -> ID_COMPLETO ASIGNACION ID_COMPLETO','A',3,'p_A','cuadruplos.py',295),
  ('A -> ID_COMPLETO ASIGNACION ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO','A',5,'p_A','cuadruplos.py',296),
  ('A -> ID_COMPLETO ASIGNACION INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO','A',5,'p_A','cuadruplos.py',297),
  ('EST -> empty','EST',1,'p_EST','cuadruplos.py',321),
  ('EST -> LOOP_ END LOOP PUNTO_COMA EST','EST',5,'p_EST','cuadruplos.py',322),
  ('EST -> IF_ END IF PUNTO_COMA EST','EST',5,'p_EST','cuadruplos.py',323),
  ('EST -> A PUNTO_COMA EST','EST',3,'p_EST','cuadruplos.py',324),
  ('EST -> PROCEDURE PUNTO_COMA EST','EST',3,'p_EST','cuadruplos.py',325),
  ('EST -> FUNCTION PUNTO_COMA EST','EST',3,'p_EST','cuadruplos.py',326),
  ('EST -> PRINT PARENTESIS_IZQUIERDO COMILLAS COMILLAS PARENTESIS_DERECHO PUNTO_COMA EST','EST',7,'p_EST','cuadruplos.py',327),
  ('EST -> INPUT PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST','EST',5,'p_EST','cuadruplos.py',328),
  ('EST -> ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO PUNTO_COMA EST','EST',5,'p_EST','cuadruplos.py',329),
  ('LOOP_ -> DO_WHILE','LOOP_',1,'p_LOOP','cuadruplos.py',338),
  ('LOOP_ -> WHILE_','LOOP_',1,'p_LOOP','cuadruplos.py',339),
  ('LOOP_ -> FOR_','LOOP_',1,'p_LOOP','cuadruplos.py',340),
  ('DO_WHILE -> DO DOS_PUNTOS EST WHILE E PUNTO_COMA','DO_WHILE',6,'p_DO_WHILE','cuadruplos.py',342),
  ('WHILE_ -> WHILE E DOS_PUNTOS EST','WHILE_',4,'p_WHILE_','cuadruplos.py',344),
  ('FOR_ -> FOR EST E EST DOS_PUNTOS EST','FOR_',6,'p_FOR_','cuadruplos.py',346),
  ('IF_ -> IF E DOS_PUNTOS EST ELSIF_','IF_',5,'p_IF_','cuadruplos.py',350),
  ('ELSIF_ -> END IF PUNTO_COMA','ELSIF_',3,'p_ELSIF_','cuadruplos.py',352),
  ('ELSIF_ -> ELSE DOS_PUNTOS EST END IF PUNTO_COMA','ELSIF_',6,'p_ELSIF_','cuadruplos.py',353),
  ('ELSIF_ -> ELSIF DOS_PUNTOS E EST ELSIF_','ELSIF_',5,'p_ELSIF_','cuadruplos.py',354),
  ('P -> PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA','P',12,'p_P','cuadruplos.py',358),
  ('P -> PROCEDURE ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA','P',14,'p_P','cuadruplos.py',359),
  ('F -> TIPO ID PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN ID PUNTO_COMA END FUNCTION PUNTO_COMA','F',15,'p_F','cuadruplos.py',368),
  ('MP -> PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST END PROCEDURE PUNTO_COMA','MP',12,'p_MP','cuadruplos.py',377),
  ('MP -> PROCEDURE MAIN PARENTESIS_IZQUIERDO PARENTESIS_DERECHO DOS_PUNTOS V_M BEGIN PUNTO_COMA EST RETURN PUNTO_COMA END PROCEDURE PUNTO_COMA','MP',14,'p_MP','cuadruplos.py',378),
  ('PROGRAMA -> MP PROGRAMA_H','PROGRAMA',2,'p_PROGRAMA','cuadruplos.py',384),
  ('PROGRAMA_H -> empty','PROGRAMA_H',1,'p_PROGRAMA_H','cuadruplos.py',387),
  ('PROGRAMA_H -> P PROGRAMA_H','PROGRAMA_H',2,'p_PROGRAMA_H','cuadruplos.py',388),
  ('PROGRAMA_H -> F PROGRAMA_H','PROGRAMA_H',2,'p_PROGRAMA_H','cuadruplos.py',389),
]
